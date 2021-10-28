#' Make package bibliography, including tweaks for known package issues.
#'
#' Fixes sentence case applied to package names via `knitr::write_bib` process.
#' Makes heavy use of `bib2df` package.
#'
#' @param bib_file Path to bibliography. If not existing, it will be made.
#' @param make_key Logical. Generate a new key via author and year? Defaults to
#' `FALSE`.
#' @param is_package_bib Logical. Is this a bibliography of package citations?
#' Defaults to `TRUE.`
#'
#' @return Tweaked `bib_file` and data frame of references (from
#' `bib2df::bib2df`).
#' @export
#'
#' @examples
fix_bib <- function(bib_file = "packages.bib"
                    , make_key = FALSE
                    , is_package_bib = TRUE
                    ) {

  if(!file.exists(bib_file)) {

    pac <- .packages()

    knitr::write_bib(pac
                     , bib_file
                     )

  }

  in_refs <- bib2df::bib2df(bib_file)

  names_in_refs <- colnames(in_refs) %>%
    grep("\\.\\d+$",.,value = TRUE,invert = TRUE) %>%
    c(.,"COPYRIGHT")

  refs <- in_refs %>%
    {if(is_package_bib) (.) %>% dplyr::mutate(package = gsub("R-|\\d{4}","",BIBTEXKEY)) else (.)} %>%
    tidytext::unnest_tokens("titleWords"
                            , TITLE
                            , token = "regex"
                            , pattern = " "
                            , to_lower = FALSE
                            #, strip_punct = FALSE
                            , collapse = NULL
                            ) %>%
    dplyr::mutate(titleWords = gsub("\\{|\\}","",titleWords)
                  , isCap = grepl(paste0(LETTERS,collapse="|"),titleWords)
                  , titleWords = dplyr::if_else(isCap,paste0("{",titleWords,"}"),titleWords)
                  ) %>%
    tidyr::nest(data = c(titleWords,isCap)) %>%
    dplyr::mutate(TITLE = purrr::map_chr(data,. %>% dplyr::pull(titleWords) %>% paste0(collapse = " "))
                  , AUTHOR = purrr::map(AUTHOR,~gsub("Microsoft Corporation","{Microsoft Corporation}",.))
                  , AUTHOR = purrr::map(AUTHOR,~gsub("Fortran original by |R port by ","",.))
                  , AUTHOR = purrr::map(AUTHOR, ~gsub("with contributions by","and",.))
                  , AUTHOR = purrr::map(AUTHOR, ~gsub("Â "," ",.))
                  , YEAR = substr(YEAR,1,4)
                  ) %>%
    {if(make_key) (.) %>%
        dplyr::mutate(BIBTEXKEY = purrr::map2_chr(AUTHOR
                                           ,YEAR
                                           ,~paste0(toupper(gsub("[[:punct:]]|\\s","",.x[[1]]))
                                                    , .y
                                                    )
                                           )
                      ) else (.)} %>%
    {if(is_package_bib) (.) %>% dplyr::mutate(TITLE = purrr::map2_chr(package,TITLE,~gsub(.x,paste0("{",.x,"}"),.y))) else (.)} %>%
    dplyr::select(tidyselect::any_of(names_in_refs)) %>%
    dplyr::filter(!grepl("MEDIA SCREEN AND",CATEGORY))

  bib2df::df2bib(refs
                 , bib_file
                 )

  return(refs)

}
