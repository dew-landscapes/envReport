#' Make package bibliography, including tweaks for known package issues.
#'
#' Makes use of both [knitr::write_bib()] \insertCite{`r gsub("@","",envReport::cite_package("knitr", brack = FALSE, sep = ",", bib_file = "inst/REFERENCES.bib"))`}{envReport}
#' and [bib2df::bib2df()] \insertCite{`r gsub("@","",envReport::cite_package("bib2df", brack = FALSE, sep = ",", bib_file = "inst/REFERENCES.bib"))`}{envReport}.
#'
#' @param bib_file Path to bibliography. Will be created if it doesn't exist.
#' @param make_key Logical. Generate a new key using author and year.
#' @param is_package_bib Logical. Is this a bibliography of package citations?
#' Defaults to `TRUE.`
#' @param pacs Character. Name of any R packages to add to `bib_file`.
#'
#' @return Tweaked `bib_file` and data frame of references (from
#' `bib2df::bib2df`).
#' @references
#'   \insertAllCited{}
#' @export
#'
#' @examples
#' \dontrun{
#' library(envReport)
#' bib_file <- tempfile()
#' make_bib(pacs = .packages())
#' refs
#' rm(refs)
#' unlink(bib_file)
#' }
make_bib <- function(bib_file = "packages.bib"
                    , make_key = FALSE
                    , is_package_bib = TRUE
                    , pacs = NULL
                    ) {

  if(!file.exists(bib_file)) {

    knitr::write_bib(c(pacs
                       , .packages()
                       )
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
                  , AUTHOR = gsub("Microsoft Corporation"
                                  , "{Microsoft Corporation}"
                                  , AUTHOR
                                  )
                  , AUTHOR = gsub("Fortran original by |R port by "
                                  , ""
                                  , AUTHOR
                                  )
                  , AUTHOR = gsub("with contributions by"
                                  , "and"
                                  , AUTHOR
                                  )
                  , AUTHOR = gsub("Â "
                                  , " "
                                  , AUTHOR
                                  )
                  , AUTHOR = gsub("\\{|\\}"
                                  , ""
                                  , AUTHOR
                                  )
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
