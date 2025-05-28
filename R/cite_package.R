
#' Cite a package
#'
#' Default output is rmarkdown format.
#'
#' `bib_df` and `bib_file` will be created if they don't currently exist.
#'
#' @param package Character. Name of package.
#' @param brack Logical. If true, include brackets - '('ref')' - in output.
#' @param startText Character. Text to include before reference.
#' @param endText Character. Text to include after reference.
#' @param bib_df Dataframe of package citations, usually read in via
#' `bib2df::bib2df()`
#' @param bib_file Path to bibliography. Will be created if it doesn't exist.
#' `knitr::write_bib()`.
#' @param sep Character. What character to use to separate more than one
#' reference. Defaults to ';' which is needed for rmarkdown.
#'
#' @return Character string with appropriate markdown formatting to insert the
#' appropriate reference at that point in the text. If `bib_df` did not exist
#' when the function is called, `bib_df` is created in the global environment
#' and if `bib_file` did not exist it is created.
#' @export
#'
#' @examples
#' \dontrun{
#' library(envReport)
#' bib_file = tempfile()
#' cite_package("dplyr", bib_file = bib_file)
#' refs
#' rm(refs)
#' unlink(bib_file)
#' }
cite_package <- function(package
                         , brack = TRUE
                         , startText = ""
                         , endText = ""
                         , bib_df = "refs"
                         , bib_file = here::here("report/packages.bib")
                         , sep = ";"
                         ) {

  if(!file.exists(bib_file)) {

    pacs <- c(package
             , .packages()
             )

    fs::dir_create(dirname(bib_file))

    knitr::write_bib(sort(unique(pacs))
                     , bib_file
                     )

  }

  if(!exists(bib_df)) {

    refs <- bib2df::bib2df(bib_file)

    assign("refs"
           , refs
           )

    }

  check_refs <- refs %>%
    dplyr::filter(grepl(paste0("-"
                               , package
                               , collapse = "|"
                               )
                        , BIBTEXKEY
                        )
                  ) %>%
    dplyr::pull(BIBTEXKEY)

  if(length(check_refs) != length(package)) {

    already_done <- refs %>%
      dplyr::filter(grepl("R-", BIBTEXKEY)) %>%
      dplyr::pull(BIBTEXKEY) %>%
      gsub("R-", "", .)

    pacs <- c(package, already_done) %>%
      unique() %>%
      sort()

    fs::dir_create(dirname(bib_file))

    knitr::write_bib(pacs
                     , bib_file
                     )

    refs <- bib2df::bib2df(bib_file)

    assign("refs"
           , refs
           )

  }

  thisRef <- refs %>%
      dplyr::filter(grepl(paste0("-",package
                                 , collapse = "|"
                                 )
                          , BIBTEXKEY
                          ) |
                      grepl(paste0("^"
                                   , package
                                   , collapse = "|"
                                   )
                            , BIBTEXKEY
                            )
                    ) %>%
      dplyr::pull(BIBTEXKEY)

  starts <- if(brack) paste0("[",startText) else startText
  ends <- if(brack) paste0(endText,"]") else endText

  paste0(starts, paste0("@", thisRef, collapse = sep), ends)

}
