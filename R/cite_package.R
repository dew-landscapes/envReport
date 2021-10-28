
#' Cite a package in rmarkdown
#'
#' Requires that `package_citations.bib` exists in the project root directory
#' and that the data frame `refs` exists
#' (where `refs <- bib2df::bib2df(package_citations.bib)`). These will be
#' created if `refs` does not exist when the function is called.
#'
#' @param package Character. Name of package.
#' @param brack Logical. If true, include brackets - '('ref')' - in output.
#' @param startText Character. Text to include before reference.
#' @param endText Character. Text to include after reference.
#' @param pac_df Dataframe of package citations, usually read in via
#' `bib2df::bib2df()`
#' @param pac_cite_file Name of file in the project root directory containing
#' package citations. Usually called `package_citations.bib` and created with
#' `knitr::write_bib()`.
#'
#' @return Character string with appropriate markdown formatting to insert the
#' appropriate reference at that point in the text. If `refs` did not exist when
#' the function is called, `refs` is created in the global environment and
#' if `package_citations.bib` did not exist it is created in the project root
#' directory.
#' @export
#'
#' @examples
#' library("dplyr")
#' cite_package("dplyr")
cite_package <- function(package
                         , brack = TRUE
                         , startText = ""
                         , endText = ""
                         , pac_df = "refs"
                         , pac_cite_file = "packages.bib"
                         ) {

  if(!file.exists(pac_cite_file)) {

    pac <- .packages()

    knitr::write_bib(pac,pac_cite_file)

  }

  if(!exists(pac_df, envir = globalenv())) {

    refs <- bib2df::bib2df(pac_cite_file)

    assign("refs",refs,envir = globalenv())

    }

  thisRef <- refs %>%
    dplyr::filter(grepl(paste0("-",package),BIBTEXKEY) | grepl(paste0("^",package),BIBTEXKEY)) %>%
    dplyr::pull(BIBTEXKEY)

  starts <- if(brack) paste0("[",startText,"@") else paste0(startText,"@")
  ends <- if(brack) paste0(endText,"]") else endText

  if(length(thisRef) > 1) {

    paste0(starts,paste0(thisRef,collapse = "; @"),ends)

  } else {

    paste0(starts,"R-",package,ends)

  }

}
