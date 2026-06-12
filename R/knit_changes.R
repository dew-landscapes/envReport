#' Create markdown text regarding changes to a workflow
#'
#' Useful in the context of the workflow undergoing constant improvement.
#'
#' @param project Character. Rstudio 'project'
#' @param include_packages Character. Any packages to include in the summary
#' text
#' @param add_dots Character. Usually `"."` for interactive use or `".."` inside
#' a knit.
#' @param use_units Character. Passed to the `unit` argument of
#' `lubridate::time_length()`
#' @param suffix_text Character. Text to add on to the end of the workflow
#' summary information
#'
#' @returns Text formatted for inclusion in rmarkdown.
#' @export
#'
#' @examples
knit_changes <- function(project = basename(here::here())
                         , include_packages = NULL
                         , add_dots = "."
                         , use_units = "years"
                         , suffix_text = "The workflow and associated package(s) are undergoing constant improvement."
                         ) {

  files <- system2("git", args = c("ls-files"), stdout = TRUE) |>
    length()

  lines <- system2("git", args = c("ls-files"), stdout = TRUE) |>
    tibble::enframe(name = NULL, value = "path") |>
    dplyr::mutate(path = fs::path(add_dots, path)
                  , lines = purrr::map_int(path, \(x) length(readLines(x, warn = FALSE)))
                  ) |>
    dplyr::pull(lines) |>
    sum(na.rm = TRUE)

  out_text <- paste0(
    "The customisable workflow for `"
    , project
    , "` allows for changes to be made over time. For example "
    , nrow(gert::git_log(max = 1e6))
    , " changes have been made to the `"
    , project
    , "` workflow within the last "
    , round(lubridate::time_length(difftime(Sys.time(), min(gert::git_log(max = 1e6)$time)), unit = use_units), 0)
    , " years. Today the project has "
    , files
    , " files and "
    , format(lines, big.mark = ",")
    , " lines of code."
    )

  if(!is.null(include_packages)) {

    pac_text <- tibble::tibble(pac = include_packages) |>
      dplyr::mutate(log = purrr::map(pac
                                     , \(x) gert::git_log(max = 1e6
                                                          , repo = fs::path(add_dots, "..", "packages", x)
                                                          )
                                     )
                    , changes = purrr::map_int(log, nrow)
                    , timeframe = purrr::map_dbl(log
                                                 , \(x) round(lubridate::time_length(difftime(Sys.time(), min(x$time)), unit = use_units), 0)
                                                 )
                    , text = paste0("`"
                                    , pac
                                    , "`"
                                    , " had "
                                    , changes
                                    , " changes over the last "
                                    , timeframe
                                    , " "
                                    , use_units
                                    )
                    )

    pac_text <- paste0(
      "The accompanying package"
      , if(length(include_packages) > 1) "s: " else " "
      , envFunc::vec_to_sentence(pac_text$text, sep  = ";")
      , "."
    )

    out_text <- paste(out_text, pac_text, suffix_text, collapse = " ")

  }

  cat(out_text)

}
