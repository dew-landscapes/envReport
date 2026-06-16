#' Create markdown text regarding changes to a workflow
#'
#' Useful in the context of the workflow undergoing constant improvement.
#'
#' @param project Character. Rstudio 'project'
#' @param include_packages Character. Any packages to include in the summary
#' text. Assumes the user has any `include_packages` at
#' `fs::path_abs("..", start = here::here()) |> fs::path("packages", x)`
#' @param use_units Character. Passed to the `unit` argument of
#' `lubridate::time_length()`
#' @param suffix_text Character. Text to add on to the end of the workflow
#' summary information
#'
#' @returns Text formatted for inclusion in rmarkdown.
#' @export
#'
#' @examples
knit_changes <- function(project = here::here()
                         , include_packages = NULL
                         , use_units = "years"
                         , suffix_text = "The workflow and associated package(s) are undergoing constant improvement."
                         ) {

  files <- fs::dir_ls(project
                      , regexp = "\\.R$|\\.r$|\\.Rmd$|\\.rmd$|\\.yaml$|\\.txt$|\\.csv$"
                      , recurse = TRUE
                      )

  n_files <- length(files)

  n_lines <- files |>
    purrr::map_int(\(x) length(readLines(fs::path(x), warn = FALSE))) |>
    sum(na.rm = TRUE)

  out_text <- paste0(
    "The customisable workflow for `"
    , basename(project)
    , "` allows for changes to be made over time. For example "
    , nrow(gert::git_log(max = 1e6))
    , " changes have been made to the `"
    , basename(project)
    , "` workflow within the last "
    , round(lubridate::time_length(difftime(Sys.time(), min(gert::git_log(max = 1e6)$time)), unit = use_units), 0)
    , " years. Today the project has "
    , n_files
    , " files and "
    , format(n_lines, big.mark = ",")
    , " lines of code."
    )

  if(!is.null(include_packages)) {

    pac_text <- tibble::tibble(pac = include_packages) |>
      dplyr::mutate(log = purrr::map(pac
                                     , \(x) gert::git_log(max = 1e6
                                                          , repo = fs::path_abs("..", start = here::here()) |>
                                                            fs::path("packages", x)
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
      "Within accompanying package"
      , if(length(include_packages) > 1) "s: " else " "
      , envFunc::vec_to_sentence(pac_text$text, sep  = ";")
      , "."
    )

    out_text <- paste(out_text, pac_text, suffix_text, collapse = " ")

  }

  cat(out_text)

}
