#' Insert generic text
#'
#' e.g. from generic_env_methods.Rmd; generic_occurrence.Rmd
#'
#' @param section Character. Name of generic file to knit, excluding
#'  `generic_`. e.g. the default "scale" will knit `generic_scale.Rmd`
#'
#' @returns Markdown child document
#' @export
#'
knit_generic <- function(section = "scale") {

  file_to_knit <- paste0("rmd/generic_", section, ".Rmd")

  text <- knitr::knit_child(system.file(file_to_knit
                                        , package = "envReport"
                                        )
                            , quiet = TRUE
                            )

  cat(paste(text, collapse = '\n'))

}
