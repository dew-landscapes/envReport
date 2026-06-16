#' Insert the method descriptions common to most projects
#'
#' Describes `extent` and `grain`; `envCleaned` cleaning steps; optionally
#' project-specific cleaning steps; and, optionally, environmental variables.
#'
#' @param include_specific Logical. Include project-specific cleaning methods?
#'
#' @returns Markdown child document.
#' @export
#'
knit_clean <- function(include_specific = TRUE) {

  text <- knitr::knit_child(system.file("rmd/generic_clean.Rmd"
                                        , package = "envReport"
                                        )
                            , quiet = TRUE
                            )

  if(include_specific) {

    text <- c(text
              , knitr::knit_child(system.file("rmd/specific_clean.Rmd"
                                              , package = "envReport"
                                              )
                                  , quiet = TRUE
                                  )
              )

  }

  cat(paste(text, collapse = '\n'))

}
