#' Insert the scripted workflow description common to most projects
#'
#' Describes R/RStudio, git, packages, targets, and reporting. Also introduces
#' `envBlah` packages, pulling from packages_bib_file
#'
#' @param packages Character. Vector of any loaded packages (doesn't have to
#' be limited to just `envBlah` packages).
#'
#' @returns Markdown child document.
#' @export
#'
knit_scripting <- function(packages = sort(unique(unname(unlist(yaml::read_yaml(here::here("settings", "packages.yaml"))))))) {
  
  knitr::knit_child(system.file("rmd/scripting.Rmd"
                                , package = "envReport"
                                )
                    , quiet = TRUE
                    ) |>
    cat()

}
