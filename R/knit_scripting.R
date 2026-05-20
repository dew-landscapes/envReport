#' Insert the scripted workflow description common to most projects
#'
#' Describes R/RStudio, git, packages, targets, and reporting. Also introduces `envBlah` packages, pulling from packages_bib_file
#'
#' @param pkg_bib_file *Absolute* file path to packages.bib (e.g. in report_prep store). Usually needs additional dots .. when run inside a report.
#'
#' @returns Markdown child document.
#' @export
#'
knit_scripting <- function(pkg_bib_file) {

  if(!fs::is_absolute_path(pkg_bib_file)) stop("Ensure pkg_bib_file is an absolute path (and remember to add additional dots .. where necessary)")

  if(!file.exists(pkg_bib_file)) stop("pkg_bib_file not found at ", pkg_bib_file)

  env_packages_txt <- bib2df::bib2df(pkg_bib_file) |>
    dplyr::filter(grepl("^R-env", BIBTEXKEY)) |>
    dplyr::select(BIBTEXKEY,TITLE) |>
    dplyr::rowwise() |>
    dplyr::mutate(usetitle = paste0(
      "*",
      gsub("^R-", "", BIBTEXKEY),
      "*",
      gsub("^env.*:", ":", TITLE)
      )) |>
    dplyr::pull(usetitle) |>
    paste0(collapse = "\n-   ")

  knitr::knit_child(system.file("rmd/scripting.Rmd", package = "envReport"),
                    envir = environment(),
                    quiet = TRUE) |>
    cat()

}
