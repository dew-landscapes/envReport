#' Standardise and format report tables
#'
#' Includes `knitr::kable()`; `kableExtra::kable_sytling()`;
#' and `kableExtra::scroll_box()`
#'
#' @param data dataframe
#' @param font_size passed to the `font_size` argument of
#' `kableExtra::kable_sytling()`
#' @param width,height passed to the matching argument of
#' `kableExtra::scroll_box()`
#' @param ... passed to `knitr::kable()`
#'
#' @returns data formatted appropriately for rmarkdown
#' @export
#'
#' @examples
env_kable_extra <- function(data
                            , font_size = 12
                            , width = "100%"
                            , height = NULL
                            , ...
                            ) {

  knitr::kable(data
               , format = "html"
               , booktabs = TRUE
               , ...
               ) |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "bordered", "condensed") # For HTML
                              , full_width = FALSE
                              , position = "center"
                              , fixed_thead = TRUE
                              , font_size = font_size
                              ) |>
    kableExtra::scroll_box(width = width
                           , height = height
                           )

}
