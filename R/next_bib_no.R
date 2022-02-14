

#' Get the next bibliography number to use
#'
#' Assumes a bibliography with sequential, integer reference IDs.
#'
#' @param bib_path Character. Path to bibliography.
#'
#' @return Integer. Number to use as the next ID in bibliography `bib_path`.
#' @export
#'
#' @examples
  next_bib_no <- function(bib_path = "common/refs.bib") {

    as_tibble(read_lines(bib_path)) %>%
      dplyr::filter(grepl("^@",value)) %>%
      dplyr::mutate(value = readr::parse_number(value)) %>%
      dplyr::pull(value) %>%
      max() %>%
      `+` (1)

  }
