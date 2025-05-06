
#' Identify original source (field, data source) of a column in a data map
#'
#' Create a phrase, for use as text, identifying the original field and data
#' source for any column in a data map.
#'
#' @param data_map Dataframe mapping original field to unified fields
#' @param this_col Which field (column) should be summarised here.
#'
#' @return text string
#' @export
#'
#' @examples
source_of_field <- function(data_map, this_col) {

  data_map %>%
    dplyr::filter(col == this_col) %>%
    tidyr::pivot_longer(4:ncol(.)) |>
    na.omit() |>
    dplyr::mutate(text = paste0("`"
                                , name
                                , "`: `"
                                , value
                                , "`"
                                )
                  ) %>%
    dplyr::pull(text) %>%
    envFunc::vec_to_sentence()

}

