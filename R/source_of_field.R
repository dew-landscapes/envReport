
#' Identify original source (field, data source) of a column in a data map
#'
#' Create a phrase, for use as text, identifying the original field and data
#' source for any column in a data map.
#'
#' @param data_map Dataframe mapping original field to unified fields
#' @param this_col Which field (column) should be summarised here.
#' @param first_data_col First column in data_map containing data_names.
#'
#' @return text string
#' @export
#'
#' @examples
source_of_field <- function(data_map, this_col, first_data_col = 2) {

  data_map %>%
    dplyr::filter(col == this_col) %>%
    tidyr::pivot_longer(first_data_col:ncol(.)) |>
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

