
#' Identify original source (field, data source) of a column in a data map
#'
#' Create a phrase, for use as text, identifying the original field and data
#' source for any column in a data map.
#'
#' @param data_map Dataframe mapping original field to unified fields
#' @param col Which field (column) should be summarised here.
#'
#' @return text string
#' @export
#'
#' @examples
source_of_field <- function(data_map, col) {

  data_map %>%
    dplyr::filter(!is.na(!!ensym(col))) %>%
    dplyr::select(data_name, !!ensym(col)) %>%
    dplyr::mutate(text = paste0("`"
                                , !!ensym(col)
                                , "`: `"
                                , data_name
                                , "`"
                                )
                  ) %>%
    dplyr::pull(text) %>%
    envFunc::vec_to_sentence()

}

