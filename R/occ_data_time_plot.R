
#' Produce a histogram of points over time from a data frame of occurrences
#'
#' @param df Dataframe with year or date column.
#' @param year_col Character. Name of column with date stamp. Can be numeric year, a date class eg posix, or character
#' @param date_format Character. Format of year_col, if a character, to be parsed by as.Date. E.g. %d-%m-%Y
#' @param minyear Integer. Records before this year will be lumped into one bar labelled 'pre-minyear'. Useful for datasets with very long temporal tails eg gbif
#'
#' @return ggplot2 object
#' @export

occ_data_time_plot <- function(df,
                               year_col = "year",
                               date_format,
                               minyear = NULL
) {

  #parse date
  if(class(df[[year_col]]) %in% c("integer", "numeric")) {

    df <- df %>%
      dplyr::mutate(use_year = !!rlang::ensym(year_col))

  } else if(class(df[[year_col]]) %in% "Date") {

    df <- df %>%
      dplyr::mutate(use_year = lubridate::year(!!rlang::ensym(year_col)))

  } else if(class(df[[year_col]]) %in% "character") {

    df <- df %>%
      dplyr::mutate(use_year = lubridate::year(as.Date(df$date_char,
                                                format = date_format)))
  }



  #set minimum year
  if(is.null(minyear)){
    minyear <- min(df[,"use_year"])
  }

  #create year bins and count records
  df <- df %>%
    dplyr::mutate(yearbin = ifelse(use_year < minyear,
                                   minyear-1,
                                   use_year))

  dfTime_pre <- df %>%
    dplyr::filter(use_year < minyear) %>%
    dplyr::count(yearbin)

  dfTime_post <- df %>%
    dplyr::filter(use_year > minyear) %>%
    dplyr::count(yearbin)

  #plot
  ggplot2::ggplot() +
    ggplot2::geom_col(data = dfTime_post, ggplot2::aes(yearbin, n)) +
    ggplot2::geom_col(data = dfTime_pre, ggplot2::aes(yearbin, n), fill="grey20", width = 1.5) +
    ggplot2::geom_text(data = dfTime_pre, ggplot2::aes(yearbin, -Inf), vjust = -.3,
              label = paste0("pre-", minyear)) +
    ggplot2::labs(y = "Number of records",
         x = "Year")

}
