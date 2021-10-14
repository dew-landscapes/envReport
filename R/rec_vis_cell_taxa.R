#' How many records, visits, sites and taxa in a dataframe
#'
#' @param df Dataframe with taxa records.
#' @param site_cols Name of columns defining sites.
#' @param visit_cols Name of columns defining visits.
#' @param taxa_col Name of column containing the taxa.
#'
#' @return One row dataframe
#' @export
#'
#' @examples
rec_vis_sit_tax <- function(df
                            , site_cols
                            , visit_cols
                            , taxa_col
                            ) {

  df %>%
    dplyr::summarise(taxa = n_distinct(!!ensym(taxa_col))
                     , records = nrow(.)
                     , visits = n_distinct(across(any_of(site_cols)))
                     , sites = n_distinct(across(any_of(visit_cols)))
                     )

}
