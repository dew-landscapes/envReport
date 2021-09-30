
#' Plot distribution of probabilities for predicting a class
#'
#' @param this_clust Name of class
#' @param pred_df Dataframe of predicted probabilities for each class
#' @param cluster_colours Dataframe mapping classes to colours. This is also used
#' to determine how many other classes were in the clustering.
#' @param clust_col Name of column in `cluster_colours` with the classes.
#'
#' @return
#' @export
#'
#' @examples
make_cluster_pred_plot <- function(this_clust
                               , pred_df
                               , cluster_colours
                               , clust_col = "cluster"
                               ) {

  this_clust <- as.character(this_clust)

  levs <- levels(cluster_colours[clust_col][[1]])

  df <- pred_df %>%
    tidyr::pivot_longer(cluster_colours[clust_col][[1]]
                        , names_to = clust_col
                        ) %>%
    #dplyr::filter(value > 0) %>%
    dplyr::mutate(!!ensym(clust_col) := factor(!!ensym(clust_col), levels = levs)
                  , !!ensym(clust_col) := forcats::fct_expand(!!ensym(clust_col), toupper(this_clust))
                  ) %>%
    dplyr::left_join(cluster_colours) %>%
    dplyr::mutate(!!ensym(clust_col) := replace(!!ensym(clust_col), !!ensym(clust_col) == this_clust, toupper(this_clust))
                  , !!ensym(clust_col) := forcats::fct_relevel(!!ensym(clust_col), toupper(this_clust))
                  )

  ggplot(df,aes(value,fct_rev(!!ensym(clust_col)),colour = colour)) +
    ggdist::stat_eye() +
    scale_colour_identity() +
    labs(y = "Ecosystem") +
    theme(text = element_text(size = 8))

}
