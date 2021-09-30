
#' Plot distribution of probabilities for predicting a class
#'
#' @param this_clust Name of class
#' @param pred_df Dataframe of predicted probabilities for each class
#' @param cluster_colours Dataframe mapping classes to colours. This is also used
#' to determine how many other classes were in the clustering.
#'
#' @return
#' @export
#'
#' @examples
make_cluster_pred_plots <- function(this_clust
                               , pred_df
                               , cluster_colours
                               ) {

  this_clust <- as.character(this_clust)

  df <- pred_df %>%
    tidyr::pivot_longer(cluster_colours$cluster,names_to = "cluster") %>%
    #dplyr::filter(value > 0) %>%
    dplyr::mutate(cluster = factor(cluster, levels = levs)
                  , cluster = forcats::fct_expand(cluster, toupper(this_clust))
                  ) %>%
    dplyr::left_join(cluster_colours) %>%
    dplyr::mutate(cluster = replace(cluster, cluster == this_clust, toupper(this_clust))
                  , cluster = forcats::fct_relevel(cluster, toupper(this_clust))
                  )

  ggplot(df,aes(value,fct_rev(cluster),colour = colour)) +
    ggdist::stat_eye() +
    scale_colour_identity() +
    labs(y = "Ecosystem") +
    theme(text = element_text(size = 8))

}
