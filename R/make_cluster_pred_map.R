

#' Create a map of correctly and incorrectly predicted sites
#'
#' @param this_clust Name of class.
#' @param full_pred_df All sites predicted to class.
#' @param cluster_colours Dataframe of classes mapped to colours.
#' @param context Character names of columns in full_pred_df defining the context.
#' @param clust_col Character name of column with the class membership.
#' @param pred_clust_col Character name of the column with the predicted class membership.
#' @param label_for_correct Label to use in maps for correctly predicted sites.
#' @param x Column in `full_pred_df` with the x coordinates
#' @param y Column in `full_pred_df` with the y coordinates
#' @param xy_crs Anything interpreted by `crs` argument of `sf::st_as_sf()` as
#' valid coordinate system for `x` and `y`.
#'
#' @return
#' @export
#'
#' @examples
  make_cluster_pred_map <- function(this_clust
                                    , full_pred_df
                                    , cluster_colours
                                    , context
                                    , clust_col = "cluster"
                                    , pred_clust_col = "pred_cluster"
                                    , label_for_correct = "correct"
                                    , x = "long"
                                    , y = "lat"
                                    , xy_crs = 4326
                                    ) {

    this_clust <- as.character(this_clust)

    levs <- levels(full_pred_df[clust_col][[1]])

    a <- full_pred_df %>%
      dplyr::filter(!!ensym(clust_col) == this_clust) %>%
      dplyr::select(any_of(context), !!ensym(clust_col), !!ensym(pred_clust_col)) %>%
      dplyr::left_join(cluster_colours) %>%
      dplyr::mutate(correct = as.character(!!ensym(pred_clust_col)) == !!ensym(clust_col)
                    , !!ensym(pred_clust_col) := if_else(correct, label_for_correct, as.character(!!ensym(pred_clust_col)))
                    , !!ensym(pred_clust_col) := factor(!!ensym(pred_clust_col), levels = c(label_for_correct,levs))
                    , colour = if_else(!!ensym(pred_clust_col) == label_for_correct,"black",colour)
                    , type = "Site in ecosystem"
                    )

    b <- full_pred_df %>%
      dplyr::filter(!!ensym(pred_clust_col) == this_clust) %>%
      dplyr::select(any_of(context), !!ensym(clust_col), !!ensym(pred_clust_col)) %>%
      dplyr::left_join(cluster_colours) %>%
      dplyr::rename(!!ensym(clust_col) := !!ensym(pred_clust_col)
                    , !!ensym(pred_clust_col) := !!ensym(clust_col)
                    ) %>%
      dplyr::mutate(correct = as.character(!!ensym(pred_clust_col)) == !!ensym(clust_col)
                    , !!ensym(pred_clust_col) := if_else(correct, label_for_correct, as.character(!!ensym(pred_clust_col)))
                    , !!ensym(pred_clust_col) := factor(!!ensym(pred_clust_col), levels = c(label_for_correct,levs))
                    , colour = if_else(!!ensym(pred_clust_col) == label_for_correct,"black",colour)
                    , type = "Site predicted to be ecosystem"
                    )

    map_df <- a %>%
      dplyr::bind_rows(b) %>%
      dplyr::mutate(!!ensym(pred_clust_col) := factor(!!ensym(pred_clust_col))) %>%
      sf::st_as_sf(coords = c(x, y)
                   , crs = xy_crs
                   )

    colours <- cluster_colours %>%
      dplyr::arrange(!!ensym(clust_col)) %>%
      dplyr::distinct(!!ensym(clust_col),colour) %>%
      dplyr::rename(!!ensym(pred_clust_col) := !!ensym(clust_col))

    map_palette <- c("black", colours$colour)
    names(map_palette) <- c(label_for_correct
                            , as.character(colours[pred_clust_col][[1]])
                            )

    tm_shape(aoi) +
      tm_fill(col = "grey"
              , alpha = 0.5
              ) +
    tm_shape(sa) +
      tm_borders(col = "black") +
    tm_shape(map_df) +
      tm_dots(col = pred_clust_col
              , title = "Predicted ecosystem"
              , palette = map_palette
              , size = 0.5
              ) +
      tm_facets(by = "type"
                , free.coords = FALSE
                ) +
      tm_layout(title = paste0('Ecosystem\n"',this_clust,'"')
                , legend.outside = TRUE
                , legend.outside.position = "left"
                )

  }
