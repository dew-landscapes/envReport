

  test_map <- function(this_clust
                       , full_pred_df
                       ) {

    this_clust <- as.character(this_clust)

    a <- full_pred_df %>%
      dplyr::select(any_of(c("cluster","predCluster","cell"))) %>%
      dplyr::left_join(ecosystemsDesc[,c("cluster","vegCol")]
                       , by = c("predCluster" = "cluster")
                       ) %>%
      dplyr::select(cell,predCluster,vegCol) %>%
      dplyr::mutate(correct = as.character(predCluster) == this_clust
                    , predCluster = if_else(correct,labelForCorrect,as.character(predCluster))
                    , predCluster = factor(predCluster, levels = c(labelForCorrect,levels(clusters)))
                    , vegCol = if_else(predCluster == labelForCorrect,"black",vegCol)
                    , type = "Site in ecosystem"
      ) %>%
      make_sf_from_cell(r)

    b <- full_pred_df %>%
      dplyr::select(any_of(c("cluster","predCluster","cell"))) %>%
      dplyr::filter(predCluster == this_clust) %>%
      dplyr::left_join(ecosystemsDesc[,c("cluster","vegCol")]) %>%
      dplyr::select(cell,cluster,vegCol) %>%
      dplyr::mutate(correct = as.character(cluster) == this_clust
                    , size = if_else(correct,1,0.1)
                    , cluster = if_else(correct,labelForCorrect,as.character(cluster))
                    , cluster = factor(cluster, levels = c(labelForCorrect,levels(clusters)))
                    , vegCol = if_else(cluster == labelForCorrect,"black",vegCol)
                    , type = "Site predicted to be ecosystem"
      ) %>%
      dplyr::rename(predCluster = cluster) %>%
      make_sf_from_cell(r)

    mapDf <- a %>%
      dplyr::bind_rows(b) %>%
      dplyr::mutate(predCluster = factor(predCluster))

    colours <- mapDf %>%
      st_set_geometry(NULL) %>%
      dplyr::distinct(predCluster,vegCol) %>%
      dplyr::arrange(predCluster)

    mapPalette <- colours$vegCol
    names(mapPalette) <- colours$predCluster

    tm_shape(aoi) +
      tm_fill(col = "grey"
              , alpha = 0.5
      ) +
      tm_shape(sa) +
      tm_borders(col = "black") +
      tm_shape(mapDf) +
      tm_dots(col = "predCluster"
              , title = "Predicted ecosystem"
              , palette = mapPalette
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
