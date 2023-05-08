

#' Map points from a data frame with coordinates in columns `x` and `y`
#'
#'
#' @param df Dataframe with coordinate columns.
#' @param x Character name of column with X coordinates (e.g. "long").
#' @param y Character name of column with Y coordinates (e.g. "lat").
#' @param polys Simple feature with polygons to map points on.
#' @param polys_col Column in `polys` to include in map legend.
#' @param polys_name Character name to give to `polys` legend title.
#' @param polys_palette Vector of column colours to use for `polys`.
#' @param polys_alpha Passed to `tmap::tm_polygons()` `alpha` argument.
#' @param aoi Object on which `sf::st_bbox` will return a bounding box for the
#' map output.
#' @param in_crs Coordinate reference system of `x` and `y` in `df`. Can be
#' given as any coordinate reference system recognised by `sf::st_crs` (e.g.
#' EPSG code).
#' @param out_crs Coordinate reference system of resulting map.
#' @param dots_args Named list of arguments to pass to `tmap::tm_dots()`.
#' @param layout_args Named list of arguments to pass to `tmap::tm_layout()`.
#'
#' @return tmap object
#' @export
#'
#' @examples
data_name_map <- function(df
                          , x = "long"
                          , y = "lat"
                          , polys = NULL
                          , polys_col = NULL
                          , polys_name = NULL
                          , polys_palette = NULL
                          , polys_alpha = 1
                          , aoi = NULL
                          , in_crs = 4283
                          , out_crs = 4326
                          , dots_args = list()
                          , layout_args = list()
                          ) {

  dist_args <- c(x
                 , y
                 , if("col" %in% names(dots_args)) dots_args$col else "records"
                 )

  map_df <- df %>%
    {if("col" %in% names(dots_args)) (.) else (.) %>% dplyr::mutate(records = 1)} %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(dist_args))) %>%
    sf::st_as_sf(coords = c(x,y)
                 , crs = in_crs
                 ) %>%
    sf::st_transform(crs = out_crs)

  bbox_map <- if(isTRUE(is.null(aoi))) {

    sf::st_bbox(map_df)

  } else {

    sf::st_bbox(aoi)

  }

  polys_data_name <- polys %>%
    sf::st_transform(crs = st_crs(in_crs)) %>%
    sf::st_make_valid() %>%
    sf::st_filter(bbox_map %>%
                    sf::st_as_sfc() %>%
                    sf::st_make_valid() %>%
                    sf::st_transform(crs = st_crs(in_crs))
                  ) %>%
    sf::st_transform(crs = out_crs)

  if(isTRUE(is.null(polys_palette))) {

    polys_palette <- viridis::viridis(nrow(polys_data_name))

  }

  dots_args <- c(dots_args
                 , if(!"col" %in% names(dots_args)) list(col = "records")
                 )

  tmap::tm_shape(map_df
                 , bbox = bbox_map
                 ) +
      do.call(tmap::tm_dots
              , dots_args
              ) +
    tmap::tm_shape(polys_data_name) +
      tmap::tm_polygons(col = polys_col
                        , palette = polys_palette
                        , title = polys_name
                        , alpha = polys_alpha
                        ) +
    do.call(tmap::tm_layout
            , layout_args
            )

}
