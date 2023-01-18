

#' Map points from a data frame with coordinates in columns `x` and `y`
#'
#'
#'
#' @param df Dataframe with coordinate columns.
#' @param x Character name of column with X coordinates (e.g. "long").
#' @param y Character name of column with Y coordinates (e.g. "lat").
#' @param map_class Optional character name of column to include in map legend.
#' @param polys Simple feature with polygons to map points on.
#' @param polys_col Column in `polys` to include in map legend.
#' @param polys_name Character name to give to `polys` legend title.
#' @param polys_palette Vector of column colours to use for `polys`.
#' @param aoi Object on which `sf::st_bbox` will return a bounding box for the
#' map output.
#' @param in_crs Coordinate reference system of `x` and `y` in `df`. Can be
#' given as any coordinate reference system recognised by `sf::st_crs` (e.g.
#' EPSG code).
#' @param out_crs Coordinate reference system of resulting map.
#' @param ... `tmap::tm_dots` arguments. Note `col`, `title`, `palette` and
#' `legend.format` are already defined within `data_name_map`
#'
#' @return tmap object
#' @export
#'
#' @examples
data_name_map <- function(df
                          , x = "long"
                          , y = "lat"
                          , map_class = NULL
                          , polys = NULL
                          , polys_col = NULL
                          , polys_name = NULL
                          , polys_palette = NULL
                          , aoi = NULL
                          , in_crs = 4283
                          , out_crs = 4326
                          , ...
                          ) {

  map_df <- df %>%
    dplyr::distinct(across(any_of(map_class))
                    , !!ensym(x)
                    , !!ensym(y)
                    ) %>%
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

  df_data_name <- unique(df$data_name)

  if(isTRUE(is.null(polys_palette))) {

    polys_palette <- viridis::viridis(nrow(polys_data_name))

  }

  tmap::tm_shape(polys_data_name
           , bbox = bbox_map
           ) +
    tmap::tm_polygons(col = polys_col
                      , palette = polys_palette
                      , title = polys_name
                ) +
    tmap::tm_shape(map_df) +
    tmap::tm_dots(col = grep(map_class
                             , names(map_df)
                             , value = TRUE
                             )
                  , title = paste0(df_data_name
                                   , " records"
                                   )
                  , palette = "plasma"
                  , legend.format = list(big.mark = "")
                  , ...
                  ) +
    tmap::tm_layout(legend.outside = TRUE
                    , legend.outside.position = "left"
                    #, inner.margins=c(.04,.03, .02, .01)
                    )

  }
