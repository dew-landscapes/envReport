#' Produce a map of occurrence records
#'
#' Map points from a data frame with coordinates and display them binned by decade. Also includes
#' option to bin points spatially to speed up drawing of large datasets. Alternative to data_name_map.
#'
#'
#' @param df Dataframe with (at least) coordinate and year columns.
#' @param x Character name of column with X coordinates (e.g. "long").
#' @param y Character name of column with Y coordinates (e.g. "lat").
#' @param year Character name of column with year.
#' @param aoi Either a sf::bbox object or a polygon that sets the map extent. If null,
#' calculated as the bounding box of records.
#' @param in_crs Coordinate reference system of `x` and `y` in `df`. Can be
#' given as any coordinate reference system recognised by `sf::st_crs` (e.g.
#' EPSG code).
#' @param out_crs Coordinate reference system of resulting map.
#' @param min_year Minimum year for dots breaks. Useful for datasets with long temporal tails eg gbif that
#' otherwise results in very long legends.
#' @param title_prefix Name to add to records legend title i.e "title_prefix records" .
#' @param bin Logical to bin by x/y/year. Recommended for large datasets to reduce data overlap, write time, and memory use.
#' @param bin_method How to bin locations, if bin==TRUE. One of "round" (to round coords; faster but less granular) or
#' "cell" (to bin by raster cells; slower but more specific).
#' @param bin_size Integer. Either number of decimal places to round coords to, or cell resolution (in m).
#' @param aoi_args Named list of additional aesthetics to send to tmap::tm_polygons. Default is black border with linewidth=2.
#' @param aoi_name Character. Name of aoi (if using) to add to legend
#'
#' @return tmap object. Additional layers or map parameters can be added using `occ_data_map(df) + tmap::tm_blah()`
#' @export
#'
#'

occ_data_map <- function(df,
                         x = "long",
                         y = "lat",
                         year = "year",
                         aoi = NULL,
                         in_crs = 4326,
                         out_crs = 4326,
                         min_year = 1950,
                         title_prefix = NULL,
                         bin = FALSE,
                         bin_method = NULL,
                         bin_size = NULL,
                         aoi_args = list(),
                         aoi_name
){

  #arg checks
  if (!bin && (!is.null(bin_method) || !is.null(bin_size))){
    warning("Spatial bins not enabled ('bin=FALSE'); bin_method/bin_size ignored.")
  }

  if (bin){
    if(!bin_method %in% c("cell", "round")) {
      stop("'bin_method' must be either 'cell' or 'round'")
    }
    if (bin_method == "cell" && bin_size < 5){
      warning("bin_size (", bin_size, ") is likely too small for cell binning. Did you mean to use bin_method='round'?")
    }
    if (bin_method == "round" && bin_size > 5){
      warning("bin_size (", bin_size, ") is likely too small for rounding. Did you mean to use bin_method='cell'?")
    }
  }



# Round / bin data for efficiency with large maps
  if(bin){
    if(bin_method == "round"){
      df_deg <- df %>%
        sf::st_as_sf(coords = c(x,y),
                     crs = in_crs) %>%
        sf::st_transform(crs = 4326) %>%
        dplyr::mutate(long = round(sf::st_coordinates(.)[,1],
                                   bin_size),
                      lat = round(sf::st_coordinates(.)[,2],
                                  bin_size)
                      ) %>%
        sf::st_drop_geometry()

      df <- df_deg %>%
        dplyr::distinct(lat, long, year)

      bin_title <- paste0("rounded to ", bin_size, " decimal places")
    }

    if(bin_method == "cell"){
      # add projected coords
      df_proj <- df %>%
        dplyr::filter(!is.na(!!rlang::ensym(x)),
                      !is.na(!!rlang::ensym(y))) %>% select(site,  year, all_of(c(x, y))) %>% ##
        dplyr::mutate(use_long = !!rlang::ensym(x),
                      use_lat = !!rlang::ensym(y)) %>%
        sf::st_as_sf(coords = c("use_long", "use_lat")) %>%
        sf::st_set_crs(in_crs) %>%
        sf::st_transform(crs = "epsg:8059") %>%
        dplyr::mutate(use_x = sf::st_coordinates(.)[,1],
                      use_y = sf::st_coordinates(.)[,2])

      ras <- terra::rast(extent = terra::ext(sf::st_buffer(df_proj,
                                                           bin_size * 2)),
                         crs = "epsg:8059",
                         resolution = bin_size,
                         vals = 1)
      df_cell <- envRaster::add_raster_cell(ras = ras,
                                            df = df_proj %>%
                                              sf::st_drop_geometry(),
                                            x = "use_x",
                                            y = "use_y",
                                            crs_df = 8059)

      df <- df_cell %>%
        dplyr::distinct(cell, year, .keep_all = TRUE)

      bin_title <- paste0("binned to ", bin_size, "-m cells")
    }
  }

# aoi/extent

  box <- if(is.null(aoi)) {
    sf::st_bbox(sf::st_as_sf(df,
                             coords = c(x, y),
                             crs = in_crs))

  } else if(!"bbox" %in% class(aoi)) {

    sf::st_bbox(aoi %>%
                  sf::st_transform(crs = out_crs))

  } else aoi


# Map

  ##breaks args
  if(is.null(min_year)){
    min_year <- min(df[,year])
  }

  minyear <- max(min_year, floor(min(df[,year])/10) * 10) #if no records older than given min_year, then earliest decade start

  breaks <-  c(if(minyear > min(df[,year])){min(df[,year])},
               seq(minyear - 1,
                   2029,
                   10)
  )

  labels <-  c(if(minyear > min(df[,year])){paste0("pre-", minyear, " (min: ", min(df[,year]), ")")},
               paste0(seq(minyear,
                          2020,
                          10),
                      " to ",
                      seq(minyear + 9,
                          2029,
                          10)
               ) %>% stringr::str_replace_all("2029",
                                              format(Sys.Date(), "%Y"))
  )




  ##do map
  m <- tmap::tm_shape(sf::st_as_sf(df,
                                   coords = c(x, y),
                                   crs = out_crs),
                      bbox = box) +
    tmap::tm_grid(lines = FALSE,
                  crs = out_crs) +
    tmap::tm_dots(fill = "year",
                  fill.legend = tmap::tm_legend(title = paste(title_prefix, "records")),
                  fill.scale = tmap::tm_scale_intervals(values = "viridis",
                                                        label.format = list(big.mark = ""),
                                                        breaks = breaks,
                                                        labels = labels
                  )
    )

  if(!is.null(aoi)){
    aoi_args <- c(aoi_args,
                  fill_alpha = 0,
                  col = "black",
                  lwd = 2)

    aoi_args_use <- aoi_args[!duplicated(names(aoi_args))]

    m <- m +
      tmap::tm_shape(aoi) +
      do.call(tmap::tm_polygons,
              aoi_args_use) +
      do.call(tmap::tm_add_legend,
              c(type = "polygons",
                title = aoi_name,
                aoi_args_use))
    }

   m <- m + tmap::tm_layout(legend.outside = TRUE)

  if(bin){
    m <- m + tmap::tm_title(text = paste0("* data points ", bin_title, "\n to reduce overlap and file size"),
                            size = 0.6,
                            position = c( "LEFT", "BOTTOM"))
    }

  return(m)

}

