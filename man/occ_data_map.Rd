% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/occ_data_map.R
\name{occ_data_map}
\alias{occ_data_map}
\title{Produce a map of occurrence records}
\usage{
occ_data_map(
  df,
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
)
}
\arguments{
\item{df}{Dataframe with (at least) coordinate and year columns.}

\item{x}{Character name of column with X coordinates (e.g. "long").}

\item{y}{Character name of column with Y coordinates (e.g. "lat").}

\item{year}{Character name of column with year.}

\item{aoi}{Either a sf::bbox object or a polygon that sets the map extent. If null,
calculated as the bounding box of records.}

\item{in_crs}{Coordinate reference system of \code{x} and \code{y} in \code{df}. Can be
given as any coordinate reference system recognised by \code{sf::st_crs} (e.g.
EPSG code).}

\item{out_crs}{Coordinate reference system of resulting map.}

\item{min_year}{Minimum year for dots breaks. Useful for datasets with long temporal tails eg gbif that
otherwise results in very long legends.}

\item{title_prefix}{Name to add to records legend title i.e "title_prefix records" .}

\item{bin}{Logical to bin by x/y/year. Recommended for large datasets to reduce data overlap, write time, and memory use.}

\item{bin_method}{How to bin locations, if bin==TRUE. One of "round" (to round coords; faster but less granular) or
"cell" (to bin by raster cells; slower but more specific).}

\item{bin_size}{Integer. Either number of decimal places to round coords to, or cell resolution (in m).}

\item{aoi_args}{Named list of additional aesthetics to send to tmap::tm_polygons. Default is black border with linewidth=2.}

\item{aoi_name}{Character. Name of aoi (if using) to add to legend}
}
\value{
tmap object. Additional layers or map parameters can be added using \code{occ_data_map(df) + tmap::tm_blah()}
}
\description{
Map points from a data frame with coordinates and display them binned by decade. Also includes
option to bin points spatially to speed up drawing of large datasets. Alternative to data_name_map.
}
