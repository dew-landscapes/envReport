
#' Describe occurrence data
#'
#' Build a description of occurrence records including number of records/sites/taxa, earliest and latest records/taxa, and summaries of data fields.
#'
#' @param df_file File path to .parquet of occurrence records, e.g. as made by envImport. Needs aligned attribute names.
#' @param site_cols Character vector. Name of columns defining a site. E.g. lat/long, other coordinates, or raster cell ID.
#' @param visit_cols Character vector. Name of columns defining a visit, usually
#'   site (e.g. coordinates/cell) plus timestamp (e.g. date/year), but could
#'   also include others such as a site name or spatial reliability
#' @param data_name How to refer to the dataset, usually a dataset name (e.g "BDBSA", "TERN", "Other"). Only used to paste as text
#'
#' @return Text paragraph/s including formatting for markdown
#' @export


data_summary_text <- function(df_file
                              , site_cols = c("lat", "long")
                              , visit_cols = c("lat", "long", "year")
                              , data_name = NULL
                              , aoi_name = if(exists("settings")) settings$name else NA
) {

  if(!c("character") %in% class(df_file)) stop("data_summary_text expects a file path to a parquet file.")

  df <- arrow::open_dataset(df_file)

  if(is.null(data_name)) data_name <- df |> dplyr::pull(data_name) |> unique()

  # df <- df %>%
  #   dplyr::mutate(year = lubridate::year(date)
  #                 , month = lubridate::month(date)
  #                 , formatted_name = gsub("^([A-Za-z]+ [a-z]+)(.*)$", "_\\1_\\2", original_name) #add italics around genus-species
  #   )

  format_name <- function(x) { #add italics around genus-species
    gsub("^([A-Za-z]+ [a-z]+)(.*)$", "_\\1_\\2", x)
  }

  #counts
  taxa <- length(unique(dplyr::pull(df, original_name)))
  sites <- df %>% dplyr::distinct(across(any_of(site_cols))) %>% dplyr::collect() |> nrow()
  visits <- df %>% dplyr::distinct(across(any_of(visit_cols))) %>% dplyr::collect() |> nrow()
  records <- nrow(df)

  #first record year & month
  minYear <- df %>%
    dplyr::pull(year) %>%
    min()

  minYearMonth <- df %>%
    dplyr::filter(year == minYear) %>%
    dplyr::pull(month) %>%
    min()

  #newest record year & month
  maxYear <- df %>%
    dplyr::pull(year) %>%
    max()

  maxYearMonth <- df %>%
    dplyr::filter(year == maxYear) %>%
    dplyr::pull(month) %>%
    max()

  #most recorded taxa, with count
  maxSpp <- df %>%
    dplyr::count(original_name) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::collect() |>
    dplyr::slice(1) |>
    dplyr::mutate(formatted_name = format_name(original_name))

  #first record year of most recorded taxa
  maxSppMinYear <- df %>%
    dplyr::select(original_name, year) |>
    dplyr::collect() |>
    dplyr::add_count(original_name) %>%
    dplyr::filter(n == max(n)) %>%
    dplyr::filter(year == min(year)) %>%
    dplyr::pull(year) %>%
    unique()

  #full-row record of most recent taxa
  lastSppDf <- df %>%
    dplyr::select(original_name, year, month) |>
    dplyr::collect() |>
    dplyr::group_by(original_name) %>%
    dplyr::filter(year == min(year)) %>%
    dplyr::filter(month == min(month)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::filter(month == max(month)) %>%
    dplyr::add_count(original_name) %>%
    dplyr::slice(1) |>
    dplyr::mutate(formatted_name = format_name(original_name))

  #name of most recent taxa
  lastSpp <- lastSppDf %>%
    dplyr::distinct(formatted_name) %>%
    dplyr::pull(formatted_name)

  #year of most recent taxa
  lastSppYear <- lastSppDf %>%
    dplyr::distinct(year) %>%
    dplyr::pull(year)

  #month of most recent taxa
  lastSppMonth <- lastSppDf %>%
    dplyr::distinct(month) %>%
    dplyr::pull(month)


  #build text of oldest/newest records
  distinct_yrs <- df |>
    dplyr::select(year) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    nrow()
  if(distinct_yrs > 1) {

    year_range <- paste0("The earliest "
                         , data_name
                         , " records were collected in "
                         , month.name[minYearMonth]
                         , " "
                         , minYear
                         , ", and the latest records were collected in "
                         , month.name[minYearMonth]
                         , " "
                         , maxYear
                         , "."
    )

    year_spp <- paste0(", first recorded in "
                       , maxSppMinYear
                       , ". The last taxa to be added to the data set was "
                       , lastSpp
                       , " which was first recorded in "
                       , month.name[lastSppMonth]
                       , " "
                       , lastSppYear
                       , "."
    )

  } else {

    year_range <- paste0("All ",data_name," records were dated ",unique(df$year),".")

    year_spp <- "."

  }

  #plant data/survey columns
  if("quad_x" %in% names(df)) {
    df <- df %>%
      dplyr::collect() |>
      dplyr::mutate(quad_metres = quad_x*quad_y)
  }

  plantCols <- tibble::tribble(
    ~col, ~colName
    , "use_cover", "cover estimate"
    , "lifeform", "lifeform (shrub, grass, tree etc.)"
    , "lifespan", "lifespan (annual or perennial)"
    , "quad_metres", "quadrat size"
  )

  # deal with 'cover' --------
  if(any(c("cover", "cover_code") %in% names(df))) {

    if("cover" %in% names(df)) {

      df <- df |>
        dplyr::mutate(use_cover = dplyr::if_else(cover <= 0, NA, cover))

    } else df <- df |> dplyr::mutate(use_cover = NA)

    if("cover_code" %in% names(df)) {

      df <- df |>
        dplyr::mutate(use_cover = dplyr::if_else(!is.na(cover_code), 1, use_cover))

    }

  }

  #percent of records that are plants
  plantPc <- df %>%
    dplyr::filter(kingdom == "Plantae") %>%
    {nrow(.) / nrow(df) * 100} %>%
    round(digits = 1)

  plant_text <- if(all(any(c(plantCols$col) %in% names(df))
                       , plantPc > 0)
  ) {

    #build descriptions of % of records that have plantCols recorded
    plant_df <- df |>
      dplyr::filter(kingdom == "Plantae") |>
      dplyr::select(any_of(plantCols$col)) |>
      dplyr::collect() |>
      dplyr::summarise(records = dplyr::n()
                       , dplyr::across(dplyr::any_of(plantCols$col)
                                       , \(x) sum(!is.na(x))
                                       )
                       ) %>%
      tidyr::pivot_longer(2:ncol(.)
                          , names_to = "col"
                          , values_to = "value") %>%
      dplyr::mutate(per = round(100 * value / records, 1)) %>%
      dplyr::left_join(plantCols) %>%
      dplyr::mutate(text = paste0(per
                                  , "% had a "
                                  , colName
                                  , " recorded"
                                  )
                    )

    plant_summs <- plant_df %>%
      dplyr::pull(text)

    #write text of plantCols
    paste0(" \n\nOf the plant records ("
           , plantPc
           , "% of the data), "
           , if(all(plant_df$per == 0)){
             paste0("none had a "
                    , envFunc::vec_to_sentence(plantCols$colName, end = "or")
                    , "recorded")
           } else paste(envFunc::vec_to_sentence(plant_summs))
           , "."
    )

  }

  #visits & spatial accuracy
  visitsYear <- df %>%
    dplyr::count(dplyr::across(dplyr::any_of(visit_cols))
                 , name = "richness"
    ) |>
    dplyr::collect()

  visits_text <- paste0(" \n\nOf the "
                        , format(visits, big.mark = ",")
                        , " visits (site at a point in time), "
                        , if("rel_metres" %in% names(df)) {

                          with_rel <- df %>%
                            dplyr::group_by(dplyr::across(tidyselect::any_of(visit_cols))) %>%
                            dplyr::select(any_of(visit_cols), rel_metres) |>
                            dplyr::filter(!is.na(rel_metres)) |>
                            dplyr::collect() |>
                            dplyr::summarise(rel_metres = max(rel_metres, na.rm = TRUE), .groups = "keep") %>%
                            dplyr::ungroup() |>
                            dplyr::filter(rel_metres > 0)

                          per_with_rel <- 100 * nrow(with_rel) / visits

                          if(per_with_rel == 0){

                            paste0("none had a spatial accuracy estimate recorded")

                            } else {

                              paste0(round(per_with_rel, 2), "% had a spatial accuracy estimate recorded.")

                            }

                        } else  "none had a spatial accuracy estimate recorded."

                        )

  #single-taxa sites
  singletons <- df %>%
    dplyr::select(any_of(visit_cols), original_name) |>
    dplyr::count(dplyr::across(dplyr::any_of(visit_cols)), name = "sr") %>%
    dplyr::filter(sr == 1) %>%
    dplyr::inner_join(df %>%
                        dplyr::select(any_of(visit_cols), original_name)) %>%
    dplyr::collect() |>
    dplyr::count(original_name, name = "records") %>%
    dplyr::mutate(big_records = format(records, big.mark=",")
                  , text = paste0(format_name(original_name)
                                , " ("
                                , big_records
                                , " sites)"
    )
    )

  singleton_text <- if(nrow(singletons) > 0) {
    paste0(" The most common taxa at singleton sites "
           , if(dplyr::n_distinct(singletons$original_name) > 1) "were " else "was "
           , singletons %>%
             dplyr::arrange(desc(records)) %>%
             dplyr::pull(text) %>%
             head(5) %>%
             envFunc::vec_to_sentence()
           , "."
    )
  } else NULL

  #highest-taxa sites
  maxSite <- df %>%
    dplyr::count(dplyr::across(dplyr::all_of(site_cols))
                 , lat
                 , long
                 , name = "richness"
    ) %>%
    dplyr::pull(richness) %>%
    max()

  # Build final output
  text <- paste0("The retrieval of data from "
                 , data_name
                 , if(!is.na(aoi_name)) paste0(" for ", aoi_name)
                 , " returned a data set of "
                 , format(records, big.mark=",")
                 , " records of "
                 , format(taxa, big.mark = ",")
                 , " different taxa, from "
                 , format(sites, big.mark = ",")
                 , " sites. "
                 , year_range #' The earliest records ... and the latest records...'

                 , " The taxa with the most records was "
                 , maxSpp$formatted_name
                 , " with "
                 , format(maxSpp$n, big.mark=",")
                 , " records"
                 , year_spp # ', first recorded in... The last taxa to be added was...'

                 , plant_text # 'Of the plant records (%), x had cover estimate...'

                 , visits_text # 'Of the n visits, x had spatial accuracy...'

                 , " \n\nThere were "
                 , format(sum(singletons$records),big.mark=",")
                 , " sites with only one taxa recorded (singleton sites)."
                 , singleton_text # 'The most common taxa at singleton sites were...'
                 , " The site with the most taxa had "
                 , format(maxSite, big.mark = ",")
                 , " taxa recorded, and the visit with the most taxa had "
                 , format(max(visitsYear$richness, na.rm = TRUE), big.mark = ",")
                 , " taxa recorded."
  )

  return(text)

}
