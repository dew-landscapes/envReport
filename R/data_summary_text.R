
#' Produce a histogram of points over time from a data frame of occurrences
#'
#' @param df Data frame of occurrence records, e.g. as made by envImport. Needs aligned attribute names.
#' @param site_cols Character vector. Name of columns defining a site. E.g. lat/long, other coordinates, or raster cell ID.
#' @param visit_cols Character vector. Name of columns defining a visit i.e. site plus timestamp. E.g. coordinates/cell plus date/year
#' @param data_name How to refer to the dataset, usually a dataset name (e.g "BDBSA", "TERN", "Other"). Only used to paste as text
#'
#' @return Text paragraph/s including formatting for markdown
#' @export


data_summary_text <- function(df
                              , site_cols = c("lat", "long")
                              , visit_cols = c("lat", "long", "year")
                              , data_name
) {

  df <- df %>%
    dplyr::mutate(year = lubridate::year(date)
                  , month = lubridate::month(date)
                  , formatted_name = gsub("^([A-Za-z]+ [a-z]+)(.*)$", "_\\1_\\2", original_name) #add italics around genus-species
    )

  if("quad_x" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(quad_metres = quad_x*quad_y)
  }


  taxa <- dplyr::n_distinct(df["original_name"])
  sites <- df %>% dplyr::distinct(across(any_of(site_cols))) %>% nrow()
  visits <- df %>% dplyr::distinct(across(any_of(visit_cols))) %>% nrow()
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
    dplyr::count(formatted_name) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::slice(1)

  #first record year of most recorded taxa
  maxSppMinYear <- df %>%
    dplyr::add_count(original_name) %>%
    dplyr::filter(n == max(n)) %>%
    dplyr::filter(year == min(year)) %>%
    dplyr::pull(year) %>%
    unique()

  #full-row record of most recent taxa
  lastSppDf <- df %>%
    dplyr::group_by(formatted_name) %>%
    dplyr::filter(year == min(year)) %>%
    dplyr::filter(month == min(month)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::filter(month == max(month)) %>%
    dplyr::add_count(formatted_name) %>%
    dplyr::slice(1)

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
  if(dplyr::n_distinct(df$year) > 1) {

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
  plantCols <- tibble::tribble(
    ~col, ~colName
    , "cover", "cover estimate"
    , "lifeform", "lifeform (shrub, grass, tree etc.)"
    , "lifespan", "lifespan (annual or perennial)"
    , "quad_metres", "quadrat size"
  )

  #percent of records that are plants
  plantPc <- df %>%
    dplyr::filter(kingdom == "Plantae") %>%
    {nrow(.) / nrow(df) * 100} %>%
    round(digits = 1)

  plant_text <- if(all(any(c(plantCols$col) %in% names(df))
                       , plantPc > 0)
  ) {

    #build descriptions of % of records that have plantCols recorded
    plant_df <- df %>% dplyr::filter(kingdom=="Plantae") %>%
      {if(!c("COVER") %in% names(df)) (.) else if("COVCODE" %in% names(.)) (.) %>%
          dplyr::mutate(COVCODE = if_else(is.na(COVCODE)
                                          , as.character(COVER)
                                          , COVCODE)) else (.) %>%
          dplyr::mutate(COVCODE = COVER)
      } %>%
      dplyr::summarise(records = dplyr::n()
                       , dplyr::across(dplyr::any_of(plantCols$col)
                                       , ~sum(!is.na(.)))
      ) %>%
      tidyr::pivot_longer(2:ncol(.)
                          , names_to = "col"
                          , values_to = "value") %>%
      dplyr::mutate(per = round(100*value/records, 1)) %>%
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

  #visits
  visitsCols <- tibble::tribble(
    ~col, ~colName
    , "rel_metres", "spatial accuracy estimate"
  )

  #count records by visit
  visitsYear <- df %>%
    dplyr::count(dplyr::across(dplyr::all_of(c("year", "month")))
                 , dplyr::across(dplyr::any_of(visitsCols$col))
                 , lat
                 , long
                 , name = "richness"
    )

  visits_text <- if(any(visitsCols$col %in% names(df))) {

    visitsRel <- visitsYear %>%
      dplyr::summarise(visits = dplyr::n()
                       , nas = sum(is.na(rel_metres))
      )

    visits_df <- visitsYear %>%
      dplyr::summarise(records = dplyr::n()
                       , dplyr::across(dplyr::any_of(visitsCols$col)
                                       , ~sum(!is.na(.)))
      ) %>%
      tidyr::pivot_longer(2:ncol(.),names_to = "col", values_to = "value") %>%
      dplyr::mutate(per = round(100*value/records, 1)) %>%
      dplyr::left_join(visitsCols) %>%
      dplyr::mutate(text = paste0(per
                                  , "% had a "
                                  , colName
                                  , "recorded")
      )

    paste0(" \n\nOf the "
           , format(visits, big.mark = ",")
           , " visits (site at a point in time), "
           , if(visits_df$per == 0){
             paste0("none had a "
                    , envFunc::vec_to_sentence(visitsCols$colName, end = "or")
                    , " recorded")
           } else paste(envFunc::vec_to_sentence(visits_df))
           , "."
    )

  } else NULL

  #single-taxa sites
  singletons <- df %>%
    dplyr::count(dplyr::across(dplyr::any_of(visit_cols)), name = "sr") %>%
    dplyr::filter(sr == 1) %>%
    dplyr::inner_join(df) %>%
    dplyr::count(formatted_name, name = "records") %>%
    dplyr::mutate(text = paste0(formatted_name
                                , " ("
                                , format(records,big.mark=",",trim = TRUE)
                                , " sites)"
    )
    )

  singleton_text <- if(nrow(singletons) > 0) {
    paste0(" The most common taxa at singleton sites "
           , if(dplyr::n_distinct(singletons$formatted_name) > 1) "were " else "was "
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
                 , "The site with the most taxa had "
                 , maxSite
                 , " taxa recorded, and the visit with the most taxa had "
                 , max(visitsYear$richness, na.rm = TRUE)
                 , " taxa recorded."
  )

  return(text)

}
