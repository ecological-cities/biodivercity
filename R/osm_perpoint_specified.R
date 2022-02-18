#'Calculate OpenStreetMap metrics at sampling points specified by name
#'
#'Summarise specified metrics of OpenStreetMap buildings/roads at sampling points.
#'Multiple (list of) vectors may be classified, each corresponding
#'to a survey period in the dataset `points`. The character vector of predictor names
#'include the specified buffer radii within which to summarise each metric.
#'Buildings and roads are to be calculated separately.
#'
#'@param vector_list List of sf objects for either buildings (polygons) or roads (lines).
#'The sequence (number) of list elements should correspond to the survey periods (an integer) present in `points`.
#'@param predictors_osm Vector (character) of predictor variables to be calculated from the vector file(s).
#'The naming format is `<radius in metres>_osm_<metric>` (e.g. `r50m_osm_buildingFA_ratio`).
#'@param building_ndsm List of SpatRaster objects (`terra::rast()`) (optional). Each element is a continuous raster
#'of the normalised Digital Surface Model, used to calculate building heights.
#'The sequence (number) of list elements should correspond to the survey periods (an integer) present in `points`.
#'If absent (`NULL`), the column `building_height` is used instead. Defaults to `NULL`.
#'@param building_height Column name of the building height for sf objects in `vector_list`.
#'Defaults to `"height"`.
#'@param building_levels Column name of the number of building levels for sf objects in `vector_list`.
#'Defaults to `"levels"`.
#'@param road_lanes Column name of the number of
#'@param points Sampling points (sf object) representing the locations to calculate the metrics.
#'@param point_id Column name of the sampling point id within the `points` sf. Defaults to `"point_id"`.
#'@param period Column name of the survey period within the `points` sf. Defaults to `"period"`.
#'The column should contain integers that correspond to the number of elements in `vector_list`.
#'
#'@return A list containing the specified OpenStreetMap metrics summarised at the sampling points provided.
#'Each element of the list contains results for a survey `period` (represented as integers in `points`).
#'
#'@import checkmate
#'@import dplyr
#'@import sf
#'@importFrom rlang .data
#'@importFrom stringr str_extract str_detect
#'@importFrom tidyr pivot_wider drop_na replace_na
#'@importFrom tibble rownames_to_column
#'@importFrom tidyselect matches everything contains any_of
#'@importFrom terra extract vect
#'@importFrom units set_units
#'
#'@export
osm_perpoint_specified <- function(vector_list,
                                   building_ndsm = NULL, building_height = "height", building_levels = "levels",
                                   road_lanes = "lanes",
                                   predictors_osm,
                                   points,
                                   point_id = "point_id",
                                   period = "period"){

  # Error checking ------------------
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_character(predictors_osm, min.chars = 1, any.missing = FALSE, all.missing = FALSE, null.ok = FALSE, unique = TRUE, add = coll)
  checkmate::assert_list(vector_list, any.missing = FALSE, all.missing = FALSE, min.len = 1)
  checkmate::assert_list(building_ndsm, any.missing = FALSE, all.missing = FALSE, null.ok = TRUE)

  checkmate::reportAssertions(coll)


  # Calculations ------------------

  input <- data.frame(metric = predictors_osm %>%
                        stringr::str_extract("(?<=osm_).*"),
                      radius = predictors_osm %>%
                        stringr::str_extract("(?<=^r)\\d+")) %>%
    tidyr::drop_na()


  results_all <- list()
  for(i in seq_along(vector_list)){ # per period

    results <- points %>%
      dplyr::filter(.data[[period]] == i) %>%
      dplyr::select(c(.data[[point_id]], .data[[period]]))

    for(j in seq_len(nrow(input))){ # per predictor

      # subset to areas within sampling points for relevant round
      suppressWarnings(vector_sub <- vector_list[[i]] %>%
        sf::st_make_valid() %>%
        sf::st_intersection(results %>% # for relevant buffer radius
                              sf::st_buffer(dist = as.numeric(input$radius[j]))))
      row.names(vector_sub) <- NULL # reset row names for later indexing
      vector_sub <- vector_sub %>%
        tibble::rownames_to_column("ID")


      # PROCESS BUILDINGS
      if(stringr::str_detect(input$metric[j], "building")){

        vector_sub$area_m2 <- sf::st_area(vector_sub) # calc area (clipped buffer radius)


        # building-height metrics: default use column 'building_height'
        if(is.null(building_ndsm) & stringr::str_detect(input$metric[j], "buildingVol_m3") &
           !is.null(vector_sub[[building_height]])) {

          message(paste0("Note: predictors_osm includes buildingVol_m3, which requires height data. Height data from the column '",
                         building_height,"' is used."))

          suppressWarnings(buildings_summarised <- vector_sub %>%
            dplyr::group_by(.data[[period]], .data[[point_id]]) %>%
            dplyr::summarise("r{input$radius[j]}m_osm_buildingVol_m3" :=
                               sum(units::set_units(.data$area_m2, value = NULL) * .data[[building_height]],
                                   na.rm = TRUE)) %>%
            dplyr::mutate(dplyr::across(.cols = tidyselect::everything(),
                                        .fns = ~tidyr::replace_na(., 0))) %>%
            sf::st_set_geometry(NULL)) # remove geometry

          # append to results
          results <- results %>%
            dplyr::left_join(buildings_summarised,
                             by = c(.data[[point_id]], .data[[period]])) %>%
            dplyr::mutate(dplyr::across(.cols = tidyselect::contains(paste0("r", input$radius[j], "m_osm_buildingVol_m3")), # points with no buildings have a value of 0
                          .fns = ~tidyr::replace_na(., 0)))
          rm(buildings_summarised)


        # building-height metrics: if column 'building_height' not present & no ndsm data
        } else if(is.null(building_ndsm) & stringr::str_detect(input$metric[j], "buildingVol_m3") &
                  is.null(vector_sub[[building_height]])){

          warning(paste0("Column '", building_height, "' not present in element ", i," of vector_list for calculation of ",
                         paste0("r", input$radius[j], "m_osm_", input$metric[j]), ". Returning NAs.\n"))
          results[[paste0("r", input$radius[j], "m_osm_", input$metric[j])]] <- NA


        # building-height metrics: use ndsm data to process
        } else if(!is.null(building_ndsm) & stringr::str_detect(input$metric[j], "buildingVol_m3") & (i <= length(building_ndsm))){

          buildingheights <-
            terra::extract(building_ndsm[[i]], # ndsm raster for relevant round.
                           terra::vect(vector_sub), # assign pixel values to polygons
                           fun = mean, # mean height value of pixels within
                           method = "simple", weights = FALSE, exact = FALSE,
                           na.rm = TRUE # ignore NA pixels
            ) %>%
            dplyr::rename(height = 2) %>%  # rename second col (by index)
            dplyr::mutate(height = ifelse(is.nan(.data$height), NA, .data$height))
          vector_sub <- vector_sub %>%
            dplyr::select(-tidyselect::any_of("height")) %>% # remove col for height (to be replaced)
            dplyr::left_join(buildingheights %>%
                               mutate(ID = as.character(.data$ID)), by = "ID") %>%
            dplyr::select(-.data$ID)
          rm(buildingheights)

          buildings_summarised <- vector_sub %>%
            dplyr::group_by(.data[[period]], .data[[point_id]]) %>%
            dplyr::summarise("r{input$radius[j]}m_osm_buildingVol_m3" :=
                               sum(units::set_units(.data$area_m2, value = NULL) * .data$height,
                                   na.rm = TRUE)) %>%
            dplyr::mutate(dplyr::across(.cols = tidyselect::everything(),
                                 .fns = ~tidyr::replace_na(., 0))) %>%
            sf::st_set_geometry(NULL) # remove geometry

          # append to results
          results <- results %>%
            dplyr::left_join(buildings_summarised,
                             by = c(.data[[point_id]], .data[[period]])) %>%
            dplyr::mutate(dplyr::across(.cols = tidyselect::contains(paste0("r", input$radius[j], "m_osm_buildingVol_m3")), # points with no buildings have a value of 0
                          .fns = ~tidyr::replace_na(., 0)))
          rm(buildings_summarised)


        # building-height metrics: if ndsm data is present but missing for some rounds
        } else if(!is.null(building_ndsm) & i > length(building_ndsm)){

          warning(paste0("List lengths of vector_list and building_ndsm are different! List elements matched by sequence. Returning NAs for data in survey period ",i ,".\n"))
          results[[paste0("r", input$radius[j], "m_osm_", input$metric[j])]] <- NA

        }


        # other building metrics
        if(input$metric[j] %in% c("buildingArea_m2", "buildingGFA_m2", "buildingAvgLvl", "buildingFA_ratio")){

          suppressMessages(to_append <- vector_sub %>%
            dplyr::mutate(area_m2 = units::set_units(.data$area_m2, value = NULL)) %>%
            dplyr::mutate(GFA_m2 = .data$area_m2 * .data[[building_levels]]) %>%
            dplyr::mutate(GFA_m2 = units::set_units(.data$GFA_m2, value = NULL)) %>%

            # summarise
            dplyr::group_by(.data[[period]], .data[[point_id]]) %>%

            dplyr::summarise("r{input$radius[j]}m_osm_buildingArea_m2" := sum(.data$area_m2, na.rm = TRUE),
                      "r{input$radius[j]}m_osm_buildingGFA_m2" := sum(.data$GFA_m2, na.rm = TRUE)) %>%
            sf::st_set_geometry(NULL) %>%

            dplyr::mutate("r{input$radius[j]}m_osm_buildingAvgLvl" := .data[[paste0("r", input$radius[j], "m_osm_buildingGFA_m2")]] / .data[[paste0("r", input$radius[j], "m_osm_buildingArea_m2")]]) %>%
            dplyr::mutate("r{input$radius[j]}m_osm_buildingFA_ratio" := .data[[paste0("r", input$radius[j], "m_osm_buildingGFA_m2")]] / (pi * as.numeric(input$radius[j]) ^ 2)) %>%
            dplyr::mutate(dplyr::across(.cols = tidyselect::everything(),
                          .fns = ~tidyr::replace_na(., 0))) %>%
            dplyr::select(c(.data[[point_id]], .data[[period]], matches(paste0("_osm_", input$metric[j]))))) # only select metric of interest

          suppressMessages(results <- results %>%
            left_join(to_append) %>%
            dplyr::mutate(dplyr::across(.cols = tidyselect::contains(paste0("r", input$radius[j], "m_osm_", c("buildingArea_m2", "buildingGFA_m2", "buildingAvgLvl", "buildingFA_ratio"))),
                          .fns = ~tidyr::replace_na(., 0))))
          rm(to_append)
        }

      }


      # ROADS
      if(stringr::str_detect(input$metric[j], "lane")){

        vector_sub$length_m <- sf::st_length(vector_sub) # calc length (clipped buffer radius)

        vector_sub <- vector_sub %>%
          dplyr::mutate(lanelength_m = .data$length_m * ifelse(is.na(as.numeric(.data[[road_lanes]])), 1, as.numeric(.data[[road_lanes]]))) %>% # considered as 1 lane if NA!
          dplyr::mutate(lanelength_m = units::set_units(.data$lanelength_m, value = NULL))

        suppressMessages(to_append <- vector_sub %>%
          dplyr::group_by(.data[[period]], .data[[point_id]]) %>%
          dplyr::summarise("r{input$radius[j]}m_osm_laneLength_m" := sum(.data$lanelength_m),
                    "r{input$radius[j]}m_osm_laneDensity" := sum(.data$lanelength_m) / (pi * as.numeric(input$radius[j]) ^ 2)) %>% # radius depends on circles
          sf::st_set_geometry(NULL) %>% # remove geometry
          dplyr::select(c(.data[[point_id]], .data[[period]], matches(paste0("_osm_", input$metric[j]))))) # only select metric of interest

        suppressMessages(results <- results %>%
          dplyr::left_join(to_append) %>%
          dplyr::mutate(dplyr::across(.cols = tidyselect::contains(paste0("r", input$radius[j], "m_osm_", c("laneLength_m", "laneDensity"))),
                        .fns = ~tidyr::replace_na(., 0))))
        rm(to_append)

      }

      rm(j, vector_sub)
    }

    results_all[[i]] <- results
    rm(i)
  }

  return(results_all)
}
