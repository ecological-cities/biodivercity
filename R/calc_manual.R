#'Calculate metrics based on manually generated data at point locations
#'
#'Calculate metrics from manually generated vector data,
#'at specific point locations and buffer radii.
#'Currently supports vector data of buildings (polygons), roads (lines),
#'trees (points), shrubs (polygons), turf (polygons), natural vegetation (polygons),
#'and water (polygons).
#'
#'@param vector sf dataframe containing geometric features (points, polygons or lines) to be summarised.
#'Coordinate reference system should be similar to `points`.
#'@param name Specify either `"buildings"`, `"roads"`, `"trees"`, `"shrubs'`, `"turf"`, `"natveg"`, or `"water"`.
#'Used to represent the type of data in `vector`, each to be processed differently.
#'@param points Point locations (sf object) to calculate the metrics.
#'@param buffer_sizes Radius of circle (in mapunits) for each point location;
#'metrics will be calculated within the buffer area.
#'@param plant_species Column name in `vector` for plant species names,
#'if input `vector` is vegetation (i.e., argument `name=` `"trees"` or `"shrubs"`).
#'Defaults to `"species"`. Column data should be of type character.
#'@param building_levels Column name in `vector` for the number of building levels,
#'when `name="buildings"`. Defaults to `"levels"`. Column data should be numeric.
#'@param road_lanes Column name in `vector` for the number of lanes per road line,
#'when `name="roads"`. Defaults to `"lanes"`. Column data should be numeric.
#'
#'@return A list containing the features/metrics calculated for `points`, appended as new columns.
#'Each element in the list corresponds to a particular buffer size.
#'
#'@import dplyr
#'@importFrom checkmate makeAssertCollection assertTRUE assert_numeric assert_subset reportAssertions
#'@importFrom sf st_geometry_type st_is_longlat st_is_valid st_crs st_buffer st_intersection st_set_geometry st_area st_length
#'@importFrom rlang .data
#'@importFrom tidyselect everything contains
#'@importFrom tidyr replace_na
#'@importFrom tibble rownames_to_column
#'@importFrom stringr str_detect

#'
#'@export
calc_manual <-
  function(vector, name = NULL,
           points, buffer_sizes,
           plant_species = "species",
           building_levels = "levels",
           road_lanes = "lanes") {


    # Error checking ------------------
    if(!all(sf::st_geometry_type(points) == "POINT")){
      stop("Error: \nInput 'points' must all be sf points!")
    }

    coll <- checkmate::makeAssertCollection()

    # data type
    checkmate::assertTRUE(!sf::st_is_longlat(points) & !is.null(sf::st_crs(points)), add = coll)  # must be projected crs
    checkmate::assertTRUE(all(sf::st_is_valid(vector)), add = coll)
    checkmate::assert_numeric(buffer_sizes, lower = 0.000001, finite = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 1, null.ok = FALSE, add = coll)
    checkmate::assertTRUE(sf::st_crs(vector) == sf::st_crs(points))
    checkmate::assert_subset(name, choices = c("trees", "shrubs", "turf", "natveg", "water", "buildings", "roads"), empty.ok = FALSE, add = coll)

    if(name == "trees" | name == "shrubs"){
      checkmate::assert_subset(plant_species, choices = colnames(vector), empty.ok = FALSE, add = coll)
    }
    if(name == "buildings"){
      checkmate::assert_subset(building_levels, choices = colnames(vector), empty.ok = FALSE, add = coll)
    }
    if(name == "roads"){
      checkmate::assert_subset(road_lanes, choices = colnames(vector), empty.ok = FALSE, add = coll)
    }

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    results <- list()
    for(i in 1:length(buffer_sizes)){

      # subset to areas within sampling points for relevant round
      suppressWarnings(vector_sub <- vector %>%
                         sf::st_make_valid() %>%
                         sf::st_intersection(points %>% # for relevant buffer radius
                                               sf::st_buffer(dist = buffer_sizes[i]) %>%
                                               magrittr::set_rownames(NULL) %>%
                                               tibble::rownames_to_column("POINTID") %>%
                                               dplyr::select(.data$POINTID)))


      # PROCESS BUILDINGS
      if(stringr::str_detect(name, "buildings")){

        if(nrow(vector_sub) > 0){ # run if data present

          vector_sub$area_m2 <- sf::st_area(vector_sub) # calc area (clipped buffer radius)

          suppressMessages(to_append <- vector_sub %>%
                             dplyr::mutate(area_m2 = units::set_units(.data$area_m2, value = NULL)) %>%
                             dplyr::mutate(GFA_m2 = .data$area_m2 * .data[[building_levels]]) %>%
                             dplyr::mutate(GFA_m2 = units::set_units(.data$GFA_m2, value = NULL)) %>%

                             # summarise
                             dplyr::group_by(.data$POINTID) %>%

                             dplyr::summarise("man_buildingArea_m2" := sum(.data$area_m2, na.rm = TRUE),
                                              "man_buildingGFA_m2" := sum(.data$GFA_m2, na.rm = TRUE)) %>%
                             sf::st_set_geometry(NULL) %>%

                             dplyr::mutate("man_buildingAvgLvl" := .data[["man_buildingGFA_m2"]] / .data[["man_buildingArea_m2"]]) %>%
                             dplyr::mutate("man_buildingFA_ratio" := .data[["man_buildingGFA_m2"]] / (pi * as.numeric(buffer_sizes[i]) ^ 2)) %>%
                             dplyr::mutate(dplyr::across(.cols = tidyselect::everything(),
                                                         .fns = ~tidyr::replace_na(., 0))))

          suppressMessages(points_result <- points %>%
                             magrittr::set_rownames(NULL) %>%
                             tibble::rownames_to_column("POINTID") %>%
                             left_join(to_append) %>%
                             dplyr::mutate(dplyr::across(.cols = tidyselect::contains(paste0("man_", c("buildingArea_m2", "buildingGFA_m2", "buildingAvgLvl", "buildingFA_ratio"))),
                                                         .fns = ~tidyr::replace_na(., 0))) %>%
                             dplyr::select(-.data$POINTID))
          rm(to_append)
        }else{ # if no data

          suppressMessages(points_result <- points %>%
                             dplyr::mutate("man_buildingArea_m2" := 0,
                                           "man_buildingGFA_m2" := 0,
                                           "man_buildingAvgLvl" := 0,
                                           "man_buildingFA_ratio" := 0))
        }
      }


      # ROADS
      if(stringr::str_detect(name, "roads")){

        if(nrow(vector_sub) > 0){ # run if data present

          vector_sub$length_m <- sf::st_length(vector_sub) # calc length (clipped buffer radius)

          vector_sub <- vector_sub %>%
            dplyr::mutate(lanelength_m = .data$length_m * ifelse(is.na(as.numeric(.data[[road_lanes]])), 1, as.numeric(.data[[road_lanes]]))) %>% # considered as 1 lane if NA!
            dplyr::mutate(lanelength_m = units::set_units(.data$lanelength_m, value = NULL))

          suppressMessages(to_append <- vector_sub %>%
                             dplyr::group_by(.data$POINTID) %>%
                             dplyr::summarise("man_laneLength_m" := sum(.data$lanelength_m),
                                              "man_laneDensity" := sum(.data$lanelength_m) / (pi * as.numeric(buffer_sizes[i]) ^ 2)) %>%
                             sf::st_set_geometry(NULL)) # remove geometry

          suppressMessages(points_result <- points %>%
                             magrittr::set_rownames(NULL) %>%
                             tibble::rownames_to_column("POINTID") %>%
                             dplyr::left_join(to_append) %>%
                             dplyr::mutate(dplyr::across(.cols = tidyselect::contains(paste0("man_", c("laneLength_m", "laneDensity"))),
                                                         .fns = ~tidyr::replace_na(., 0))) %>%
                             dplyr::select(-.data$POINTID))
          rm(to_append)
        }else{ # if no data

          suppressMessages(points_result <- points %>%
                             dplyr::mutate("man_laneLength_m" := 0,
                                           "man_laneDensity" := 0))
        }
      }


      # TREES
      if(stringr::str_detect(name, "trees")){

        if(nrow(vector_sub) > 0){ # run if data present

          # first, get count per spp
          suppressMessages(sppAbund <- vector_sub %>% # com matrix per point/round %>%
                             dplyr::group_by(.data$POINTID, .data[[plant_species]]) %>%
                             dplyr::summarise(n = n()) %>%
                             sf::st_set_geometry(NULL))
          # spp richness
          suppressMessages(sppRich <- sppAbund %>% #
                             dplyr::group_by(.data$POINTID) %>%
                             dplyr::summarise("man_tree_sprich" := n())) # use dynamic var names
          # tree count
          suppressMessages(count <- sppAbund %>% # tree count per point
                             dplyr::group_by(.data$POINTID) %>%
                             dplyr::summarise("man_tree_count" := sum(n)))
          rm(sppAbund)

          # join results together
          suppressMessages(points_result  <- points %>%
                             magrittr::set_rownames(NULL) %>%
                             tibble::rownames_to_column("POINTID") %>%
                             dplyr::left_join(sppRich) %>%
                             dplyr::left_join(count) %>%
                             dplyr::mutate(dplyr::across(.cols = tidyselect::contains(paste0("man_", c("tree_sprich", "tree_count"))),
                                                         .fns = ~tidyr::replace_na(., 0))) %>%
                             dplyr::select(-.data$POINTID))

        }else{ # if no data

          suppressMessages(points_result <- points %>%
                                 dplyr::mutate("man_tree_sprich" := 0,
                                               "man_tree_count" := 0))
        }
      }


      # SHRUBS
      if(stringr::str_detect(name, "shrubs")){

        vector_sub$area_m2 <- sf::st_area(vector_sub) # calc area (clipped buffer radius)

        if(nrow(vector_sub) > 0){ # run if data present

          # spp richness
          suppressMessages(sppRich <- vector_sub %>% # com matrix per point/round %>%
                             dplyr::group_by(.data$POINTID, .data[[plant_species]]) %>%
                             dplyr::summarise(n = n()) %>%
                             dplyr::group_by(.data$POINTID) %>%
                             dplyr::summarise("man_shrub_sprich" := n()) %>%
                             sf::st_set_geometry(NULL))
          # shrub area
          suppressMessages(area <- vector_sub %>%
                             dplyr::group_by(.data$POINTID) %>%
                             dplyr::summarise("man_shrub_pland" := (sum(.data$area_m2)/(pi*buffer_sizes[i]^2))*100) %>%
                             sf::st_set_geometry(NULL))

          # join results together
          suppressMessages(points_result  <- points %>%
                             magrittr::set_rownames(NULL) %>%
                             tibble::rownames_to_column("POINTID") %>%
                             dplyr::left_join(sppRich) %>%
                             dplyr::left_join(area) %>%
                             dplyr::mutate(dplyr::across(.cols = tidyselect::contains(paste0("man_", c("shrub_pland", "shrub_sprich"))),
                                                         .fns = ~tidyr::replace_na(., 0))) %>%
                             dplyr::select(-.data$POINTID))
        }else{ # if no data

          suppressMessages(points_result <- points %>%
                             dplyr::mutate("man_shrub_pland" := 0,
                                           "man_shrub_sprich" := 0))
        }
      }


      # TURF
      if(stringr::str_detect(name, "turf")){

        vector_sub$area_m2 <- sf::st_area(vector_sub) # calc area (clipped buffer radius)

        if(nrow(vector_sub) > 0){

          suppressMessages(to_append <- vector_sub %>% # turf area
                             dplyr::group_by(.data$POINTID) %>%
                             dplyr::summarise("man_turf_pland" := (sum(.data$area_m2)/(pi*buffer_sizes[i]^2))*100) %>%
                             sf::st_set_geometry(NULL))

          # join results together
          suppressMessages(points_result  <- points %>%
                             magrittr::set_rownames(NULL) %>%
                             tibble::rownames_to_column("POINTID") %>%
                             dplyr::left_join(to_append) %>%
                             dplyr::mutate(dplyr::across(.cols = tidyselect::contains(paste0("man_", c("turf_pland"))),
                                                         .fns = ~tidyr::replace_na(., 0))) %>%
                             dplyr::select(-.data$POINTID))

          rm(to_append)
        }else{

          suppressMessages(points_result <- points %>%
                             dplyr::mutate("man_turf_pland" := 0))
        }
      }


      # NAT VEG
      if(stringr::str_detect(name, "natveg")){

        vector_sub$area_m2 <- sf::st_area(vector_sub) # calc area (clipped buffer radius)

        if(nrow(vector_sub) > 0){

          suppressMessages(to_append <- vector_sub %>% # turf area
                             dplyr::group_by(.data$POINTID) %>%
                             dplyr::summarise("man_natveg_pland" := (sum(.data$area_m2)/(pi*buffer_sizes[i]^2))*100) %>%
                             sf::st_set_geometry(NULL))

          # join results together
          suppressMessages(points_result  <- points %>%
                             magrittr::set_rownames(NULL) %>%
                             tibble::rownames_to_column("POINTID") %>%
                             dplyr::left_join(to_append) %>%
                             dplyr::mutate(dplyr::across(.cols = tidyselect::contains(paste0("man_", c("natveg_pland"))),
                                                         .fns = ~tidyr::replace_na(., 0))) %>%
                             dplyr::select(-.data$POINTID))

          rm(to_append)
        }else{

          suppressMessages(points_result <- points %>%
                             dplyr::mutate("man_natveg_pland" := 0))
        }

      }


      # WATER
      if(stringr::str_detect(name, "water")){

        vector_sub$area_m2 <- sf::st_area(vector_sub) # calc area (clipped buffer radius)

        if(nrow(vector_sub) > 0){

          suppressMessages(to_append <- vector_sub %>% # turf area
                             dplyr::group_by(.data$POINTID) %>%
                             dplyr::summarise("man_water_pland" := (sum(.data$area_m2)/(pi*buffer_sizes[i]^2))*100) %>%
                             sf::st_set_geometry(NULL))

          # join results together
          suppressMessages(points_result  <- points %>%
                             magrittr::set_rownames(NULL) %>%
                             tibble::rownames_to_column("POINTID") %>%
                             dplyr::left_join(to_append) %>%
                             dplyr::mutate(dplyr::across(.cols = tidyselect::contains(paste0("man_", c("water_pland"))),
                                                         .fns = ~tidyr::replace_na(., 0))) %>%
                             dplyr::select(-.data$POINTID))

          rm(to_append)
        }else{

          suppressMessages(points_result <- points %>%
                             dplyr::mutate("man_water_pland" := 0))
        }

      }

      message(paste0(Sys.time(), " Processed for ", i,"/", length(buffer_sizes),
                     " buffer sizes (", buffer_sizes[i], "m)"))

      results[[i]] <- points_result

      rm(vector_sub, i)
      gc()

    }

    names(results) <- buffer_sizes
    return(results)
  }
