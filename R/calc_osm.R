#'Calculate metrics based on OpenStreetMap data at point locations
#'
#'Calculate metrics from OpenStreetMap (OSM) vector data,
#'at specific point locations and buffer radii.
#'Currently supports vector data of buildings (polygons) and roads (lines).
#'
#'@param vector sf dataframe of either buildings (polygons) or roads (lines).
#'@param name Specify either `"buildings"` or `"roads"`.
#'@param points Point locations (sf object) to calculate the metrics.
#'@param buffer_sizes Radius of circle (in mapunits) for each point location;
#'metrics will be calculated within the buffer area.
#'@param building_ndsm SpatRaster object (`terra::rast()`) (optional). A continuous raster
#'of the normalised Digital Surface Model, used to calculate building heights.
#'If absent (`NULL`) and the variable is named in `predictors_osm`, the column `building_height` is used instead.
#'Defaults to `NULL`.
#'@param building_height Column name in `vector` for building height.
#'Defaults to `"height"`.
#'@param building_levels Column name in `vector` for the number of building levels.
#'Defaults to `"levels"`. Column data should be numeric.
#'@param road_lanes Column name in `vector` for the number of lanes per road line.
#'Defaults to `"lanes"`. Column data should be numeric.
#'@param point_id Column name of the sampling point id within the `points` sf. Defaults to `"point_id"`.
#'
#'@return A list containing the features/metrics calculated for `points`,  appended as new columns.
#'Each element in the list corresponds to a particular buffer size.
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
calc_osm <- function(vector, name = NULL,
                     points, buffer_sizes,
                     building_ndsm = NULL, building_height = "height", building_levels = "levels",
                     road_lanes = "lanes",
                     point_id = "point_id"){

  # Error checking ------------------
  if(!all(sf::st_geometry_type(points) == "POINT")){
    stop("Error: \nInput 'points' must all be sf points!")
  }

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_numeric(buffer_sizes, lower = 0.000001, finite = TRUE, any.missing = FALSE, add = coll)
  checkmate::assert_subset(name, choices = c("buildings", "roads"), empty.ok = FALSE, add = coll)
  # add check: if buildings, must be polygon/multipolygons
  # add check: if roads, must be line/polyline

  checkmate::reportAssertions(coll)



  # Calculations ------------------

  results <- list()
  for(i in 1:length(buffer_sizes)){

    # subset to areas within sampling points for relevant round
    suppressWarnings(vector_sub <- vector %>%
                       sf::st_make_valid() %>%
                       sf::st_intersection(points %>% # for relevant buffer radius
                                             sf::st_buffer(dist = buffer_sizes[i])))
    row.names(vector_sub) <- NULL # reset row names for later indexing
    vector_sub <- vector_sub %>%
      tibble::rownames_to_column("ID")


    # PROCESS BUILDINGS
    if(stringr::str_detect(name, "buildings")){

      vector_sub$area_m2 <- sf::st_area(vector_sub) # calc area (clipped buffer radius)


      # building-height metrics: default use column 'building_height'
      if(is.null(building_ndsm) & !is.null(vector_sub[[building_height]])) {

        message(paste0("Note: Height data from the column '", building_height,"' is used."))

        suppressWarnings(buildings_summarised <- vector_sub %>%
                           mutate(height = as.numeric(.data[[building_height]])) %>%
                           dplyr::group_by(.data[[point_id]]) %>%
                           dplyr::summarise("osm_buildingVol_m3" :=
                                              sum(units::set_units(.data$area_m2, value = NULL) * .data[[building_height]],
                                                  na.rm = FALSE)) %>%
                           # dplyr::mutate(dplyr::across(.cols = tidyselect::contains("osm_buildingVol_m3"),
                           #                             .fns = ~tidyr::replace_na(., 0))) %>%
                           sf::st_set_geometry(NULL)) # remove geometry

        # append to points
        suppressMessages(points_result <- points %>%
          dplyr::left_join(buildings_summarised,
                           by = c(.data[[point_id]]))) #%>%
          # dplyr::mutate(dplyr::across(.cols = everything(), # points with no buildings have a value of 0
          #                             .fns = ~tidyr::replace_na(., 0)))
        rm(buildings_summarised)


        # building-height metrics: if column 'building_height' not present & no ndsm data
      } else if(is.null(building_ndsm) & is.null(vector_sub[[building_height]])){

        warning(paste0("Column '", building_height, "' not present. Returning NAs.\n"))
        points_result <- points %>%
          dplyr::mutate(osm_buildingVol_m3 = NA)


        # building-height metrics: use ndsm data to process
      } else if(!is.null(building_ndsm)){

        buildingheights <-
          terra::extract(building_ndsm, # ndsm raster for relevant round.
                         terra::vect(vector_sub), # assign pixel values to polygons
                         fun = mean, # mean height value of pixels within
                         method = "simple", weights = FALSE, exact = FALSE,
                         na.rm = TRUE) %>% # ignore NA pixels
          dplyr::rename(height = 2) %>%  # rename second col (by index)
          dplyr::mutate(height = ifelse(is.nan(.data$height), NA, .data$height))
        vector_sub <- vector_sub %>%
          dplyr::select(-tidyselect::any_of("height")) %>% # remove col for height (to be replaced)
          dplyr::left_join(buildingheights %>%
                             mutate(ID = as.character(.data$ID)), by = "ID") %>%
          dplyr::select(-.data$ID)
        rm(buildingheights)

        buildings_summarised <- vector_sub %>%
          dplyr::group_by(.data[[point_id]]) %>%
          dplyr::summarise("osm_buildingVol_m3" :=
                             sum(units::set_units(.data$area_m2, value = NULL) * .data$height,
                                 na.rm = TRUE)) %>%
          dplyr::mutate(dplyr::across(.cols = tidyselect::everything(),
                                      .fns = ~tidyr::replace_na(., 0))) %>%
          sf::st_set_geometry(NULL) # remove geometry

        # append to points
        suppressMessages(points_result <- points %>%
          dplyr::left_join(buildings_summarised,
                           by = c(.data[[point_id]])) %>%
          dplyr::mutate(dplyr::across(.cols = tidyselect::contains("osm_buildingVol_m3"), # points with no buildings have a value of 0
                                      .fns = ~tidyr::replace_na(., 0))))
        rm(buildings_summarised)

      }


      # other building metrics
      suppressMessages(to_append <- vector_sub %>%
                         dplyr::mutate(area_m2 = units::set_units(.data$area_m2, value = NULL)) %>%
                         dplyr::mutate(GFA_m2 = .data$area_m2 * .data[[building_levels]]) %>%
                         dplyr::mutate(GFA_m2 = units::set_units(.data$GFA_m2, value = NULL)) %>%

                         # summarise
                         dplyr::group_by(.data[[point_id]]) %>%

                         dplyr::summarise("osm_buildingArea_m2" := sum(.data$area_m2, na.rm = TRUE),
                                          "osm_buildingGFA_m2" := sum(.data$GFA_m2, na.rm = TRUE)) %>%
                         sf::st_set_geometry(NULL) %>%

                         dplyr::mutate("osm_buildingAvgLvl" := .data[["osm_buildingGFA_m2"]] / .data[["osm_buildingArea_m2"]]) %>%
                         dplyr::mutate("osm_buildingFA_ratio" := .data[["osm_buildingGFA_m2"]] / (pi * as.numeric(buffer_sizes[i]) ^ 2)) %>%
                         dplyr::mutate(dplyr::across(.cols = tidyselect::everything(),
                                                     .fns = ~tidyr::replace_na(., 0))))

      suppressMessages(points_result <- points_result %>%
                         left_join(to_append) %>%
                         dplyr::mutate(dplyr::across(.cols = tidyselect::contains(paste0("osm_", c("buildingArea_m2", "buildingGFA_m2", "buildingAvgLvl", "buildingFA_ratio"))),
                                                     .fns = ~tidyr::replace_na(., 0))))
      rm(to_append)

    }


    # ROADS
    if(stringr::str_detect(name, "roads")){

      vector_sub$length_m <- sf::st_length(vector_sub) # calc length (clipped buffer radius)

      vector_sub <- vector_sub %>%
        dplyr::mutate(lanelength_m = .data$length_m * ifelse(is.na(as.numeric(.data[[road_lanes]])), 1, as.numeric(.data[[road_lanes]]))) %>% # considered as 1 lane if NA!
        dplyr::mutate(lanelength_m = units::set_units(.data$lanelength_m, value = NULL))

      suppressMessages(to_append <- vector_sub %>%
                         dplyr::group_by(.data[[point_id]]) %>%
                         dplyr::summarise("osm_laneLength_m" := sum(.data$lanelength_m),
                                          "osm_laneDensity" := sum(.data$lanelength_m) / (pi * as.numeric(buffer_sizes[i]) ^ 2)) %>%
                         sf::st_set_geometry(NULL)) # remove geometry

      suppressMessages(points_result <- points %>%
                         dplyr::left_join(to_append) %>%
                         dplyr::mutate(dplyr::across(.cols = tidyselect::contains(paste0("osm_", c("laneLength_m", "laneDensity"))),
                                                     .fns = ~tidyr::replace_na(., 0))))
      rm(to_append)

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

