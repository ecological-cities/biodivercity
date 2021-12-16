#'Summarise landscape variables per point based on supplied data
#'
#'Summaries will be calculated for each of the supplied layers (`layer_<layername>`)
#'and appended to the `points` data as additional columns.
#'Ensure that all layers have a (projected) coordinate reference system similar to `points`.
#'
#'@param points Sampling points (sf object) with a projected coordinate reference system.
#'@param uid_column character. Specify unique identifier column name in `points`.
#'Defaults to `'uid'`. Column data should be of type character.
#'@param layer_trees Geo-location of trees (sf points) to be summarised.
#'Coordinate reference system should be similar to `points`.
#'@param radii_trees Numeric vector of radii (in the units of the
#'projected coordinate reference system, e.g., metres) to summarise `layer_trees`.
#'Default values for the model are supplied (in metres).
#'@param layer_shrubs Vector of shrubs (sf polygons) to be summarised.
#'Coordinate reference system should be similar to `points`.
#'@param radii_shrubs Numeric vector of radii (in the units of the
#'projected coordinate reference system, e.g., metres) to summarise `layer_shrubs`.
#'Default values for the model are supplied (in metres).
#'@param species character. Specify column name for the species names
#'within `layer_trees` and `layer_shrubs` (if provided). Defaults to `'species'`.
#'Column data should be of type character.
#'@param layer_turf Vector of turf (sf polygons) to be summarised.
#'Coordinate reference system should be similar to `points`.
#'@param radii_turf Numeric vector of radii (in the units of the
#'projected coordinate reference system, e.g., metres) to summarise `layer_turf`.
#'Default values for the model are supplied (in metres).
#'@param layer_natveg Vector of natural vegetation (sf polygons) to be summarised.
#'Coordinate reference system should be similar to `points`.
#'@param radii_natveg Numeric vector of radii (in the units of the
#'projected coordinate reference system, e.g., metres) to summarise `layer_natveg`.
#'Default values for the model are supplied (in metres).
#'@param layer_water Vector of water (sf polygons) to be summarised.
#'Coordinate reference system should be similar to `points`.
#'@param radii_water Numeric vector of radii (in the units of the
#'projected coordinate reference system, e.g., metres) to summarise `layer_water`.
#'Default values for the model are supplied (in metres).
#'@param layer_buildings Vector of buildings (sf polygons) to be summarised.
#'Coordinate reference system should be similar to `points`.
#'@param radii_buildings Numeric vector of radii (in the units of the
#'projected coordinate reference system, e.g., metres) to summarise `layer_buildings`.
#'Default values for the model are supplied (in metres).
#'@param buildings_levels character. Specify column name for the number of levels
#'within `layer_buildings` (if provided). Defaults to `'levels'`. Column data should be numeric.
#'@param layer_roads Vector of roads (sf lines) to be summarised.
#'Coordinate reference system should be similar to `points`.
#'@param radii_roads Numeric vector of radii (in the units of the
#'projected coordinate reference system, e.g., metres) to summarise `layer_roads`.
#'Default values for the model are supplied (in metres).
#'@param roads_lanes character. Specify column name for the number of lanes
#'within `layer_roads` (if provided). Defaults to `'lanes'`. Column data should be numeric.
#'
#'@return The dataframe `points` with appended columns summarising the
#'various landscape layers provided (at their respective radii).
#'
#'@import checkmate
#'@import dplyr
#'@import foreach
#'@importFrom sf st_geometry_type st_is_longlat st_is_valid st_crs st_buffer st_agr st_intersection st_set_geometry st_area st_length
#'@importFrom glue glue
#'@importFrom rlang .data
#'@importFrom graphics hist.default
#'@importFrom tidyr pivot_longer pivot_wider
#'@importFrom doParallel registerDoParallel
#'
#'@export
summarise_landscape_perpt <-
  function(points,
           uid_column = "id",
           layer_trees = NULL, radii_trees = 50,
           layer_shrubs = NULL, radii_shrubs = 50,
           species = "species",
           layer_turf = NULL, radii_turf = 50,
           layer_natveg = NULL, radii_natveg = c(50, 126),
           layer_water = NULL, radii_water = 50,
           layer_buildings = NULL, radii_buildings = 50, buildings_levels = "levels",
           layer_roads = NULL, radii_roads = c(50, 126), roads_lanes = "lanes") {


    # Error checking ------------------
    if(!all(sf::st_geometry_type(points) == "POINT")){
      stop("Error: \nInput 'points' must all be sf points!")
    }

    coll <- checkmate::makeAssertCollection()

    # data type
    checkmate::assertTRUE(!sf::st_is_longlat(points) & !is.null(sf::st_crs(points)), add = coll)  # must be projected crs
    checkmate::assert_subset(uid_column, choices = colnames(points), empty.ok = FALSE, add = coll) # colname

    if(!is.null(layer_trees)){
      checkmate::assertTRUE(all(sf::st_is_valid(layer_trees)), add = coll)
      checkmate::assert_numeric(radii_trees, lower = 0.000001, finite = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 1, null.ok = TRUE, add = coll)
      checkmate::assertTRUE(sf::st_crs(layer_trees) == sf::st_crs(points))
      checkmate::assert_subset(species, choices = colnames(layer_trees), empty.ok = FALSE, add = coll) # colname
    }
    if(!is.null(layer_shrubs)){
      checkmate::assertTRUE(all(sf::st_is_valid(layer_shrubs)), add = coll)
      checkmate::assert_numeric(radii_shrubs, lower = 0.000001, finite = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 1, null.ok = TRUE, add = coll)
      checkmate::assertTRUE(sf::st_crs(layer_shrubs) == sf::st_crs(points))
      checkmate::assert_subset(species, choices = colnames(layer_shrubs), empty.ok = FALSE, add = coll) # colname
      }
    if(!is.null(layer_turf)){
      checkmate::assertTRUE(all(sf::st_is_valid(layer_turf)), add = coll)
      checkmate::assert_numeric(radii_turf, lower = 0.000001, finite = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 1, null.ok = TRUE, add = coll)
      checkmate::assertTRUE(sf::st_crs(layer_turf) == sf::st_crs(points))
      }
    if(!is.null(layer_natveg)){
      checkmate::assertTRUE(all(sf::st_is_valid(layer_natveg)), add = coll)
      checkmate::assert_numeric(radii_natveg, lower = 0.000001, finite = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 1, null.ok = TRUE, add = coll)
      checkmate::assertTRUE(sf::st_crs(layer_natveg) == sf::st_crs(points))
      }
    if(!is.null(layer_water)){
      checkmate::assertTRUE(all(sf::st_is_valid(layer_water)), add = coll)
      checkmate::assert_numeric(radii_water, lower = 0.000001, finite = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 1, null.ok = TRUE, add = coll)
      checkmate::assertTRUE(sf::st_crs(layer_water) == sf::st_crs(points))
      }
    if(!is.null(layer_buildings)){
      checkmate::assertTRUE(all(sf::st_is_valid(layer_buildings)), add = coll)
      checkmate::assert_numeric(radii_buildings, lower = 0.000001, finite = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 1, null.ok = TRUE, add = coll)
      checkmate::assertTRUE(sf::st_crs(layer_buildings) == sf::st_crs(points))
      checkmate::assert_subset(buildings_levels, choices = colnames(layer_buildings), empty.ok = FALSE, add = coll) # colname
      }
    if(!is.null(layer_roads)){
      checkmate::assertTRUE(all(sf::st_is_valid(layer_roads)), add = coll)
      checkmate::assert_numeric(radii_roads, lower = 0.000001, finite = TRUE, any.missing = FALSE, all.missing = FALSE, min.len = 1, null.ok = TRUE, add = coll)
      checkmate::assertTRUE(sf::st_crs(layer_roads) == sf::st_crs(points))
      checkmate::assert_subset(roads_lanes, choices = colnames(layer_roads), empty.ok = FALSE, add = coll) # colname
      }

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    # parallel processing
    cl <- parallel::makeCluster(parallel::detectCores()[1]-1, outfile = "") # not to overload your computer
    doParallel::registerDoParallel(cl)


    # loop across points
    output <- foreach::foreach(i = 1:nrow(points),
                               .packages = c("dplyr", "tidyr", "sf", "rlang")) %dopar% {

                                 # make results object to append data to
                                 results <- points[i,]


                                 # TREES
                                 if(!is.null(layer_trees)){
                                   radii <- list()

                                   for(j in seq_along(radii_trees)){

                                     circle <- sf::st_buffer(points[i,],
                                                             dist = radii_trees[j])

                                     # to get rid of warning message
                                     #https://github.com/r-spatial/sf/issues/406
                                     sf::st_agr(layer_trees) = "constant"
                                     sf::st_agr(circle) = "constant"

                                     lyr_intersect <- layer_trees %>%
                                       dplyr::select(.data[[species]]) %>%
                                       sf::st_intersection(circle) %>%
                                       dplyr::select(.data[[uid_column]], .data[[species]]) %>%
                                       sf::st_set_geometry(NULL)

                                     if(nrow(lyr_intersect) > 0){ # run if data present

                                       # first, get count per spp
                                       suppressMessages(sppAbund <- lyr_intersect %>% # com matrix per point/round %>%
                                                          dplyr::group_by(.data[[uid_column]], .data[[species]]) %>%
                                                          dplyr::summarise(n = n()))

                                       # spp richness
                                       suppressMessages(sppRich <- sppAbund %>% #
                                                          dplyr::group_by(.data[[uid_column]]) %>%
                                                          dplyr::summarise("r{radii_trees[j]}m_man_tree_sprich" := n())) # use dynamic var names

                                       # tree count
                                       suppressMessages(count <- sppAbund %>% # tree count per point
                                                          dplyr::group_by(.data[[uid_column]]) %>%
                                                          dplyr::summarise("r{radii_trees[j]}m_man_tree_count" := sum(n)))

                                       rm(sppAbund)

                                       # diversity indices
                                       suppressMessages(com_matrix <- lyr_intersect %>%
                                                          tidyr::pivot_longer(cols = -c(.data[[uid_column]])) %>%
                                                          dplyr::group_by(.data[[uid_column]], .data$value) %>%
                                                          dplyr::summarise(n = n()) %>%
                                                          tidyr::pivot_wider(names_from = .data$value, values_from = n) %>%
                                                          base::replace(is.na(.),0) %>%
                                                          dplyr::ungroup())

                                       diversity <- points[i, ] %>%
                                         sf::st_set_geometry(NULL) %>%
                                         dplyr::mutate("r{radii_trees[j]}m_man_tree_shannon" := vegan::diversity(com_matrix %>% dplyr::select(-.data[[uid_column]]), index = "shannon")) %>%
                                         dplyr::mutate("r{radii_trees[j]}m_man_tree_invsimpson" := vegan::diversity(com_matrix %>% dplyr::select(-.data[[uid_column]]), index = "invsimpson"))

                                       rm(com_matrix)

                                       # join results together
                                       suppressMessages(result  <- points %>%
                                                          sf::st_set_geometry(NULL) %>%
                                                          dplyr::inner_join(sppRich) %>%
                                                          dplyr::inner_join(count) %>%
                                                          dplyr::inner_join(diversity))

                                       radii[[j]] <- result
                                       rm(result)

                                     }else{ # if no data

                                       suppressMessages(result <- points %>%
                                                          sf::st_set_geometry(NULL) %>%
                                                          dplyr::inner_join(
                                                            points[i, ] %>%
                                                              sf::st_set_geometry(NULL) %>%
                                                              dplyr::mutate("r{radii_trees[j]}m_man_tree_sprich" := 0,
                                                                            "r{radii_trees[j]}m_man_tree_count" := 0,
                                                                            "r{radii_trees[j]}m_man_tree_shannon" := NA,
                                                                            "r{radii_trees[j]}m_man_tree_invsimpson" := NA)))

                                       radii[[j]] <- result
                                       rm(result)
                                     }

                                     rm(circle, lyr_intersect, j)
                                   }

                                   # bind different radii by columns
                                   # bind_cols() results in duplicate "uid_column" columns
                                   radii <- base::Reduce(merge, radii)

                                   # append to results output (includes geometry col)
                                   results <- results %>%
                                     dplyr::left_join(radii, by = uid_column)

                                   rm(radii)
                                 }


                                 # SHRUB
                                 if(!is.null(layer_shrubs)){
                                   radii <- list()

                                   for(j in seq_along(radii_shrubs)){

                                     circle <- sf::st_buffer(points[i,],
                                                             dist = radii_shrubs[j])

                                     # to get rid of warning message
                                     sf::st_agr(layer_shrubs) = "constant"
                                     sf::st_agr(circle) = "constant"

                                     lyr_intersect <- layer_shrubs %>%
                                       dplyr::select(.data[[species]]) %>%
                                       sf::st_intersection(circle) %>%
                                       dplyr::select(.data[[uid_column]], .data[[species]]) %>%
                                       dplyr::mutate(area_m2 = .data$geometry %>% sf::st_area() %>%
                                                       units::set_units(NULL)) %>%
                                       sf::st_set_geometry(NULL)

                                     if(nrow(lyr_intersect) > 0){ # run if data present

                                       # spp richness
                                       suppressMessages(sppRich <- lyr_intersect %>% # com matrix per point/round %>%
                                                          dplyr::group_by(.data[[uid_column]], .data[[species]]) %>%
                                                          dplyr::summarise(n = n()) %>%
                                                          dplyr::group_by(.data[[uid_column]]) %>%
                                                          dplyr::summarise("r{radii_shrubs[j]}m_man_shrub_sprich" := n())) # use dynamic var names

                                       # shrub area
                                       suppressMessages(area <- lyr_intersect %>%
                                                          dplyr::group_by(.data[[uid_column]]) %>%
                                                          dplyr::summarise("r{radii_shrubs[j]}m_man_shrub_pland" := (sum(.data$area_m2)/(pi*radii_shrubs[j]^2))*100))

                                       # diversity indices
                                       suppressMessages(com_matrix <- lyr_intersect %>%
                                                          dplyr::select(.data[[uid_column]], .data[[species]]) %>%
                                                          tidyr::pivot_longer(cols = -c(.data[[uid_column]])) %>%
                                                          dplyr::group_by(.data[[uid_column]], .data$value) %>%
                                                          dplyr::summarise(n = n()) %>%
                                                          tidyr::pivot_wider(names_from = .data$value, values_from = n) %>%
                                                          base::replace(is.na(.), 0) %>%
                                                          dplyr::ungroup())

                                       diversity <-
                                         points[i, ] %>%
                                         sf::st_set_geometry(NULL) %>%
                                         dplyr::mutate("r{radii_shrubs[j]}m_man_shrub_shannon" := vegan::diversity(com_matrix %>% dplyr::select(-.data[[uid_column]]), index = "shannon")) %>%
                                         dplyr::mutate("r{radii_shrubs[j]}m_man_shrub_invsimpson" := vegan::diversity(com_matrix %>% dplyr::select(-.data[[uid_column]]), index = "invsimpson"))

                                       rm(com_matrix)

                                       # join results together
                                       suppressMessages(result  <- points %>%
                                                          sf::st_set_geometry(NULL) %>%
                                                          dplyr::inner_join(sppRich) %>%
                                                          dplyr::inner_join(area) %>%
                                                          dplyr::inner_join(diversity))

                                       radii[[j]] <- result
                                       rm(result)

                                     }else{ # if no data

                                       suppressMessages(result <- points %>%
                                                          sf::st_set_geometry(NULL) %>%
                                                          dplyr::inner_join(
                                                            points[i, ] %>%
                                                              sf::st_set_geometry(NULL) %>%
                                                              dplyr::mutate("r{radii_shrubs[j]}m_man_shrub_sprich" := 0,
                                                                            "r{radii_shrubs[j]}m_man_shrub_pland" := 0,
                                                                            "r{radii_shrubs[j]}m_man_shrub_shannon" := NA,
                                                                            "r{radii_shrubs[j]}m_man_shrub_invsimpson" := NA)))

                                       radii[[j]] <- result
                                       rm(result)
                                     }

                                     rm(circle, lyr_intersect, j)
                                   }

                                   # bind different radii by columns
                                   radii <- base::Reduce(merge, radii)

                                   # append to results output (includes geometry col)
                                   results <- results %>%
                                     dplyr::left_join(radii, by = uid_column)

                                   rm(radii)
                                 }


                                 # TURF
                                 if(!is.null(layer_turf)){
                                   radii <- list()

                                   for(j in seq_along(radii_turf)){

                                     circle <- sf::st_buffer(points[i,],
                                                             dist = radii_turf[j])

                                     # to get rid of warning message
                                     sf::st_agr(layer_turf) = "constant"
                                     sf::st_agr(circle) = "constant"

                                     lyr_intersect <- layer_turf %>%
                                       dplyr::select(.data$geometry) %>%
                                       sf::st_intersection(circle) %>%
                                       dplyr::mutate(area_m2 = .data$geometry %>% sf::st_area() %>%
                                                       units::set_units(NULL)) %>%
                                       sf::st_set_geometry(NULL)

                                     if(nrow(lyr_intersect) > 0){

                                       suppressMessages(result <- lyr_intersect %>% # turf area
                                                          dplyr::group_by(.data[[uid_column]]) %>%
                                                          dplyr::summarise("r{radii_turf[j]}m_man_turf_pland" := (sum(.data$area_m2)/(pi*radii_turf[j]^2))*100))

                                       radii[[j]] <- result

                                     }else{

                                       suppressMessages(result <- points %>%
                                                          sf::st_set_geometry(NULL) %>%
                                                          dplyr::inner_join(
                                                            points[i, ] %>%
                                                              sf::st_set_geometry(NULL) %>%
                                                              dplyr::mutate("r{radii_turf[j]}m_man_turf_pland" := 0)))

                                       radii[[j]] <- result

                                     }

                                     rm(circle, lyr_intersect, j)
                                   }

                                   # bind different radii by columns
                                   radii <- base::Reduce(merge, radii)

                                   # append to results output (includes geometry col)
                                   results <- results %>%
                                     dplyr::left_join(radii, by = uid_column)

                                   rm(radii)
                                 }


                                 # NAT VEG
                                 if(!is.null(layer_natveg)){
                                   radii <- list()

                                   for(j in seq_along(radii_natveg)){

                                     circle <- sf::st_buffer(points[i,],
                                                             dist = radii_natveg[j])

                                     # to get rid of warning message
                                     sf::st_agr(layer_natveg) = "constant"
                                     sf::st_agr(circle) = "constant"

                                     lyr_intersect <- layer_natveg %>%
                                       dplyr::select(.data$geometry) %>%
                                       sf::st_intersection(circle) %>%
                                       dplyr::mutate(area_m2 = .data$geometry %>% sf::st_area() %>%
                                                       units::set_units(NULL)) %>%
                                       sf::st_set_geometry(NULL)

                                     if(nrow(lyr_intersect) > 0){

                                       suppressMessages(result <- lyr_intersect %>% # natveg area
                                                          dplyr::group_by(.data[[uid_column]]) %>%
                                                          dplyr::summarise("r{radii_natveg[j]}m_man_natveg_pland" := (sum(.data$area_m2)/(pi*radii_natveg[j]^2))*100))

                                       radii[[j]] <- result

                                     }else{

                                       suppressMessages(result <- points %>%
                                                          sf::st_set_geometry(NULL) %>%
                                                          dplyr::inner_join(
                                                            points[i, ] %>%
                                                              sf::st_set_geometry(NULL) %>%
                                                              dplyr::mutate("r{radii_natveg[j]}m_man_natveg_pland" := 0)))

                                       radii[[j]] <- result

                                     }

                                     rm(circle, lyr_intersect, j)
                                   }

                                   # bind different radii by columns
                                   radii <- base::Reduce(merge, radii)

                                   # append to results output (includes geometry col)
                                   results <- results %>%
                                     dplyr::left_join(radii, by = uid_column)

                                   rm(radii)
                                 }


                                 # WATER
                                 if(!is.null(layer_water)){
                                   radii <- list()

                                   for(j in seq_along(radii_water)){

                                     circle <- sf::st_buffer(points[i,],
                                                             dist = radii_water[j])

                                     # to get rid of warning message
                                     sf::st_agr(layer_water) = "constant"
                                     sf::st_agr(circle) = "constant"

                                     lyr_intersect <- layer_water %>%
                                       dplyr::select(.data$geometry) %>%
                                       sf::st_intersection(circle) %>%
                                       dplyr::mutate(area_m2 = .data$geometry %>% st_area() %>%
                                                       units::set_units(NULL)) %>%
                                       sf::st_set_geometry(NULL)

                                     if(nrow(lyr_intersect) > 0){

                                       suppressMessages(result <- lyr_intersect %>% # water area
                                                          dplyr::group_by(.data[[uid_column]]) %>%
                                                          dplyr::summarise("r{radii_water[j]}m_man_water_pland" := (sum(.data$area_m2)/(pi*radii_water[j]^2))*100))

                                       radii[[j]] <- result

                                     }else{

                                       suppressMessages(result <- points %>%
                                                          sf::st_set_geometry(NULL) %>%
                                                          dplyr::inner_join(
                                                            points[i, ] %>%
                                                              sf::st_set_geometry(NULL) %>%
                                                              dplyr::mutate("r{radii_water[j]}m_man_water_pland" := 0)))

                                       radii[[j]] <- result

                                     }

                                     rm(circle, lyr_intersect, j)
                                   }

                                   # bind different radii by columns
                                   radii <- base::Reduce(merge, radii)

                                   # append to results output (includes geometry col)
                                   results <- results %>%
                                     dplyr::left_join(radii, by = uid_column)

                                   rm(radii)
                                 }


                                 # BUILDINGS
                                 if(!is.null(layer_buildings)){
                                   radii <- list()

                                   for(j in seq_along(radii_buildings)){

                                     circle <- sf::st_buffer(points[i,],
                                                             dist = radii_buildings[j])

                                     # to get rid of warning message
                                     sf::st_agr(layer_buildings) = "constant"
                                     sf::st_agr(circle) = "constant"

                                     lyr_intersect <- layer_buildings %>%
                                       dplyr::select(.data[[buildings_levels]], .data$geometry) %>%
                                       sf::st_intersection(circle) %>%
                                       dplyr::mutate(area_m2 = .data$geometry %>% sf::st_area() %>%
                                                units::set_units(NULL),
                                              GFA_m2 = units::set_units(.data$area_m2 * .data[[buildings_levels]], NULL)) %>%
                                       sf::st_set_geometry(NULL)

                                     if(nrow(lyr_intersect) > 0){

                                       suppressMessages(result <- lyr_intersect %>% # building area
                                                          dplyr::group_by(.data[[uid_column]]) %>%
                                                          dplyr::summarise("r{radii_buildings[j]}m_man_buildingArea_m2" := sum(.data$area_m2),
                                                                           "r{radii_buildings[j]}m_man_buildingGFA_m2" := sum(.data$GFA_m2)) %>%
                                                          dplyr::mutate("r{radii_buildings[j]}m_man_buildingAvgLvl" := .data[[glue::glue("r{radii_buildings[j]}m_man_buildingGFA_m2")]] / .data[[glue::glue("r{radii_buildings[j]}m_man_buildingArea_m2")]],
                                                                        "r{radii_buildings[j]}m_man_buildingFA_ratio" := .data[[glue::glue("r{radii_buildings[j]}m_man_buildingGFA_m2")]] / (pi*radii_buildings[j]^2)
                                                          ))

                                       radii[[j]] <- result

                                     }else{

                                       suppressMessages(result <- points %>%
                                                          sf::st_set_geometry(NULL) %>%
                                                          dplyr::inner_join(
                                                            points[i, ] %>%
                                                              sf::st_set_geometry(NULL) %>%
                                                              dplyr::mutate("r{radii_buildings[j]}m_man_buildingArea_m2" := 0,
                                                                            "r{radii_buildings[j]}m_man_buildingGFA_m2" := 0,
                                                                            "r{radii_buildings[j]}m_man_buildingAvgLvl" := 0,
                                                                            "r{radii_buildings[j]}m_man_buildingFA_ratio" := 0
                                                              )))

                                       radii[[j]] <- result

                                     }

                                     rm(circle, lyr_intersect, j)
                                   }

                                   # bind different radii by columns
                                   radii <- Reduce(merge, radii)

                                   # append to results output (includes geometry col)
                                   results <- results %>%
                                     left_join(radii, by = uid_column)

                                   rm(radii)
                                 }


                                 # ROADS
                                 if(!is.null(layer_roads)){
                                   radii <- list()

                                   for(j in seq_along(radii_roads)){

                                     circle <- st_buffer(points[i,],
                                                         dist = radii_roads[j])

                                     # to get rid of warning message
                                     st_agr(layer_roads) = "constant"
                                     st_agr(circle) = "constant"

                                     lyr_intersect <- layer_roads %>%
                                       dplyr::select(.data[[roads_lanes]], .data$geometry) %>%
                                       st_intersection(circle) %>%
                                       mutate(length_m = .data$geometry %>% sf::st_length() %>%
                                                units::set_units(NULL),
                                              lanelength_m = units::set_units(.data$length_m * .data[[roads_lanes]], NULL)) %>%
                                       st_set_geometry(NULL)

                                     if(nrow(lyr_intersect) > 0){

                                       suppressMessages(result <- lyr_intersect %>% # road area
                                                          group_by(.data[[uid_column]]) %>%
                                                          summarise("r{radii_roads[j]}m_man_lanelength_m" := sum(.data$lanelength_m),
                                                                    "r{radii_roads[j]}m_man_laneDensity" := sum(.data$lanelength_m) / (pi*radii_roads[j]^2)
                                                          ))

                                       radii[[j]] <- result

                                     }else{

                                       suppressMessages(result <- points %>%
                                                          sf::st_set_geometry(NULL) %>%
                                                          dplyr::inner_join(
                                                            points[i, ] %>%
                                                              sf::st_set_geometry(NULL) %>%
                                                              dplyr::mutate("r{radii_roads[j]}m_man_lanelength_m" := 0,
                                                                            "r{radii_roads[j]}m_man_laneDensity" := 0
                                                              )))

                                       radii[[j]] <- result

                                     }

                                     rm(circle, lyr_intersect, j)
                                   }

                                   # bind different radii by columns
                                   radii <- Reduce(merge, radii)

                                   # append to results output (includes geometry col)
                                   results <- results %>%
                                     left_join(radii, by = uid_column)

                                   rm(radii)
                                 }


                                 # print progress
                                 message(paste(Sys.time(),
                                                "Processed point", i, "out of", nrow(points), "\n"))

                                 return(results)
                               }

    # combine list as df
    output <- output %>% # overwrite
      dplyr::bind_rows()

    parallel::stopCluster(cl)
    rm(cl)

    return(output)

  }
