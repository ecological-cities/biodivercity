#'Randomly generate fauna sampling points within supplied area
#'
#'Randomly generate fauna sampling points at a density of one point per specified area of supplied boundaries.
#'
#'@param x Shapefile (polygon or multipolygon) of site boundaries for sampling.
#'@param area_per_pt Area component for point density in square meters (e.g. one point every 50 hectares, area_per_pt = 500000)
#'@param pt_buffer Buffer radius of point surveys in meters so as to not survey outside area of interest.
#'@param excess_modifier Numeric value of a multiplicative modifier for excess points to be generated (i.e. a value of 1.5 = 1.5 times the expected number of points will be generated). The default value is '1'.
#'@param natveg Shapefile (polygon or multipolygon) of natural vegetation area for separate point generation.
#'@param retain Shapefile (point) of previous round of surveys to retain in the new set of survey points.
#'@param retain_prop Value of 0 to 1 for proportion of points to retain from previous round of surveys.
#'
#'@return The sf object `sampling_points` containing the coordinates of generated points.
#'
#'@import sf
#'@import dplyr
#'@import glue
#'@importFrom magrittr "%>%"
#'
#'@export
random_pt_gen <- function(x, area_per_pt, pt_buffer, excess_modifier = 1, natveg = NULL, retain = NULL, retain_prop = NULL){

  boundaries <- sf::st_read(x)
  bound_buffer <- sf::st_cast(boundaries, "LINESTRING") %>%
    sf::st_buffer(pt_buffer)
  boundaries <-  sf::st_difference(boundaries, bound_buffer)
  rm(bound_buffer)

  # cat(sf::st_crs(boundaries))

  n <- ceiling(sf::st_area(boundaries)/area_per_pt) %>%
    as.numeric()
  points <- sf::st_sample(boundaries, ceiling(n*excess_modifier), type = "random") %>%
    sf::st_as_sf() %>%
    dplyr::mutate(class = "Urban") %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::mutate(id = paste0("U", id))

  cat("\n", glue::glue("Total number of points required: {n}."))
  cat("\n")

  if(!is.null(natveg)){
    natveg <- sf::st_read(natveg) %>%
      sf::st_combine()
    boundaries_all <- boundaries
    boundaries <- sf::st_difference(boundaries_all, natveg)
    natveg <- sf::st_intersection(boundaries_all, natveg)
    rm(boundaries_all)

    n <- ceiling(sf::st_area(boundaries)/area_per_pt) %>%
      as.numeric()
    n_NV <- ceiling(sf::st_area(natveg)/area_per_pt) %>%
      as.numeric()

    points <- sf::st_sample(boundaries, ceiling(n*excess_modifier), type = "random") %>%
      sf::st_as_sf() %>%
      dplyr::mutate(class = "Urban") %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      dplyr::mutate(id = paste0("U", id))
    points_NV <- sf::st_sample(natveg, ceiling(n_NV*excess_modifier), type = "random") %>%
      sf::st_as_sf() %>%
      dplyr::mutate(class = "Natveg") %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      dplyr::mutate(id = paste0("N", id))

    cat("\n", glue::glue("Number of urban points required: {n}."))
    cat("\n", glue::glue("Number of natural vegetation points required: {n_NV}."))
    cat("\n")

    if(!is.null(retain) & !is.null(retain_prop)){
      retain <- sf::st_read(retain) %>%
        sf::st_as_sf() %>%
        dplyr::rename(id = 1) %>%
        dplyr::select(id, geometry) %>%
        sf::st_zm(drop = TRUE, what = "ZM")
      retain_NV <- retain[grepl("^WLb", retain$id),] #update!

      retain_n <- retain_prop*n
      retain_n_NV <- retain_prop*n_NV

      points <- sf::st_sample(boundaries, ceiling((n-retain_n)*excess_modifier), type = "random") %>%
        sf::st_as_sf() %>%
        dplyr::rename(geometry = x) %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::mutate(id = paste0("U", id)) %>%
        rbind(., retain[sample(1:nrow(retain), retain_n),]) %>%
        dplyr::mutate(class = "Urban")
      points_NV <- sf::st_sample(natveg, ceiling((n_NV-retain_n_NV)*excess_modifier), type = "random") %>%
        sf::st_as_sf() %>%
        dplyr::rename(geometry = x) %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::mutate(id = paste0("N", id)) %>%
        rbind(., retain_NV[sample(1:nrow(retain_NV), retain_n_NV),]) %>%
        dplyr::mutate(class = "Natveg")

      cat("\n", glue::glue("Number of urban points retained: {retain_n} (n = {n}, retain_prop = {retain_prop})."))
      cat("\n", glue::glue("Number of natural vegetation points retained: {retain_n_NV} (n = {n_NV}, retain_prop = {retain_prop})."))
      cat("\n", glue::glue("Number of new urban points generated: {ceiling((n-retain_n)*excess_modifier)} (n = {(n-retain_n)}, excess_modifier = {excess_modifier})."))
      cat("\n", glue::glue("Number of new natural vegetation points generated: {ceiling((n_NV-retain_n_NV)*excess_modifier)} (n = {(n_NV-retain_n_NV)}, excess_modifier = {excess_modifier})."), "\n")

      rm(retain_NV, retain_n, retain_n_NV)

    } else if(is.null(retain) & !is.null(retain_prop) | !is.null(retain) & is.null(retain_prop)) {
      stop("Either arguments 'retain' or 'retain_prop' is missing.")
    } else {
      cat("\n", glue::glue("Number of urban points generated: {ceiling(n*excess_modifier)} (n = {n}, excess_modifier = {excess_modifier})."))
      cat("\n", glue::glue("Number of natural vegetation points generated: {ceiling(n_NV*excess_modifier)} (n = {n_NV}, excess_modifier = {excess_modifier})."), "\n")
    }

    points <- rbind(points, points_NV)

    rm(n, n_NV, points_NV)
  } else {

    if(!is.null(retain) & !is.null(retain_prop)){
      retain <- sf::st_read(retain) %>%
        sf::st_as_sf() %>%
        dplyr::rename(id = 1) %>%
        dplyr::select(id, geometry) %>%
        sf::st_zm(drop = TRUE, what = "ZM")
      retain_n <- retain_prop*n
      points <- sf::st_sample(boundaries, ceiling((n-retain_n)*excess_modifier), type = "random") %>%
        sf::st_as_sf() %>%
        dplyr::rename(geometry = x) %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::mutate(id = paste0("U", id)) %>%
        rbind(., retain[sample(1:nrow(retain), retain_n),]) %>%
        dplyr::mutate(class = "Urban") %>%

      cat("\n", glue::glue("Number of urban points retained: {retain_n} (n = {n}, retain_prop = {retain_prop})."))
      cat("\n", glue::glue("Number of new urban points generated: {ceiling((n-retain_n)*excess_modifier)} (n = {(n-retain_n)}, excess_modifier = {excess_modifier})."), "\n")

    } else if(is.null(retain) & !is.null(retain_prop) | !is.null(retain) & is.null(retain_prop)) {
      stop("Either arguments 'retain' or 'retain_prop' is missing.")
    } else {
      cat("\n", glue::glue("Number of urban points generated: {ceiling(n*excess_modifier)} (n = {n}, excess_modifier = {excess_modifier})."), "\n")
    }
  }

  return(points)
}
