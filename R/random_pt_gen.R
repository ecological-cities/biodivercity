#'Randomly generate sampling points within a supplied area
#'
#'Randomly generate sampling points at a specified density and buffer radius, within
#'supplied polygons. An excess of sampling points can be generated,
#'in case some of the generated points are unsuitable for surveys (e.g. within inaccessible areas).
#'If necessary, stratified random sampling can be performed to ensure sufficient representation
#'between sub-areas of interest. By supplying the `sub_areas` of interest,
#'sampling points will be stratified between the areas outside ('normal' areas) and within these 'sub-areas'.
#'
#'@param boundaries Shapefile (polygon or multipolygon) of site boundaries for sampling.
#'@param area_per_pt Area component for point density in square meters (e.g. one point every 50 hectares, area_per_pt = 500000).
#'@param pt_radius Buffer radius of survey points in meters.
#'Ensures that the generated points do not extend beyond the boundaries of the area of interest.
#'@param excess_modifier Numeric value of a multiplicative modifier for excess points to be generated (i.e. a value of 1.5 = 1.5 times the expected number of points will be generated). The default value is '1'.
#'@param sub_areas Shapefile (polygon or multipolygon) of sub-area of interest,
#'where sampling points will be generated separately (stratified random sampling).
#'@param retain Shapefile (point) of previous round of surveys to retain in the new set of survey points.
#'The column `type` must exist, with the value of either `Normal` or `Sub-area`.
#'@param retain_prop Value of 0 to 1 for proportion of points to retain from previous round of surveys.
#'
#'@return The sf object `sampling_points` containing the geographical coordinates of generated points.
#'
#'@import dplyr
#'@importFrom sf st_read st_cast st_buffer st_difference st_area st_sample st_as_sf st_intersection st_combine st_zm
#'@importFrom glue glue
#'@importFrom rlang .data
#'@importFrom magrittr '%>%'
#'
#'@export
random_pt_gen <- function(boundaries, area_per_pt, pt_radius, excess_modifier = 1,
    sub_areas = NULL, retain = NULL, retain_prop = NULL) {

    suppressWarnings(bound_buffer <- sf::st_cast(boundaries, "LINESTRING") %>%
        sf::st_buffer(pt_radius))
    suppressWarnings(boundaries <- sf::st_difference(boundaries, bound_buffer))
    rm(bound_buffer)

    # cat(sf::st_crs(boundaries))

    n <- ceiling(sf::st_area(boundaries)/area_per_pt) %>%
        as.numeric()
    points <- sf::st_sample(boundaries, ceiling(n * excess_modifier), type = "random") %>%
        sf::st_as_sf() %>%
        dplyr::mutate(type = "Normal") %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::mutate(id = paste0("normal", id))

    cat("\n", glue::glue("Total number of points required: {n}."))
    cat("\n")

    if (!is.null(sub_areas)) {
        sub_areas <- sub_areas %>%
            sf::st_union()
        boundaries_all <- boundaries
        suppressWarnings(boundaries <- sf::st_difference(boundaries_all, sub_areas))
        suppressWarnings(sub_areas <- sf::st_intersection(boundaries_all, sub_areas))
        rm(boundaries_all)

        n <- ceiling(sf::st_area(boundaries)/area_per_pt) %>%
            as.numeric()
        n_subarea <- ceiling(sf::st_area(sub_areas)/area_per_pt) %>%
            as.numeric()

        points <- sf::st_sample(boundaries, ceiling(n * excess_modifier),
            type = "random") %>%
            sf::st_as_sf() %>%
            dplyr::mutate(type = "Normal") %>%
            dplyr::mutate(id = dplyr::row_number()) %>%
            dplyr::mutate(id = paste0("normal", id))
        points_subarea <- sf::st_sample(sub_areas, ceiling(n_subarea * excess_modifier),
            type = "random") %>%
            sf::st_as_sf() %>%
            dplyr::mutate(type = "Sub-area") %>%
            dplyr::mutate(id = dplyr::row_number()) %>%
            dplyr::mutate(id = paste0("subarea", id))

        cat("\n", glue::glue("Number of points required within 'normal' areas: {n}."))
        cat("\n", glue::glue("Number of points required within 'sub-areas': {n_subarea}."))
        cat("\n")

        if (!is.null(retain) & !is.null(retain_prop)) {
            retain <- retain %>%
                sf::st_as_sf() %>%
                dplyr::rename(id = 1) %>%
                dplyr::select(id, .data$type, .data$geometry) %>%
                sf::st_zm(drop = TRUE, what = "ZM")
            retain_subarea <- dplyr::filter(retain, .data$type == "Sub-area") %>%
                dplyr::select(-.data$type) %>%
                dplyr::mutate(status = "Old")
            retain <- retain %>%
                dplyr::select(-.data$type) %>%
                dplyr::mutate(status = "Old")

            retain_n <- retain_prop * n
            retain_n_subarea <- retain_prop * n_subarea

            points <- sf::st_sample(boundaries, ceiling(ceiling(n - retain_n) *
                excess_modifier), type = "random") %>%
                sf::st_as_sf() %>%
                dplyr::rename(geometry = .data$x) %>%
                dplyr::mutate(id = dplyr::row_number()) %>%
                dplyr::mutate(id = paste0("normal", id)) %>%
                dplyr::mutate(status = "New") %>%
                rbind(., retain[sample(1:nrow(retain), ceiling(retain_n)), ]) %>%
                dplyr::mutate(type = "Normal")
            points_subarea <- sf::st_sample(sub_areas, ceiling(ceiling(n_subarea - retain_n_subarea) *
                excess_modifier), type = "random") %>%
                sf::st_as_sf() %>%
                dplyr::rename(geometry = .data$x) %>%
                dplyr::mutate(id = dplyr::row_number()) %>%
                dplyr::mutate(id = paste0("subarea", id)) %>%
                dplyr::mutate(status = "New") %>%
                rbind(., retain_subarea[sample(1:nrow(retain_subarea), ceiling(retain_n_subarea)),
                  ]) %>%
                dplyr::mutate(type = "Sub-area")

            cat("\n", glue::glue("Number of points within 'normal' areas that were retained: {ceiling(retain_n)} (n = {n}, retain_prop = {retain_prop})."))
            cat("\n", glue::glue("Number of points within 'sub-areas' that were retained: {ceiling(retain_n_subarea)} (n = {n_subarea}, retain_prop = {retain_prop})."))
            cat("\n", glue::glue("Number of new points within 'normal' areas that were generated: {ceiling((n-retain_n)*excess_modifier)} (n = {(n-retain_n)}, excess_modifier = {excess_modifier})."))
            cat("\n", glue::glue("Number of new points within 'sub-areas' that were generated: {ceiling((n_subarea-retain_n_subarea)*excess_modifier)} (n = {(n_subarea-retain_n_subarea)}, excess_modifier = {excess_modifier})."),
                "\n")

            rm(retain_subarea, retain_n, retain_n_subarea)

        } else if (is.null(retain) & !is.null(retain_prop) | !is.null(retain) &
            is.null(retain_prop)) {
            stop("Either arguments 'retain' or 'retain_prop' is missing.")
        } else {
            cat("\n", glue::glue("Number of points within 'normal' areas that were generated: {ceiling(n*excess_modifier)} (n = {n}, excess_modifier = {excess_modifier})."))
            cat("\n", glue::glue("Number of points within 'sub-areas' that were generated: {ceiling(n_subarea*excess_modifier)} (n = {n_subarea}, excess_modifier = {excess_modifier})."),
                "\n")
        }

        points <- rbind(points, points_subarea)

        rm(n, n_subarea, points_subarea)
    } else {

        if (!is.null(retain) & !is.null(retain_prop)) {
            retain <- retain %>%
                sf::st_as_sf() %>%
                dplyr::rename(id = 1) %>%
                dplyr::select(id, .data$geometry) %>%
                sf::st_zm(drop = TRUE, what = "ZM") %>%
                dplyr::mutate(status = "Old")
            retain_n <- retain_prop * n
            points <- sf::st_sample(boundaries, ceiling(ceiling(n - retain_n) *
                excess_modifier), type = "random") %>%
                sf::st_as_sf() %>%
                dplyr::rename(geometry = x) %>%
                dplyr::mutate(id = dplyr::row_number()) %>%
                dplyr::mutate(id = paste0("normal", id)) %>%
                dplyr::mutate(status = "New") %>%
                rbind(., retain[sample(1:nrow(retain), retain_n), ]) %>%
                dplyr::mutate(type = "Normal") %>%
            cat("\n", glue::glue("Number of points within 'normal' areas that were retained: {retain_n} (n = {n}, retain_prop = {retain_prop})."))
            cat("\n", glue::glue("Number of points within 'normal' areas that were generated: {ceiling((n-retain_n)*excess_modifier)} (n = {(n-retain_n)}, excess_modifier = {excess_modifier})."),
                "\n")

        } else if (is.null(retain) & !is.null(retain_prop) | !is.null(retain) &
            is.null(retain_prop)) {
            stop("Either arguments 'retain' or 'retain_prop' is missing.")
        } else {
            cat("\n", glue::glue("Number of points within 'normal' areas that were generated: {ceiling(n*excess_modifier)} (n = {n}, excess_modifier = {excess_modifier})."),
                "\n")
        }
    }

    return(points)
}
