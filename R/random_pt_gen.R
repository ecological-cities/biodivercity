#'Randomly generate sampling points within supplied area
#'
#'Randomly generate fauna sampling points at a density of one point per specified area of supplied boundaries.
#'
#'@param boundaries Shapefile (polygon or multipolygon) of site boundaries for sampling.
#'@param area_per_pt Area component for point density in square meters (e.g. one point every 50 hectares, area_per_pt = 500000).
#'@param pt_buffer Buffer radius of survey points in meters. Points will be generated at this distance away from the boundary so as to not survey outside area of interest.
#'@param excess_modifier Numeric value of a multiplicative modifier for excess points to be generated (i.e. a value of 1.5 = 1.5 times the expected number of points will be generated). The default value is '1'.
#'@param forest Shapefile (polygon or multipolygon) of forest area for separate point generation.
#'@param retain Shapefile (point) of previous round of surveys to retain in the new set of survey points. There must exist a column `landcover` with either `Urban` or `Forest` as values.
#'@param retain_prop Value of 0 to 1 for proportion of points to retain from previous round of surveys.
#'
#'@return The sf object `sampling_points` containing the coordinates of generated points.
#'
#'@import dplyr
#'@importFrom sf st_read st_cast st_buffer st_difference st_area st_sample st_as_sf st_intersection st_combine st_zm
#'@importFrom glue glue
#'@importFrom rlang .data
#'@importFrom magrittr '%>%'
#'
#'@export
random_pt_gen <- function(boundaries, area_per_pt, pt_buffer, excess_modifier = 1,
    forest = NULL, retain = NULL, retain_prop = NULL) {

    suppressWarnings(bound_buffer <- sf::st_cast(boundaries, "LINESTRING") %>%
        sf::st_buffer(pt_buffer))
    suppressWarnings(boundaries <- sf::st_difference(boundaries, bound_buffer))
    rm(bound_buffer)

    # cat(sf::st_crs(boundaries))

    n <- ceiling(sf::st_area(boundaries)/area_per_pt) %>%
        as.numeric()
    points <- sf::st_sample(boundaries, ceiling(n * excess_modifier), type = "random") %>%
        sf::st_as_sf() %>%
        dplyr::mutate(class = "Urban") %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::mutate(id = paste0("U", id))

    cat("\n", glue::glue("Total number of points required: {n}."))
    cat("\n")

    if (!is.null(forest)) {
        forest <- forest %>%
            sf::st_union()
        boundaries_all <- boundaries
        suppressWarnings(boundaries <- sf::st_difference(boundaries_all, forest))
        suppressWarnings(forest <- sf::st_intersection(boundaries_all, forest))
        rm(boundaries_all)

        n <- ceiling(sf::st_area(boundaries)/area_per_pt) %>%
            as.numeric()
        n_forest <- ceiling(sf::st_area(forest)/area_per_pt) %>%
            as.numeric()

        points <- sf::st_sample(boundaries, ceiling(n * excess_modifier),
            type = "random") %>%
            sf::st_as_sf() %>%
            dplyr::mutate(class = "Urban") %>%
            dplyr::mutate(id = dplyr::row_number()) %>%
            dplyr::mutate(id = paste0("U", id))
        points_forest <- sf::st_sample(forest, ceiling(n_forest * excess_modifier),
            type = "random") %>%
            sf::st_as_sf() %>%
            dplyr::mutate(class = "Forest") %>%
            dplyr::mutate(id = dplyr::row_number()) %>%
            dplyr::mutate(id = paste0("N", id))

        cat("\n", glue::glue("Number of urban points required: {n}."))
        cat("\n", glue::glue("Number of forest points required: {n_forest}."))
        cat("\n")

        if (!is.null(retain) & !is.null(retain_prop)) {
            retain <- retain %>%
                sf::st_as_sf() %>%
                dplyr::rename(id = 1) %>%
                dplyr::select(id, .data$landcover, .data$geometry) %>%
                sf::st_zm(drop = TRUE, what = "ZM")
            retain_forest <- dplyr::filter(retain, .data$landcover == "Forest") %>%
                dplyr::select(-.data$landcover) %>%
                dplyr::mutate(status = "Old")
            retain <- retain %>%
                dplyr::select(-.data$landcover) %>%
                dplyr::mutate(status = "Old")

            retain_n <- retain_prop * n
            retain_n_forest <- retain_prop * n_forest

            points <- sf::st_sample(boundaries, ceiling(ceiling(n - retain_n) *
                excess_modifier), type = "random") %>%
                sf::st_as_sf() %>%
                dplyr::rename(geometry = x) %>%
                dplyr::mutate(id = dplyr::row_number()) %>%
                dplyr::mutate(id = paste0("U", id)) %>%
                dplyr::mutate(status = "New") %>%
                rbind(., retain[sample(1:nrow(retain), ceiling(retain_n)), ]) %>%
                dplyr::mutate(class = "Urban")
            points_forest <- sf::st_sample(forest, ceiling(ceiling(n_forest - retain_n_forest) *
                excess_modifier), type = "random") %>%
                sf::st_as_sf() %>%
                dplyr::rename(geometry = x) %>%
                dplyr::mutate(id = dplyr::row_number()) %>%
                dplyr::mutate(id = paste0("N", id)) %>%
                dplyr::mutate(status = "New") %>%
                rbind(., retain_forest[sample(1:nrow(retain_forest), ceiling(retain_n_forest)),
                  ]) %>%
                dplyr::mutate(class = "Forest")

            cat("\n", glue::glue("Number of urban points retained: {ceiling(retain_n)} (n = {n}, retain_prop = {retain_prop})."))
            cat("\n", glue::glue("Number of forest points retained: {ceiling(retain_n_forest)} (n = {n_forest}, retain_prop = {retain_prop})."))
            cat("\n", glue::glue("Number of new urban points generated: {ceiling((n-retain_n)*excess_modifier)} (n = {(n-retain_n)}, excess_modifier = {excess_modifier})."))
            cat("\n", glue::glue("Number of new forest points generated: {ceiling((n_forest-retain_n_forest)*excess_modifier)} (n = {(n_forest-retain_n_forest)}, excess_modifier = {excess_modifier})."),
                "\n")

            rm(retain_forest, retain_n, retain_n_forest)

        } else if (is.null(retain) & !is.null(retain_prop) | !is.null(retain) &
            is.null(retain_prop)) {
            stop("Either arguments 'retain' or 'retain_prop' is missing.")
        } else {
            cat("\n", glue::glue("Number of urban points generated: {ceiling(n*excess_modifier)} (n = {n}, excess_modifier = {excess_modifier})."))
            cat("\n", glue::glue("Number of forest points generated: {ceiling(n_forest*excess_modifier)} (n = {n_forest}, excess_modifier = {excess_modifier})."),
                "\n")
        }

        points <- rbind(points, points_forest)

        rm(n, n_forest, points_forest)
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
                dplyr::mutate(id = paste0("U", id)) %>%
                dplyr::mutate(status = "New") %>%
                rbind(., retain[sample(1:nrow(retain), retain_n), ]) %>%
                dplyr::mutate(class = "Urban") %>%
            cat("\n", glue::glue("Number of urban points retained: {retain_n} (n = {n}, retain_prop = {retain_prop})."))
            cat("\n", glue::glue("Number of new urban points generated: {ceiling((n-retain_n)*excess_modifier)} (n = {(n-retain_n)}, excess_modifier = {excess_modifier})."),
                "\n")

        } else if (is.null(retain) & !is.null(retain_prop) | !is.null(retain) &
            is.null(retain_prop)) {
            stop("Either arguments 'retain' or 'retain_prop' is missing.")
        } else {
            cat("\n", glue::glue("Number of urban points generated: {ceiling(n*excess_modifier)} (n = {n}, excess_modifier = {excess_modifier})."),
                "\n")
        }
    }

    return(points)
}
