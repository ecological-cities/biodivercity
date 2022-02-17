#'Generate regular grid of spatial points within target area of interest
#'
#'Generate a regular grid of spatial points at a specified pixel resolution (i.e. density of points),
#'from provided polygon(s) or multipolygon(s). The points will be used in subsequent processing,
#'for example, to summarise landscape variables as model inputs, as well as to generate spatial predictions
#'across the target area. This function is a wrapper around the `sf::st_make_grid()` function with the argument `what = "centers"`.
#'
#'@param target_areas sf object(s) of type POLYGON or MULTIPOLYGON.
#'@param innerbuffer_m numeric value indicating the minimum distance between generated points and the boundaries of the target area.
#'@param pixelsize_m numeric value for output grid cell size (in metres).
#'
#'@return An sf dataframe with an additional `geometry` (`sfc`) column of type POINT.
#'
#'@importFrom sf st_buffer st_make_grid st_crs st_intersection st_as_sf st_join
#'@importFrom rlang .data
#'
#'@export
generate_grid <- function(target_areas,
                          innerbuffer_m,
                          pixelsize_m){

  bound_diff <- sf::st_buffer(target_areas, dist = -innerbuffer_m)

  bound_grid <- bound_diff %>%
    sf::st_make_grid(cellsize = c(pixelsize_m, pixelsize_m), # regular grid across entire region
                 what = "centers", square = T,
                 crs = sf::st_crs(target_areas)) %>%
    sf::st_intersection(bound_diff) %>% # subset to only within regions of interest
    sf::st_as_sf() %>%
    sf::st_join(bound_diff) # add back col info

  return(bound_grid)
}
