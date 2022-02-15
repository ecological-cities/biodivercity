#'Creates an empty grid with specified resolution for subsequent spatial visualisation of diversity patterns
#'
#'Creates an empty grid with specified resolution from a provided multipolygon or list of multipolygons.
#'
#'@param geom sf geometry object, typically one or more multipolygons.
#'@param buffer_dist minimum distance from boundaries where grid cell centres should be.
#'@param grid_dim dimension in metres for each grid cell in eventual grid.
#'
#'@return The dataframe `bound_grid` with three columns. The third column contains S3 point objects which are the grid cell centres.
#'
#'@import dplyr
#'@import tibble
#'@import sf
#'
#'@export
geom_to_grid <- function(geom,buffer_dist,grid_dim){
  bound_diff <- st_buffer(geom, dist = -buffer_dist)

  bound_grid <- bound_diff %>%
    st_make_grid(cellsize = grid_dim, # regular grid across entire region
                 what = "centers", square = T,
                 crs = st_crs(geom)) %>%
    st_intersection(bound_diff) %>% # subset to only within regions of interest
    st_as_sf() %>%
    st_join(bound_diff) # add back col info

  bound_grid <- bound_grid %>%
    group_by(sites) %>%
    mutate(id = row_number()) %>%
    mutate(site_id = paste0(sites, id)) %>%
    dplyr::select(sites, site_id, geometry) %>%
    ungroup()
  return(bound_grid)
}
