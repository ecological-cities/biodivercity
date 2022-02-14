#'Creates heatmap with specified resolution from a provided geometry object and site-env data.
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
heatmap_raster <- function(means,sds, mods, landscape, geom, buffer_dist, grid_dim, name){
  pixel_pred <-
    function(points,
             taxon_mods, #what if user only wants to use one model
             mod_env,
             means,sds) {

      #load grid_landscape then process for each column: if NA, replace with mean; else, scale the UGS values with mod_attr

      new.us <- mod_env %>%
        st_set_geometry(NULL)

      # need to replace diversity indices with NA values to 0

      new.rescale <- new.us %>%
        dplyr::select(site_id)

      for (i in seq_along(colnames(new.us))) {

        if(colnames(new.us)[i] %in% colnames(means)){
          var <- colnames(new.us)[i]
          j <- match(var,colnames(mean_vec))

          new.us <- new.us %>%
            mutate(!!sym(var) := replace_na(data = !!sym(var), replace = 0))

          scaling.temp <- new.us %>%
            dplyr::select(site_id, !!sym(var)) %>%
            mutate(!!sym(var) := (!!sym(var)-means[1,j])) %>%
            mutate(!!sym(var) := (!!sym(var)/sds[1,j]))

          new.rescale <- new.rescale %>%
            left_join(scaling.temp, by = "site_id")
        }
      }

      sr.pred_all<- AICcmodavg::modavgPred(taxon_mods, newdata=new.rescale,type="response") %>% as.data.frame()
      sr.pred <- sr.pred_all$mod.avg.pred

      points <- points %>%
        mutate(sr.pred = sr.pred) %>%
        dplyr::select(sites, site_id, sr.pred, geometry)

      rm(scaling.temp)

      return(points)


    }
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

  pix <- pixel_pred(points = bound_grid,
               taxon_mods = mods,
               mod_env = landscape,
               means=means, sds=sds)

  bound_gridlines <- bound_diff %>%
    st_make_grid(cellsize = grid_dim, # regular grid across entire region
                 what = "polygons", square = T,
                 crs = st_crs(geom))

  bound_gridlines <- bound_gridlines[unlist(st_intersects(bound_grid, bound_gridlines))] %>% # intersection with grid centers
    st_as_sf() %>%
    st_join(bound_diff)

  hm <- bound_gridlines %>%
    mutate(sr.pred = pix$sr.pred) %>%
    dplyr::select(sites, sr.pred, geometry)

  raster_template <- bound_gridlines %>%
    raster::raster(res = grid_dim, crs = raster::crs(bound_gridlines))

  heatmap <- terra::rasterize(terra::vect(hm), terra::rast(raster_template),
                              field = "sr.pred", crs = crs(bound_gridlines))

  names(heatmap) <- paste0(name)
  return(heatmap)
}
