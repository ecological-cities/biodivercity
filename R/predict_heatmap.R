#'Generate continuous raster of spatial predictions
#'
#'@param models Model objects for a specific animal (taxon) group to be used for predictions
#'(output of `MuMIn::get.models()`).
#'@param recipe_data recipe_data recipe object (`recipes::recipe()`) containing information on how to
#'transform the new data to the appropriate format, prior to making predictions (e.g. scale and center variables).
#'@param points_topredict sf dataframe with `geometry` (`sfc`) column of type POINT,
#'as well as column names that correspond to landscape predictors within `models`.
#'@param pixelsize_m numeric value for output grid cell size (in metres). Must be the same
#'pixel resolution of the regular grid of spatial points `points_topredict`.
#'
#'@return A `terra::rast()` raster object with continuous values representing the
#'spatial predictions made across a regular grid (e.g. number of animal species).
#'
#'@importFrom dplyr mutate select
#'@importFrom sf st_buffer st_bbox
#'@importFrom recipes bake
#'@importFrom tidyselect all_of any_of
#'@importFrom lme4 ranef
#'@importFrom AICcmodavg modavgPred
#'@importFrom stars st_rasterize st_as_stars
#'@importFrom terra rasterize vect rast
#'@importFrom rlang .data
#'
#'@export
predict_heatmap <- function(models,
                           recipe_data,
                           points_topredict,
                           pixelsize_m){

  # convert grid points to grid squares
  grid_topredict <- sf::st_buffer(points_topredict,
                                  dist = pixelsize_m/2,
                                  endCapStyle = "SQUARE")


  # scale predictors based on original input data, unecessary columns will be removed
  data_topredict <- recipe_data %>% recipes::bake(grid_topredict)


  # extract predictors of interest from models
  randef <- lapply(models, lme4::ranef)
  randef_names <- lapply(randef, names) %>%
    unlist() %>% unname() %>% unique()
  rm(randef)

  fixedef_names <- lapply(models, function(x) names(x@frame)) %>%
    unlist() %>% unique()
  fixedef_names <- fixedef_names[!grepl(paste0("^sprich$"), fixedef_names)] # remove response var
  fixedef_names <- fixedef_names[!fixedef_names %in% randef_names] # remove rand effect


  # select cols in data_topredict that match response & predictors (all must be present)
  data_topredict <- data_topredict %>%
    dplyr::select(tidyselect::all_of(fixedef_names),
                  tidyselect::any_of(randef_names))
  # if random effect is absent after bake(), modavgPred uses the "average" randeffect for predictions
  rm(randef_names, fixedef_names)


  # make predictions
  avgpred <- AICcmodavg::modavgPred(models,
                                    newdata = data_topredict,
                                    type = "response")

  # append predictions to grid squares
  grid_topredict <- grid_topredict %>%
    dplyr::mutate(predicted = avgpred$mod.avg.pred)
  rm(data_topredict, avgpred)


  # rasterise the grid squares
  raster_template <- grid_topredict %>%
    stars::st_rasterize(stars::st_as_stars(sf::st_bbox(grid_topredict),
                                           nx = pixelsize_m, ny = pixelsize_m, values = NA_real_))

  output <- terra::rasterize(terra::vect(grid_topredict),
                              terra::rast(raster_template),
                              field = "predicted")
  rm(grid_topredict, raster_template)

  return(output)
}
