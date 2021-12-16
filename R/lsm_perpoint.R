#'Calculate landscape metrics at sampling points
#'
#'
#'@param raster Classified land cover raster object to be analysed.
#'The number of layers should correspond to the number of survey rounds present in `points`.
#'@param points Sampling points (sf objects) for the calculation of landscape metrics.
#'@param buffer_sizes Radius of circles for sampling points (in mapunits);
#'landscape metrics will be calculated within the buffer area.
#'@param class_names Vector (character) of land cover class names to be used to identify the corresponding integer values in `class_values`.
#'@param class_values Vector of (integer) values of interest within `raster`. Should not include the value `0`.
#'@param landscape_name Character string for name of landscape. Used to label landscape-level metrics.
#'@param point_id Column name of the sampling point id within the `points` sf.
#'@param round Column name of the survey round within the `points` sf.
#'The column should contain integers that correspond to the number of layers in `raster`.
#'@param what Argument passed to `landscapemetrics::calculate_lsm()`.
#'Currently only supports class- and landscape-level metrics
#'@param level Argument passed to `landscapemetrics::calculate_lsm()`.
#'Currently only supports class- and landscape-level metrics.
#'@param ... Additional arguments (e.g. `type=`) passed to `landscapemetrics::calculate_lsm()`.
#'
#'@return A list containing the landscape metrics across the input `buffer_sizes`.
#'Each element in the list contains results for each `point_id` and `round`, for a particular buffer size.
#'
#'@import checkmate
#'@import dplyr
#'@import foreach
#'@import sf
#'@importFrom rlang .data
#'@importFrom methods is
#'@importFrom tidyr separate pivot_wider
#'@importFrom tibble deframe tibble
#'@importFrom raster res nlayers
#'@importFrom doParallel registerDoParallel
#'@importFrom landscapemetrics sample_lsm
#'
#'@export
lsm_perpoint <- function(raster, points, buffer_sizes,
                         class_names,
                         class_values,
                         landscape_name,
                         point_id = "point_id", round = "round",
                         what = NULL,
                         level = NULL, ...){

  # Error checking ------------------
  if(!methods::is(raster, "Raster")){
    stop("Error: \nInput 'raster' must be a Raster* Layer/Stack/Brick!")
  }
  if(!all(st_geometry_type(points) == "POINT")){
    stop("Error: \nInput 'points' must all be sf points!")
  }

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_character(class_names, min.chars = 1, any.missing = FALSE, all.missing = FALSE, len = length(class_values), unique = TRUE, add = coll)
  checkmate::assert_numeric(class_values, lower = 0, finite = TRUE, any.missing = FALSE, all.missing = FALSE, len = length(class_names), unique = TRUE, add = coll)
  checkmate::assert_character(landscape_name, min.chars = 1, any.missing = FALSE, all.missing = FALSE, len = 1, add = coll)
  checkmate::assert_subset(point_id, choices = colnames(points), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(round, choices = colnames(points), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(level, choices = c("class", "landscape"), empty.ok = TRUE, add = coll)

  checkmate::reportAssertions(coll)

  if(!checkmate::test_numeric(buffer_sizes, lower = min(raster::res(raster)),
                              finite = TRUE, any.missing = FALSE,
                              all.missing = FALSE, min.len = 1)){
    stop("All elements in buffer_size must be larger than the spatial (pixel) resolution of the input raster")
    }

  if((raster::nlayers(raster) != length(unique(points[[round]]))) & any(points[[round]]%%1!=0)){
    stop("The number of layers in 'raster' should correspond to the unique number of survey rounds (integers) present in 'points'.")
  }


  # Calculations ------------------

  # parallel processing
  cl <- parallel::makeCluster(parallel::detectCores()[1]-1, outfile = "") # not to overload your computer
  doParallel::registerDoParallel(cl)

  circles <- foreach::foreach(i = 1:length(buffer_sizes),
                              .packages = c("dplyr", "tidyr", "tibble", "raster", "sf", "landscapemetrics","rlang")) %dopar% {


                       lsm <- landscapemetrics::sample_lsm(raster, points,
                                                           plot_id = paste0(points[[point_id]], "_round", points[[round]]), # combine point_id & round
                                                           shape = "circle",
                                                           size = buffer_sizes[i],
                                                           all_classes = TRUE, # NA for classes not present in some sample plots

                                                           # arguments passed to calculate_lsm()
                                                           what = what,
                                                           level = level,
                                                           ...
                                                           ) %>%

                         tidyr::separate(.data$plot_id, c(point_id, round), sep = "_round") %>% # separate cols

                         dplyr::filter(.data$layer == round) %>% # remove results for irrelevant round
                         dplyr::filter(!(level == "class" & class == 0) & !(level == "class" & is.na(class))) %>% # remove land cover class 0 or NA

                         # remove useless metrics
                         dplyr::filter(!.data$metric %in% c("pr", "prd", "rpr")) %>%
                         dplyr::filter(!((.data$metric %in% c("ta")) & (level == "landscape"))) %>% # total landscape area
                         dplyr::filter(!((.data$metric %in% c("lpi")) & (level == "landscape"))) %>% # largest patch in landscape

                         # new col for levels
                         dplyr::mutate(levels = dplyr::case_when(!is.na(class) ~ tibble:deframe(tibble::tibble(class_values, class_names))[class], # class-lvl
                                                                 is.na(class) ~ paste0(landscape_name, "_l"))) %>% # landscape-lvl

                         # remove duplicated metrics at landscape lvl
                         dplyr::filter(!duplicated(cbind(.data[[point_id]], .data[[round]], .data$metric, .data$value))) %>%



                         tidyr::pivot_wider(id_cols = c(.data[[round]], .data[[point_id]], "percentage_inside"),
                                            names_from = c("levels", "metric"), values_from = "value",
                                            names_glue = "lsm_{levels}_{metric}") %>%

                         # change specific cols (metrics) from NA to 0 (ONLY if percentage_inside > 90)
                         dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("_area_mn") | # area (mean)
                                         dplyr::ends_with("ca") | # total class area
                                         dplyr::ends_with("_pland") | # class area % landscape

                                         dplyr::ends_with("_core_mn") | # core area (mean)
                                         dplyr::ends_with("_tca") | # total core area
                                         dplyr::ends_with("_dcore_mn") | # disjunct core area (mean)
                                         dplyr::ends_with("_cpland") | # core area % landscape

                                         dplyr::ends_with("_te") | # total edge
                                         dplyr::ends_with("_ed") | # edge density

                                         dplyr::ends_with("_np") | # no of patches
                                         dplyr::ends_with("_pd"), # patch density
                                       ~ifelse(is.na(.), 0, .)))


                       message(paste0(Sys.time(), " Processed for ", i,"/", length(buffer_sizes),
                                      " buffer sizes (", buffer_sizes[i], "m)"))

                       lsm

                     }

  names(circles) <- buffer_sizes

  parallel::stopCluster(cl)
  rm(cl)

  return(circles)
}

