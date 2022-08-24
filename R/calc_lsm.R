#'Calculate landscape metrics at point locations
#'
#'Calculate landscape metrics from a classified raster,
#'at specific point locations and buffer radii.
#'
#'@param raster SpatRaster object (`terra::rast()`). Classified land cover raster object to be analysed.
#'The number of layers should correspond to the number of survey periods present in `points`.
#'@param points Points locations (sf object) to calculate the metrics.
#'@param buffer_sizes Radius of circle (in mapunits) for each point location;
#'metrics will be calculated within the buffer area.
#'@param class_names Vector (character) of land cover class names to be used to identify the corresponding integer values in `class_values`.
#'@param class_values Vector of (integer) values of interest within `raster`. Should not include the value `0`.
#'@param landscape_name Character string for name of landscape. Used to label landscape-level metrics.
#'@param point_id Column name of the sampling point id within the `points` sf.
#'@param period Column name of the survey period within the `points` sf.
#'The column should contain integers that correspond to the number of layers in `raster`.
#'@param what Argument passed to `landscapemetrics::calculate_lsm()`.
#'Currently only supports class- and landscape-level metrics
#'@param level Argument passed to `landscapemetrics::calculate_lsm()`.
#'Currently only supports class- and landscape-level metrics.
#'@param ... Additional arguments (e.g. `type=`) passed to `landscapemetrics::calculate_lsm()`.
#'
#'@return A list containing the features/metrics calculated for `points`.
#'Each element in the list corresponds to a particular buffer size.
#'
#'@import checkmate
#'@import dplyr
#'@import sf
#'@importFrom rlang .data
#'@importFrom tidyr separate pivot_wider drop_na
#'@importFrom tibble deframe tibble
#'@importFrom terra res nlyr
#'@importFrom landscapemetrics sample_lsm
#'
#'@export
calc_lsm <- function(raster, points, buffer_sizes,
                     class_names = NULL,
                     class_values = NULL,
                     landscape_name = NULL,
                     point_id = "point_id", period = "period",
                     what = NULL,
                     level = NULL, ...){

  # Error checking ------------------
  # if(!methods::is(raster, "Raster")){
  #   stop("Error: \nInput 'raster' must be a Raster* Layer/Stack/Brick!")
  # }
  if(!all(sf::st_geometry_type(points) == "POINT")){
    stop("Error: \nInput 'points' must all be sf points!")
  }

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_character(class_names, min.chars = 1, any.missing = FALSE, all.missing = FALSE, null.ok = TRUE, len = length(class_values), unique = TRUE, add = coll)
  checkmate::assert_numeric(class_values, lower = 0, finite = TRUE, any.missing = FALSE, all.missing = FALSE, null.ok = TRUE, len = length(class_names), unique = TRUE, add = coll)
  checkmate::assert_numeric(buffer_sizes, lower = 0, finite = TRUE, any.missing = FALSE, add = coll)
  checkmate::assert_character(landscape_name, min.chars = 1, any.missing = FALSE, all.missing = FALSE, null.ok = TRUE, len = 1, add = coll)
  # checkmate::assert_subset(point_id, choices = colnames(points), empty.ok = FALSE, add = coll)
  # checkmate::assert_subset(period, choices = colnames(points), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(level, choices = c("class", "landscape"), empty.ok = TRUE, add = coll)

  checkmate::reportAssertions(coll)

  if(!checkmate::test_numeric(buffer_sizes, lower = min(terra::res(raster)),
                              finite = TRUE, any.missing = FALSE,
                              all.missing = FALSE, min.len = 1)){
    stop("All elements in buffer_size must be larger than the spatial (pixel) resolution of the input raster")
  }

  if(period %in% colnames(points)){
    if((terra::nlyr(raster) != length(unique(points[[period]]))) & any(points[[period]]%%1!=0)){
      stop("The number of layers in 'raster' should correspond to the unique number of survey periods (integers) present in 'points'.")
    }
  }


  # Calculations ------------------

  # parallel processing
  # cl <- parallel::makeCluster(parallel::detectCores()[1]-1, outfile = "") # not to overload your computer
  # doParallel::registerDoParallel(cl)
  # circles <- foreach::foreach(i = 1:length(buffer_sizes),
  #                             .packages = c("dplyr", "tidyr", "tibble", "raster", "sf", "landscapemetrics","rlang")) %dopar% {

  circles <- list()
  for(i in 1:length(buffer_sizes)){

    if(period %in% colnames(points)){
       lsm <- landscapemetrics::sample_lsm(raster, points,
                                           plot_id = paste0(points[[point_id]], "_period", points[[period]]), # combine point_id & period
                                           shape = "circle",
                                           size = buffer_sizes[i],
                                           all_classes = TRUE, # NA for classes not present in some sample plots

                                           # arguments passed to calculate_lsm()
                                           what = what,
                                           level = level,
                                           ...
                                           ) %>%

         tidyr::separate(.data$plot_id, c(point_id, period), sep = "_period") %>% # separate cols

         # remove results for irrelevant period (if > 1 layer)
         if(terra::nlyr(raster) > 1){
           lsm <- lsm %>%
             dplyr::filter(.data$layer == .data[[period]])
         }

       lsm <- lsm %>%
         dplyr::filter(!(level == "class" & class == 0) & !(level == "class" & is.na(class))) %>% # remove land cover class 0 or NA

         # remove useless metrics
         dplyr::filter(!.data$metric %in% c("pr", "prd", "rpr")) %>%
         dplyr::filter(!((.data$metric %in% c("ta")) & (level == "landscape"))) %>% # total landscape area
         dplyr::filter(!((.data$metric %in% c("lpi")) & (level == "landscape"))) %>% # largest patch in landscape

         # amend erroneous values
         dplyr::mutate(value = dplyr::case_when((.data$metric == "clumpy" & .data$value > 1) ~ 1,
                                                (.data$metric == "clumpy" & .data$value < -1 ~ -1),
                                                TRUE ~ .data$value)) %>%

         # new col for levels
         dplyr::mutate(levels = dplyr::case_when(!is.na(class) ~ tibble::deframe(tibble(class_values, class_names))[class], # class-lvl
                                                 is.na(class) ~ paste0(landscape_name, "_l"))) %>% # landscape-lvl (NULL if not provided!)

         tidyr::drop_na(levels) %>% # remove NA for classes not specified in arguments but present in raster

         # remove duplicated metrics at landscape lvl
         dplyr::filter(!duplicated(cbind(.data[[point_id]], .data[[period]], .data$metric, .data$value))) %>%



         tidyr::pivot_wider(id_cols = c(.data[[period]], .data[[point_id]], "percentage_inside"),
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


    # if period col not present in points
    } else {

      lsm <- landscapemetrics::sample_lsm(raster, points,
                                          plot_id = points[[point_id]],
                                          shape = "circle",
                                          size = buffer_sizes[i],
                                          all_classes = TRUE, # NA for classes not present in some sample plots

                                          # arguments passed to calculate_lsm()
                                          what = what,
                                          level = level,
                                          ...) %>%

        dplyr::rename(point_id =.data$plot_id) %>% # separate cols
        dplyr::filter(!(level == "class" & class == 0) & !(level == "class" & is.na(class))) %>% # remove land cover class 0 or NA

        # remove useless metrics
        dplyr::filter(!.data$metric %in% c("pr", "prd", "rpr")) %>%
        dplyr::filter(!((.data$metric %in% c("ta")) & (level == "landscape"))) %>% # total landscape area
        dplyr::filter(!((.data$metric %in% c("lpi")) & (level == "landscape"))) %>% # largest patch in landscape

        # new col for levels
        dplyr::mutate(levels = dplyr::case_when(!is.na(class) ~ tibble::deframe(tibble(class_values, class_names))[class], # class-lvl
                                                is.na(class) ~ paste0(landscape_name, "_l"))) %>% # landscape-lvl (NULL if not provided!)

        tidyr::drop_na(levels) %>% # remove NA for classes not specified in arguments but present in raster

        # remove duplicated metrics at landscape lvl
        dplyr::filter(!duplicated(cbind(.data[[point_id]], .data$metric, .data$value))) %>%



        tidyr::pivot_wider(id_cols = c(.data[[point_id]], "percentage_inside"),
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


    }


     message(paste0(Sys.time(), " Processed for ", i,"/", length(buffer_sizes),
                    " buffer sizes (", buffer_sizes[i], "m)"))

     circles[[i]] <- lsm

     gc()

   }

  names(circles) <- buffer_sizes

  # parallel::stopCluster(cl)
  # rm(cl)

  return(circles)
}

