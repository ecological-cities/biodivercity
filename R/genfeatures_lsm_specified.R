#'Generate specified landscape metrics at point locations
#'
#'Generate specified landscape metrics at point locations, for classified raster object. Calls the function `genfeatures_lsm()` internally.
#'The character vector of predictor names include the specified buffer radii within which to summarise each metric.
#'Refer to `landscapemetrics::list_lsm()` for the full list of metric names and abbreviations.
#'
#'@param raster A classified SpatRaster object (`terra::rast()`). Pixels have integer values.
#'@param predictors_lsm Vector (character) of predictor variables to be calculated from the raster(s).
#'The naming format is `r<radius of point buffer in metres>m_lsm_<class>_<metric>` (e.g. `r50m_lsm_veg_pland`).
#'Refer to `landscapemetrics::list_lsm()` for the full list of metrics.
#'@param class_names Vector (character) of land cover class names to be used to identify the corresponding integer values in `class_values`.
#'@param class_values Vector of (integer) values of interest within the classified rasters in `raster`. Should not include the value `0`.
#'@param points Points locations (sf object) to calculate the metrics.
#'@param na_threshold Value for calculated predictor will be `NA` if the percentage of raster data
#'within the respective point buffer areas is less than this value (`0` to `100`).
#'@param point_id Column name of the sampling point id within the `points` sf. Defaults to `"point_id"`.
#'@param ... Arguments passed on to `genfeatures_lsm()`
#'
#'@return The `points` object including new columns for the variables specified in `predictors_osm`.
#'
#'@import checkmate
#'@import dplyr
#'@import sf
#'@importFrom rlang .data is_empty
#'@importFrom stringr str_extract
#'@importFrom tidyr pivot_wider drop_na
#'@importFrom tibble deframe tibble
#'@importFrom purrr map_dfr reduce
#'@importFrom tidyselect matches starts_with
#'
#'@export
genfeatures_lsm_specified <- function(raster,
                                   class_names, class_values,
                                   predictors_lsm,
                                   points,
                                   na_threshold = 90,
                                   point_id = "point_id", ...){

  # Error checking ------------------
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_character(predictors_lsm, min.chars = 1, any.missing = FALSE, all.missing = FALSE, null.ok = FALSE, unique = TRUE, add = coll)
  checkmate::assert_numeric(na_threshold, lower = 0, upper = 100, min.len = 1, max.len = 1, any.missing = FALSE, null.ok = FALSE, add = coll)

  checkmate::reportAssertions(coll)


  # Calculations ------------------

  input <- data.frame(class = predictors_lsm %>%
                        stringr::str_extract("(?<=lsm_)[^_]+"), # get class
                      metric = predictors_lsm %>%
                        stringr::str_extract("(?<=lsm_).*") %>% # get class + metric
                        stringr::str_extract("(?<=_).*"), # get metric (after underscore)
                      radius = predictors_lsm %>%
                        stringr::str_extract("(?<=^r)\\d+")) %>%
    tidyr::drop_na() %>%

    dplyr::mutate(value = dplyr::case_when(!is.na(class) ~ tibble::deframe(tibble(class_names, class_values))[class])) %>% # assign value to class name
    dplyr::arrange(.data$class, .data$metric)


  for(i in seq_len(nrow(input))){ # per predictor

    succeeded <- tryCatch({

      suppressMessages(result <-
                         genfeatures_lsm(raster = raster,
                                      points = points,
                                      buffer_sizes = as.numeric(input$radius[i]),
                                      class_names = input$class[i],
                                      class_values = input$value[i],
                                      point_id = point_id,
                                      what = paste0("lsm_c_", input$metric[i]), ...))

      result <- purrr::map_dfr(result, ~as_tibble(., ), .id = "buffer") %>%

        # make NA if percentage_inside < specified amount
        dplyr::mutate(dplyr::across(.cols = tidyselect::starts_with("lsm_"),
                                    ~ifelse(percentage_inside < na_threshold, NA, .))) %>%
        # dplyr::select(-percentage_inside) %>%

        tidyr::pivot_wider(id_cols = c(.data[[point_id]]),
                    names_from = "buffer",
                    values_from = tidyselect::matches("lsm") | tidyselect::matches("osm"), # only pivot cols with these strings
                    names_glue = "r{buffer}m_{.value}")

      message(paste0(Sys.time(), " Summarised at sampling points for ", i, "/", nrow(input), " predictor variables.\n"))


      points <- points %>%
        left_join(result, by = point_id)
      rm(result)

      succeeded <- "success"

    }, error = function(e){

      warning(paste0(Sys.time(), " Error in summarising predictor variable ", i, "/", nrow(input), " - Returning NAs. Original warning message:"), sep = "\n")
      warning(e)
      return("error")
    })
    if(succeeded == "error"){

      # append NAs
      points <- points %>%
           dplyr::mutate("r{input$radius[i]}m_lsm_{input$class[i]}_{input$metric[i]}" := NA)
    }
    rm(succeeded, i)
  }

  return(points)
}
