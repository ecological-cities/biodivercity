#'Calculate landscape metrics at sampling points specified by name
#'
#'Summarise specified landscape metrics at sampling points, for classified raster object. Calls the function `lsm_perpoint()` internally.
#'The character vector of predictor names include the specified buffer radii within which to summarise each metric.
#'Refer to `landscapemetrics::list_lsm()` for the full list of metric names and abbreviations.
#'
#'@param raster A classified SpatRaster object (`terra::rast()`). Pixels have integer values.
#'@param predictors_lsm Vector (character) of predictor variables to be calculated from the raster(s).
#'The naming format is `<radius in metres>_lsm_<class>_<metric>` (e.g. `r50m_lsm_veg_pland`).
#'Refer to `landscapemetrics::list_lsm()` for the full list of metrics.
#'@param class_names Vector (character) of land cover class names to be used to identify the corresponding integer values in `class_values`.
#'@param class_values Vector of (integer) values of interest within the classified rasters in `raster`. Should not include the value `0`.
#'@param points Sampling points (sf object) representing the locations to calculate the metrics.
#'@param point_id Column name of the sampling point id within the `points` sf. Defaults to `"point_id"`.
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
#'@importFrom tidyselect matches
#'
#'@export
lsm_perpoint_specified <- function(raster,
                                   class_names, class_values,
                                   predictors_lsm,
                                   points,
                                   point_id = "point_id"){

  # Error checking ------------------
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_character(predictors_lsm, min.chars = 1, any.missing = FALSE, all.missing = FALSE, null.ok = FALSE, unique = TRUE, add = coll)

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
                         lsm_perpoint(raster = raster,
                                      points = points,
                                      buffer_sizes = as.numeric(input$radius[i]),
                                      class_names = input$class[i],
                                      class_values = input$value[i],
                                      point_id = point_id,
                                      what = paste0("lsm_c_", input$metric[i])))

      result <- purrr::map_dfr(result, ~as_tibble(., ), .id = "buffer") %>%
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
