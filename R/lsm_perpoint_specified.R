#'Calculate landscape metrics at sampling points specified by name
#'
#'Summarise specified landscape metrics at sampling points, for classified raster object(s).
#'Multiple (list of) rasters may be classified, each corresponding
#'to a survey period in the dataset `points`. Calls the function `lsm_perpoint()` internally.
#'The character vector of predictor names include the specified buffer radii within which to summarise each metric.
#'Refer to `landscapemetrics::list_lsm()` for the full list of metric names and abbreviations.
#'
#'@param raster_list List of SpatRaster objects (`terra::rast()`). Each element is a classified land cover raster object to be analysed,
#'and its sequence (number) should correspond to the survey periods (an integer) present in `points`.
#'@param predictors_lsm Vector (character) of predictor variables to be calculated from the raster(s).
#'Naming format is formatted as `<radius in metres>_lsm_<metric>` (e.g. `r50m_lsm_pland`).
#'Refer to `landscapemetrics::list_lsm()` for the full list of metrics.
#'@param class_names Vector (character) of land cover class names to be used to identify the corresponding integer values in `class_values`.
#'@param class_values Vector of (integer) values of interest within the classified rasters in `raster_list`. Should not include the value `0`.
#'@param points Sampling points (sf object) representing the locations to calculate the metrics.
#'@param point_id Column name of the sampling point id within the `points` sf. Defaults to `"point_id"`.
#'@param period Column name of the survey period within the `points` sf. Defaults to `"period"`.
#'The column should contain integers that correspond to the number of elements in `raster_list`.
#'
#'@return A list containing the specified landscape metrics summarised at the sampling points provided.
#'Each element of the list contains results for a survey `period` (represented as integers in `points`).
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
lsm_perpoint_specified <- function(raster_list, predictors_lsm,
                                   class_names, class_values,
                                   points,
                                   point_id = "point_id",
                                   period = "period"){

  # Error checking ------------------
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_character(predictors_lsm, min.chars = 1, any.missing = FALSE, all.missing = FALSE, null.ok = FALSE, unique = TRUE, add = coll)
  checkmate::assert_list(raster_list, any.missing = FALSE, all.missing = FALSE, min.len = 1)

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
    dplyr::arrange(class, metric)


  # parallel processing
  # cl <- parallel::makeCluster(parallel::detectCores()[1]-1, outfile = "") # not to overload your computer
  # doParallel::registerDoParallel(cl)

  results_all <- list()
  for(i in seq_along(raster_list)){ # per period

    # results <- foreach(j = seq_len(nrow(input)), # parallel loop - can't work
    #                    .packages = c("tidyverse", "sf", "terra", "landscapemetrics", "checkmate")) %dopar% {

    results <- list()
    for(j in seq_len(nrow(input))){ # per predictor

      succeeded <- tryCatch({

        suppressMessages(result <-
                           lsm_perpoint(raster = raster_list[[i]],
                                        points = points %>%
                                          filter(.data[[period]] == i),
                                        buffer_sizes = as.numeric(input$radius[j]),
                                        class_names = input$class[j],
                                        class_values = input$value[j],
                                        point_id = point_id,
                                        period = period,
                                        what = paste0("lsm_c_", input$metric[j])))

        result <- purrr::map_dfr(result, ~as_tibble(., ), .id = "buffer") %>%
          tidyr::pivot_wider(id_cols = c(.data[[period]], .data[[point_id]]),
                      names_from = "buffer",
                      values_from = tidyselect::matches("lsm") | tidyselect::matches("osm"), # only pivot cols with these strings
                      names_glue = "r{buffer}m_{.value}") # edit col name

        message(paste0(Sys.time(), " Summarised at sampling points for ", j, "/", nrow(input), " predictor variables (period ", i,").\n"))


        # return(result)
        results[[j]] <- result
        rm(result)

        succeeded <- "success"

      }, error = function(e){

        cat(paste0(Sys.time(), " Error in summarising predictor variable ", j, "/", nrow(input), " - Returning empty list. Original warning message:"), sep = "\n")
        warning(e)
        return("error")
      })
      if(succeeded == "error"){
        results[[j]] <- list() # append to list empty object
      }
      rm(succeeded, j)
    }

    # combine list into df (by cols)
    if(!(rlang::is_empty(results) | all(sapply(results, is_empty)))){
      results <- results[lengths(results) > 1] %>% # remove empty elements
        purrr::reduce(dplyr::full_join, by = c(point_id, period))
      results_all[[i]] <- results

      message(paste0(Sys.time(), " Landscape predictors summarised for period ", i, "/", length(raster_list), "."))
    } else {
      warning(paste0(Sys.time(), " Landscape predictors not summarised for period ", i,"/", length(raster_list), "."))
    }
  }

  # parallel::stopCluster(cl)
  # rm(cl)

  return(results_all)
}
