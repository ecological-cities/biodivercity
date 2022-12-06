#'Binary classification of a single-band image based on a threshold value
#'
#'Classify a single-band image such as a spectral index or greyscale raster derived from satellite data.
#'If a threshold value is not specified, the Otsu's threshold can be calculated and used for classification.
#'
#'@param image Single band raster to be classified (`SpatRaster` object from `library(terra)`),
#'or a file path to the image to be imported (`terra::rast()` will be used).
#'@param threshold Either a numeric value for a manually-specified threshold,
#'or `"otsu"` to calculate the threshold via Otsu's method. Defaults to `"otsu"`.
#'@param range Numeric vector (of length 2) specifying the histogram range to be used
#'for Otsu's thresholding (only relevant if `threshold = "otsu"`). Defaults to the minimum and maximum values of the imported raster.
#'@param levels Number of histogram bins used to calculate the threshold value (only relevant if `threshold = "otsu"`).
#'Typically based on the bit depth of the image (e.g. 8-bit image has `2^8` = `256` levels).
#'Defaults to `256`.
#'@param file File path to export output raster (optional). Defaults to `NULL`.
#'@param ... Other arguments that may be supplied to `terra::writeRaster()`.
#'
#'@return The classified raster (`SpatRaster` object).
#'
#'@seealso
#' * [threshold_otsu()] Internal function used to calculate threshold value via Otsu's method.
#'
#'@importFrom checkmate makeAssertCollection test_vector assert_character test_true assert_true reportAssertions
#'@importFrom terra rast writeRaster
#'
#'@examples
#' \dontrun{
#'   ndvi_mosaic <- system.file("extdata", "ndvi_mosaic.tif",
#'                               package="biodivercity")
#'   terra::plot(ndvi_mosaic) # examine continuous raster
#'   ndvi_classified <- classify_image_binary(image = ndvi_mosaic)
#'   terra::plot(ndvi) # examine classified raster
#' }
#'
#'@export
classify_image_binary <- function(image, threshold = "otsu",
                                  range = NULL, levels = 256,
                                  file = NULL, ...){

  # Error checking ------------------

  coll <- checkmate::makeAssertCollection()

  # image
  if(checkmate::test_vector(image)){ # if file path
    checkmate::assert_character(image, len = 1, any.missing = FALSE, null.ok = FALSE, add = coll)
    image <- terra::rast(image) # # import image & overwrite var

  } else if(checkmate::test_true(attributes(image)$class[1] == "SpatRaster")) { # if terra rast object
    checkmate::assert_true(terra::nlyr(image) == 1, add = coll) # single band

  } else {
    stop("\'image\' needs to be either a (single-band) SpatRaster, or file path to the raster to be imported.")
  }

  # filepath for export
  checkmate::assert_character(file, null.ok = TRUE, len = 1)

  checkmate::reportAssertions(coll)


  # Calculations ------------------

  # otsu or manually-specified
  if(threshold == "otsu"){ # threshold_value is unspecified
    # threshold <- vector()
    threshold <- threshold_otsu(image, range = range,
                                levels = levels) # overwrite
  }


  if(is.numeric(threshold)){ # threshold now is numeric

    classified <- image # duplicate

    # absent
    classified[image < threshold] <- 0

    # present
    classified[image >= threshold] <- 1


    if(!is.null(file)){
      terra::writeRaster(x = classified, filename = file, ...)
    }

  } else {stop("Argument 'threshold' needs to be either \"otsu\" or a numeric value.")}


  rm(image, threshold)

  return(classified)
}

