#'Calculate pixel value threshold of a single-band image via Otsu's method
#'
#'Calculate Otsu's threshold value that separates a single-band (e.g. greyscale) image into two distinct classes.
#'The threshold value is determined by minimizing the combined intra-class variance.
#'`library(terra)` is used to perform out-of-memory operations.
#'
#'@param image Single-band raster to be classified (`SpatRaster` object from `library(terra)`),
#'or a file path to the image to be imported (`terra::rast()` will be used).
#'@param range Numeric vector (of length 2) specifying the histogram range to be used
#'for thresholding. Defaults to the minimum and maximum values of the imported raster.
#'@param levels Number of histogram bins used to calculate the threshold value,
#'typically based on the bit depth of the image (e.g. 8-bit image has `2^8` = `256` levels).
#'Defaults to `256`.
#'
#'@return Otsu's threshold value for the image (a single number), which can subsequently be
#'used for image classification (e.g. convert continuous raster to binary raster).
#'
#'@seealso
#' * [classify_image_binary()] to classify single-band image using Otsu's method.
#' Calls `threshold_otsu()` internally.
#'
#'@references Otsu, N. (1979). A threshold selection method from gray-level histograms.
#'IEEE transactions on systems, man, and cybernetics, 9(1), 62-66.
#'Pau, G., Fuchs, F., Sklyar, O., Boutros, M., & Huber, W. (2010).
#'
#'EBImage—an R package for image processing with applications to cellular phenotypes.
#'Bioinformatics, 26(7), 979-981.
#'
#'
#'@importFrom checkmate makeAssertCollection test_vector assert_character test_true assert_true assert_numeric assert_number reportAssertions
#'@importFrom graphics hist
#'@importFrom terra rast minmax nlyr values
#'
#'@examples
#' \dontrun{
#'   ndvi_mosaic <- system.file("extdata", "ndvi_mosaic.tif",
#'                               package="biodivercity")
#'   terra::plot(ndvi_mosaic) # examine raster data
#'   threshold_otsu(image = ndvi_mosaic)
#' }
#'
#'@export
threshold_otsu <- function(image,
                           range = NULL,
                           levels = 256){

  # Error checking ------------------

  coll <- checkmate::makeAssertCollection()

  # image
  if(checkmate::test_vector(image)){ # if file path
    checkmate::assert_character(image, len = 1, any.missing = FALSE, null.ok = FALSE)
    image <- terra::rast(image) # # import image & overwrite var

  } else if(checkmate::test_true(attributes(image)$class[1] == "SpatRaster")) { # if terra rast object
    checkmate::assert_true(terra::nlyr(image) == 1, add = coll) # single band

  } else {
    stop("\'image\' needs to be either a single-band SpatRaster, or file path to the raster to be imported.")
  }

  # range
  if(is.null(range)){
    range <- c(terra::minmax(image)) # default to use min/max of raster values
  }else{ # do some checks
    checkmate::assert_numeric(range, len = 2, add = coll)
  }

  # levels
  checkmate::assert_number(as.integer(levels), na.ok = FALSE, null.ok = FALSE, lower = 1, add = coll)


  checkmate::reportAssertions(coll)


  # Calculations ------------------

  breaks <- seq(floor(range[1]*1000)/1000,  # ceiling/floor with decimal places
                ceiling(range[2]*1000)/1000,
                length.out = levels + 1)
  hist_object <- graphics::hist(terra::values(image),  # just get the array
                                breaks = breaks, plot = FALSE)
  counts <- as.double(hist_object$counts)
  mids <- as.double(hist_object$mids)

  len <- length(counts)
  w1 <- cumsum(counts)
  w2 <- w1[len] + counts - w1
  cm <- counts * mids
  m1 <- cumsum(cm)
  m2 <- m1[len] + cm - m1
  var <- w1 * w2 * (m2/w2 - m1/w1)^2
  maxi <- which(var == max(var, na.rm = TRUE))

  result <- (mids[maxi[1]] + mids[maxi[length(maxi)]])/2

  return(result)

}

