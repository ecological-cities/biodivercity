#'Calculate pixel value threshold of an image via Otsu's method
#'
#'Calculate Otsu's threshold value that separates a greyscale image into two distinct classes.
#'The threshold value is determined by minimizing the combined intra-class variance.
#'`library(terra)` and `library(stars)` are used to perform out-of-memory operations.
#'
#'@param x filename to be imported using `terra::rast()`, or a SpatRaster object.
#'@param range Numeric vector (of length 2) specifying the histogram range to be used
#'for thresholding. Defaults to the minimum and maximum values of the imported raster.
#'@param levels Number of histogram bins used to calculate the threshold value,
#'typically based on the bit depth of the image (e.g. 8-bit image has `2^8` = `256` levels).
#'Defaults to `256`.
#'@param ... Optional arguments to be passed to `terra::rast()` when importing image.
#'
#'@return Otsu's threshold value for the image (a single number), which can subsequently be
#'used for image classification (e.g. convert greyscale to binary image).
#'
#'@references Otsu, N. (1979). A threshold selection method from gray-level histograms.
#'IEEE transactions on systems, man, and cybernetics, 9(1), 62-66.
#'Pau, G., Fuchs, F., Sklyar, O., Boutros, M., & Huber, W. (2010).
#'
#'EBImage—an R package for image processing with applications to cellular phenotypes.
#'Bioinformatics, 26(7), 979-981.
#'
#'
#'@import checkmate
#'@importFrom terra rast
#'@importFrom terra minmax
#'@importFrom stars st_as_stars
#'
#'@export
threshold_otsu <- function(x = NULL,
                           range = NULL,
                           levels = 256, ...){

  # import image
  if(is.character(x)){
    raster <- terra::rast(x, ...) # import as terra
  }else{
    raster <- x
  }



  # Error checking ------------------
  # range
  if(is.null(range)){
    range <- c(terra::minmax(raster)) # default to use min/max of raster values
  }else{ # do some checks
    checkmate::assert_numeric(range, len = 2)
  }

  # levels
  checkmate::assert_number(as.integer(levels), na.ok = FALSE, null.ok = FALSE, lower = 1)


  # Calculations ------------------

  raster <- stars::st_as_stars(raster, proxy = FALSE) # convert to stars object
  # names(NDVI_show) <- NULL

  # class(NDVI_show[[1]])
  # dim(NDVI_show[[1]])
  raster <- raster[[1]] # just get the array

  breaks = seq(floor(range[1]*1000)/1000,  # ceiling/floor with decimal places
               ceiling(range[2]*1000)/1000,
               length.out = levels + 1)
  hist_object = hist.default(raster, breaks = breaks, plot = FALSE)
  counts = as.double(hist_object$counts)
  mids = as.double(hist_object$mids)

  len = length(counts)
  w1 = cumsum(counts)
  w2 = w1[len] + counts - w1
  cm = counts * mids
  m1 = cumsum(cm)
  m2 = m1[len] + cm - m1
  var = w1 * w2 * (m2/w2 - m1/w1)^2
  maxi = which(var == max(var, na.rm = TRUE))

  result <- (mids[maxi[1]] + mids[maxi[length(maxi)]])/2

  return(result)

}

