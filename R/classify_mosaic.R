#'Binary classification of a spectral index mosaic image with a threshold value.
#'
#'Binary classification of a spectral index mosaic image with a threshold value.
#'If a threshold value is not specified, the Otsu's threshold can be calculated and used for classification.
#'
#'@param x Spectral index mosaic image (.tif) to be classified.
#'@param threshold_value Numeric value to determine absence or presence of elements according to spectral index.
#'@param otsu TRUE or FALSE to perform Otsu's thresholding.
#'@param output Output filename.
#'
#'@return The .tif raster `classified_<index>.tif` in the output folder.
#'
#'@import threshold_otsu
#'@import terra
#'
#'@export
classify_mosaic <- function(x, threshold_value = NULL, otsu = FALSE, output){

  if(is.null(threshold_value)){ #threshold_value is unspecified
    if(otsu != TRUE) { #no threshold is specified
      stop("Argument 'threshold_value' is missing and 'otsu' is FALSE by default.")
      } else { #calculate otsu's threshold
        # threshold <- vector()
        threshold <- threshold_otsu(x, levels = 256)
      }
  } else { #threshold_value is specified
    threshold <-  threshold_value
    if(otsu == TRUE) message("Threshold is set to 'threshold_value' even if 'otsu' is TRUE. Remove 'threshold_value' to calculate Otsu's threshold.")
  }

  raster <- terra::rast(x)
  classified <- raster # duplicate

  # absent
  classified[raster < threshold] <- 0

  # present
  classified[raster >= threshold] <- 1

  terra::writeRaster(classified,
                     output,
                     wopt = list(gdal=c("COMPRESS=LZW")), # reduce file size
                     overwrite = TRUE)

  rm(classified, raster, threshold)
  terra::tmpFiles(remove = TRUE) # remove temp files to clear memory
}

