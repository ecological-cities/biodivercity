#'Create mosaics of Sentinel-2 images downloaded via the sen2r package
#'
#'Create mosaics of downloaded sen2r images from the parent directory, as specified in sen2r's `path_out` argument.
#'The images used are those detected in the parent directory as of running the function.
#'The list of spectral indices are retrieved from `sen2r` and `library('terra')` is used to create the mosaics.
#'
#'@param parent_dir File path of parent directory for `sen2r` images, as specified in sen2r's `path_out` argument.
#'@param exclude Character vector of spectral indices to be excluded from the mosaic operation. 'CLD', 'SCL', 'RGB432B', and 'BOA' are excluded by default.
#'@param rm_outlier TRUE or FALSE to perform outlier removal. Threshold values are specific to spectral indices.
#'
#'@return The .tif raster `mosaic.tif` within the subdirectory of included indices.
#'
#'@import sen2r
#'@importFrom terra rast mosaic writeRaster
#'
#'@export
mosaic_sen2r <- function(parent_dir, exclude = NULL, rm_outlier = TRUE) {
    # parent_dir is the directory path as specified in sen2r
    # `path_out` argument

    indices <- sen2r::list_indices("name", all = TRUE)
    nonspectral <- c("TOA", "BOA", "SCL", "CLD", "SNW", "WVP", "AOT")  #need to figure out how to detect RGB_ products

    for (i in seq_along(exclude)) {
        if (!exclude[i] %in% indices & !exclude[i] %in% nonspectral)
            stop("Argument 'exclude' does not contain any sen2r output formats.")
    }

    directories <- list.dirs(parent_dir, recursive = FALSE)
    directories <- directories[!basename(directories) %in% nonspectral]  # rm directories not to be mosaicked
    directories <- directories[!basename(directories) %in% exclude]
    directories <- directories[!grepl("^RGB", basename(directories))]  #temp code to remove RGB_ products

    # loop to process/export mosaic for each subdirectory

    for (i in 1:length(directories)) {
        # normal loop

        # for each spectral index, import images as rasterStack
        filepaths <- dir(path = directories[i], full.names = TRUE, pattern = "*.tif$")
        filepaths <- filepaths[!grepl("mosaic", filepaths)]  # exclude previously generated/processed mosaics

        images <- lapply(filepaths, terra::rast)  # import as list

        # don't run these for non-spectral indices
        images <- lapply(images, function(x) x/10000)  # re-scale

        # remove outliers (for specific spectral indices)
        if (rm_outlier == TRUE) {
            if (basename(directories[i]) %in% c("NDVI", "NDRE", "NDVIre",
                "GNDVI", "EVI", "ARVI", "SAVI", "OSAVI", "MSAVI2", "MTVI2",
                "TCIdx", "MCARI", "NDWI", "NDWI2", "NDFI", "NDFI2", "NDII",
                "NDBI")) {
                for (j in 1:length(images)) {
                  images[[j]][images[[j]] < -1] <- NA
                  images[[j]][images[[j]] > 1] <- NA
                }

            } else if (basename(directories[i]) %in% c("ARI")) {
                for (j in 1:length(images)) {
                  images[[j]][images[[j]] < 0] <- NA
                  images[[j]][images[[j]] > 0.2] <- NA
                }

            } else if (basename(directories[i]) %in% c("SIPI1")) {
                for (j in 1:length(images)) {
                  images[[j]][images[[j]] < 0] <- NA
                  images[[j]][images[[j]] > 2] <- NA
                }

            } else if (basename(directories[i]) %in% c("MSI")) {
                for (j in 1:length(images)) {
                  images[[j]][images[[j]] < 0] <- NA
                }

            } else if (basename(directories[i]) %in% c("TCARI-OSAVI")) {
                for (j in 1:length(images)) {
                  images[[j]][images[[j]] < 0] <- NA
                  images[[j]][images[[j]] > 1] <- NA
                }
            }
        }

        # mosaic images

        mosaic <- do.call(terra::mosaic, c(images, list(fun = "mean")))
        terra::writeRaster(mosaic, paste0(directories[i], "/mosaic.tif"),
            wopt = list(gdal = c("COMPRESS=LZW")), overwrite = TRUE)

        cat(paste0(Sys.time(), " Exported mosaic for: "), filepaths, append = TRUE)

        rm(mosaic, images, filepaths, i, j)
        gc()
    }

    rm(directories)
}
