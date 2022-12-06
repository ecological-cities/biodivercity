#'Create mosaics of Sentinel-2 images downloaded via the sen2r package
#'
#'Create mosaics of downloaded sen2r images from the parent directory, as specified in the sen2r `path_out` argument.
#'The images used are those detected in the parent directory as of running the function.
#'The list of spectral indices are retrieved from `sen2r` and `library('terra')` is used to create the mosaics.
#'
#'@param parent_dir File path of parent directory for `sen2r` images, as specified in the sen2r `path_out` argument.
#'@param exclude Character vector of spectral indices to be excluded from the mosaic operation. 'CLD', 'SCL', 'RGB432B', and 'BOA' are excluded by default.
#'@param rm_outlier TRUE or FALSE to perform outlier removal. Threshold values are specific to spectral indices.
#'
#'@return The .tif raster `mosaic.tif` within the subdirectory of included indices.
#'
#'@importFrom terra rast mosaic writeRaster
#'
#'@export
mosaic_sen2r <- function(parent_dir, exclude = NULL, rm_outlier = TRUE) {

  # parent_dir is the directory path as specified in sen2r `path_out` argument

  # get indices for sen2r pkg version 1.5.0
  indices <- structure(c("ATSAVI", "AFRI1600", "AFRI2100", "ARI", "AVI", "ARVI",
                         "ARVI2", "BWDRVI", "BRI", "CARI", "Chlgreen", "CIgreen", "CIrededge",
                         "Chlred-edge", "CVI", "CI", "Datt1", "Datt4", "Datt6", "D678-500",
                         "D800-550", "D800-680", "D833-658", "GDVI", "DVIMSS", "EVI",
                         "EVI2", "EPI", "Fe2+", "Fe3+", "GVMI", "GARI", "GLI", "GNDVI",
                         "GOSAVI", "GSAVI", "GBNDVI", "GRNDVI", "IVI", "IPVI", "I", "LCI",
                         "MCARI-MTVI2", "MCARI-OSAVI", "MVI", "MGVI", "MNSI", "MSBI",
                         "MYVI", "mND680", "MCARI", "MCARI1", "MCARI2", "mNDVI", "mSR",
                         "MSR670", "MSRNir-Red", "MSAVI2", "MSAVIhyper", "MTVI1", "MTVI2",
                         "Norm G", "Norm NIR", "Norm R", "PPR", "PVR", "ND774-677", "GNDVIhyper",
                         "ND782-666", "ND790-670", "ND800-2170", "PSNDc2", "PSNDc1", "GNDVIhyper2",
                         "PSNDb1", "PSNDa1", "ND800-680", "NDII", "NDII2", "NDMI", "ND827-668",
                         "ND833-1649", "ND833-658", "SIWSI", "ND895-675", "NGRDI", "NDVI2",
                         "BNDVI", "MNDVI", "NDVI", "NDRE", "NBR", "RI", "NDSaI", "NDVI690-710",
                         "OSAVI", "PNDVI", "PVI", "Rededge1", "Rededge2", "RBNDVI", "REP",
                         "Rre", "RDVI", "SAVImir", "IF", "MSI2", "MSI", "TM5-TM7", "SR440-740",
                         "BGI", "SR520-670", "SR550-670", "DSWI-4", "SR550-800", "GI",
                         "SR560-658", "SR672-550", "SR672-708", "SR674-553", "SR675-555",
                         "SR675-700", "SR675-705", "SR700", "SR700-670", "SR710-670",
                         "SR735-710", "SR774-677", "SR800-2170", "PSSRc2", "PSSRc1", "SR800-550",
                         "PSSRb1", "RVI", "PSSRa1", "SR800-680", "SR801-550", "SR801-670",
                         "PBI", "SR833-1649", "SR833-658", "Datt2", "SR860-550", "SR860-708",
                         "RDI", "SRMIR-Red", "SRNir-700-715", "GRVI", "SRNIR-MIR", "DVI",
                         "RRI1", "IO", "RGR", "SRRed-NIR", "SB1580", "SB2100", "SB2130",
                         "SB2180", "SB2218", "SB2240", "SB2250", "SB2270", "SB2280", "SB460",
                         "BB3", "SR495", "SB550", "SB555", "SB655", "SB660", "SB670",
                         "SB675", "BB1", "SB700", "SB703", "SB705", "SB735", "SB801",
                         "SB850", "SB885", "SAVI", "SBL", "SAVI2", "SLAVI", "SQRT(IR-R)",
                         "SIPI1", "SIPI3", "SBI", "GVIMSS", "NSIMSS", "SBIMSS", "GVI",
                         "WET", "YVIMSS", "TCARI-OSAVI", "TCARI", "TSAVI", "TSAVI2", "TCIdx",
                         "VI700", "VARIgreen", "VARIrededge", "WDVI", "WDRVI", "NDFI",
                         "NDFI2", "NDSI", "NBR2", "MIRBI", "CSI", "CRred", "CRred-2A",
                         "CRred-2B", "CRred-0", "BDred", "BDred-2A", "BDred-2B", "BDred-0",
                         "CRred2", "CRred2-2A", "CRred2-2B", "CRred2-0", "NDWI", "NDWI2",
                         "NDVIre", "NDBI", "Rcc", "Gcc", "Bcc", "ExG"), pkg_version = structure(list(
                           c(1L, 5L, 0L)), class = c("package_version", "numeric_version"
                           )), creation_date = structure(1624787785, class = c("POSIXct",
                                                                               "POSIXt"), tzone = ""))
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
