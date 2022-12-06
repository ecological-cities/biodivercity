test_that("Test threshold_otsu()", {

  imagepath <- system.file("extdata", "ndvi_mosaic.tif", package="biodivercity")
  # imagepath <- system.file("ex/elev.tif", package="terra") # alternatively can use eg data from terra package


  # CHECK INPUTS
  testthat::expect_error(threshold_otsu(image = imagepath, range = "character")) # range shld be numeric
  testthat::expect_error(threshold_otsu(image = imagepath, range = 1)) # range shld be numeric vector of length 2
  testthat::expect_error(threshold_otsu(image = imagepath, levels = "")) # 'levels' shld be a integer


  # CHECK OUTPUTS
  # 'image' is filepath
  checkmate::expect_number(threshold_otsu(image = imagepath))

  # 'image' is SpatRaster object
  checkmate::expect_number(threshold_otsu(terra::rast(imagepath)))

})


