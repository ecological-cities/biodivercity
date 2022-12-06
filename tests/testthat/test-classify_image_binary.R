test_that("Test threshold_otsu()", {

  imagepath <- system.file("extdata", "ndvi_mosaic.tif", package="biodivercity", mustWork = TRUE)
  # imagepath <- system.file("ex/elev.tif", package="terra") # alternatively can use eg data from terra package


  # CHECK INPUTS
  testthat::expect_error(classify_image_binary(image = imagepath, threshold = "test")) # threshold shld be "otsu" or a number


  # CHECK OUTPUTS
  # 'image' is filepath
  expected <- classify_image_binary(image = imagepath)
  testthat::expect_true(attributes(expected)$class[1] == "SpatRaster")

  # 'image' is SpatRaster object
  expected <- classify_image_binary(image = terra::rast(imagepath))
  testthat::expect_true(attributes(expected)$class[1] == "SpatRaster")

})


