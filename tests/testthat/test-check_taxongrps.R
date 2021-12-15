test_that("Test check_taxongrps()", {


  # CHECK OUTPUTS
  # level - area
  expected <- check_taxongrps(observations = fauna, level = "area")
  expect_s3_class(expected, "data.frame")

  # level - point
  expected <- check_taxongrps(observations = fauna, level = "point")
  expect_s3_class(expected, "data.frame")


  # CHECK INPUTS
  expect_error(check_taxongrps(observations = fauna)) # level not defined

})


