test_that("Test check_taxongrps()", {

  # CHECK INPUTS
  expect_error(check_taxongrps(observations = fauna)) # level not defined
  expect_error(check_taxongrps(observations = fauna, level = area)) # level shld be character string 'area' or 'point', not a var


  # CHECK OUTPUTS
  # level - area
  expected <- check_taxongrps(observations = fauna, level = "area")
  expect_s3_class(expected, "data.frame")

  # level - point
  expected <- check_taxongrps(observations = fauna, level = "point")
  expect_s3_class(expected, "data.frame")

})


