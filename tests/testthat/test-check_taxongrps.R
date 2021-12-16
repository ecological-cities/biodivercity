test_that("Test check_taxongrps()", {

  # CHECK INPUTS
  expect_error(check_taxongrps(observations = animal_observations)) # level not defined
  expect_error(check_taxongrps(observations = animal_observations, level = area)) # level shld be character 'area' or 'point', not a var


  # CHECK OUTPUTS
  # level - area
  expected <- check_taxongrps(observations = animal_observations, level = "area")
  expect_s3_class(expected, "data.frame")

  # level - point
  expected <- check_taxongrps(observations = animal_observations, level = "point")
  expect_s3_class(expected, "data.frame")

})


