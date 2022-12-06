test_that("Test filter_obs()", {

  # CHECK INPUTS
  testthat::expect_error(filter_observations(observations = animal_observations)) # survey_ref not provided
  testthat::expect_error(filter_observations(survey_ref = animal_surveys)) # observations not provided


  # CHECK OUTPUTS
  expected <- filter_observations(observations = animal_observations, survey_ref = animal_surveys,
                         specify_taxon = "Aves", specify_period = c("1","2")) # filter to birds in both periods 1 and 2
  testthat::expect_s3_class(expected, "data.frame")

})


