test_that("Test exclude_simulator()", {

  # filter animal observations to taxon of interest
  birds <- filter_obs(observations = animal_observations,
                      survey_ref = animal_surveys,
                      specify_taxon = "Aves")

  # convert animal observations to community matrix
  birds <- as.data.frame.matrix(xtabs(abundance ~ survey_id + species, data = birds))
  birds <- cbind(survey_id = rownames(birds), birds) # convert rownames to col

  set.seed(123)


  # CHECK INPUTS

  exclude_simulator(community_matrix = birds, survey_ref = animal_surveys, exclude_num = 10)

  expect_error(exclude_simulator(survey_ref = animal_surveys, exclude_num = 10)) # community_matrix absent
  expect_error(exclude_simulator(community_matrix = birds, exclude_num = 10)) # survey_ref absent
  expect_error(exclude_simulator(birds, animal_surveys)) # argument exclude_num not specified

  expect_data_frame(birds[ , -which(names(birds) %in% c("survey_id"))],
                    types = "numeric", # check if community matrix (numbers only)
                    any.missing = FALSE,
                    all.missing = FALSE,
                    info = "All cols in community_matrix except 'survey_id' should be numeric")


  # CHECK OUTPUTS
  # exclude surveys
  result <- exclude_simulator(birds, animal_surveys,
                              exclude_num = 15, exclude_level = "survey", rep = 5)
  expect_s3_class(result, "data.frame")

  # exclude points
  exclude_simulator(birds, animal_surveys,
                    exclude_num = 15, exclude_level = "point", rep = 5)
  expect_s3_class(result, "data.frame")

})


