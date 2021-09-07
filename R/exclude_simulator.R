#'Simulate multiple scenarios with randomly excluded surveys
#'
#'Randomly exclude either surveys or sampling points (and their associated surveys) from a
#'species community matrix. Each column of the community matrix should represent a unique species,
#'while each row represents a single unique survey (for a particular sampling point in a town,
#'as well as survey round and cycle).
#'
#'@param community Dataframe of the species community matrix (one survey per row).
#'It should include columns for `survey_id`, `point_id`, `town`, and `cycle`.
#'@param survey_ref Dataframe of all surveys conducted. Values in the column
#'  `survey_id` should correspond to those in the `community` data.
#'@param exclude_num Number of surveys to exclude. Value should be a single integer.
#'@param exclude_level Exclude randomly based on one of two levels: `"survey"` or `"point"`.
#'@param specify_towns Specify the `town`(s) to randomly exclude surveys from.
#'Defaults to `NULL`, which excludes surveys randomly across all towns.
#'@param specify_points Specify the `point_id`(s) of sampling points to randomly exclude surveys from.
#'Defaults to `NULL`, which excludes surveys randomly across all points.
#'@param specify_rounds Specify the survey `round` to exclude surveys from. Defaults to `NULL`,
#'which excludes surveys randomly across all rounds.
#'@param specify_cycles Specify the survey `cycle`(s) to exclude surveys from. Defaults to `NULL`,
#'which excludes surveys randomly across all cycles.
#'@param town_ignore Ignore specified `town`(s) in `community` data from analysis and results. Defaults to `NULL`,
#'which includes all towns in the simulation.
#'@param round_ignore Ignore specified `round` in `community` data from analysis and results. Defaults to `NULL`,
#'which includes all rounds in the simulation.
#'@param cycle_ignore Ignore specified `cycles`s in `community` data from analysis and results. Defaults to `NULL`,
#'which includes all cycles in the simulation.
#'@param survey_id Column name of the unique identifier for each survey in `community` and
#'`survey_ref`. Defaults to `survey_id`.
#'@param point_id Column name of the unique identifier for each sampling point in `survey_ref`.
#'Defaults to `survey_id`.
#'@param town Column name of the town specified in `survey_ref`.
#'Defaults to `town`.
#'@param round Column name of the survey round specified in `survey_ref`.
#'Defaults to `round`.
#'@param cycle Column name of the survey cycle specified in `survey_ref`.
#'Defaults to `cycle`.
#'@param rep Specify number of repetitions (scenarios) of randomly excluded survey data.
#'Defaults to `100`.
#'
#'@return A dataframe containing the count of points with each species for the full dataset,
#'as well as for each repetition scenario with randomly excluded data.
#'
#'@import checkmate
#'@import dplyr
#'@importFrom tidyr pivot_longer
#'@importFrom rlang .data
#'
#'@export
exclude_simulator <- function(community, survey_ref,
                              exclude_level = "survey",
                              exclude_num,
                              specify_towns = NULL, specify_points = NULL, specify_rounds = NULL, specify_cycles = NULL,
                              town_ignore = NULL, round_ignore = NULL, cycle_ignore = NULL,
                              survey_id = "survey_id", point_id = "point_id",
                              town = "town", round = "round", cycle = "cycle",
                              rep = 100){

  # Error checking ------------------

  coll <- checkmate::makeAssertCollection()

  # data type
  checkmate::assert_data_frame(community, add = coll)
  checkmate::assert_data_frame(survey_ref, add = coll)

  # colnames
  checkmate::assert_subset(survey_id, choices = colnames(survey_ref), empty.ok = FALSE, add = coll)

  # numbers
  checkmate::assert_number(rep, lower = 0, add = coll)
  checkmate::assert_number(exclude_num, lower = 0, null.ok = FALSE, add = coll)

  # subsetting
  checkmate::assert_subset(town_ignore, choices = as.character(unique(survey_ref[[town]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(round_ignore, choices = as.character(unique(survey_ref[[round]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(cycle_ignore, choices = as.character(unique(survey_ref[[cycle]])), empty.ok = TRUE, add = coll)

  checkmate::assert_subset(specify_towns, choices = as.character(unique(survey_ref[[town]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(specify_points, choices = as.character(unique(survey_ref[[point_id]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(specify_rounds, choices = as.character(unique(survey_ref[[round]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(as.character(specify_cycles), choices = as.character(unique(survey_ref[[cycle]])), empty.ok = TRUE, add = coll)

  checkmate::assert_subset(exclude_level, choices = c("survey", "point"), empty.ok = TRUE, add = coll)


  checkmate::reportAssertions(coll)


  # Calculations ------------------

  # add cols with key info
  community_info <- community %>%
    left_join(survey_ref %>% select(.data[[survey_id]], .data[[point_id]], .data[[town]], .data[[round]], .data[[cycle]]))

  # subset data if specified
  if(!is.null(town_ignore)){ # to particular town
  community_info <- community_info[-which(community_info$town %in% town_ignore),]
  }
  if(!is.null(round_ignore)){
    community_info <- community_info[-which(community_info$round %in% round_ignore),]
  }
  if(!is.null(cycle_ignore)){
    community_info <- community_info[-which(community_info$cycle %in% cycle_ignore),]
  }

  # specify all for specify_* if null
  if(is.null(specify_towns)){
    specify_towns <- unique(survey_ref[[town]])
  }
  if(is.null(specify_points)){
    specify_points <- unique(survey_ref[[point_id]])
  }
  if(is.null(specify_rounds)){
    specify_rounds <- unique(survey_ref[[round]])
  }
  if(is.null(specify_cycles)){
    specify_cycles <- unique(survey_ref[[cycle]])
  }


  # FULL data - summarise by point instead of survey
  sp_pointdens <- community_info %>%
    tidyr::pivot_longer(cols = -c(.data[[survey_id]], .data[[point_id]], .data[[town]], .data[[round]], .data[[cycle]]),
                        names_to = "species", values_to = "count") %>%
    dplyr::group_by(.data[[point_id]], .data[["species"]]) %>% # counts per point per species
    dplyr::summarise(count = sum(count)) %>%
    dplyr::filter(count > 0) %>% # remove points with 0 for particular species
    dplyr::group_by(.data[["species"]]) %>% # no. of points per species
    dplyr::summarise(full = n())



  # Random exclusion by SURVEY
  if(exclude_level == "survey"){

    # SUBSET dataset
    for (i in 1:rep) {

      # sample
      sampled <- community_info %>%

        # filtered subset to sample from
        filter((community_info$town %in% specify_towns) &
                 (community_info$point_id %in% specify_points) &
                 (community_info$round %in% specify_rounds) &
                 (community_info$cycle %in% specify_cycles)) %>%
        slice_sample(n = exclude_num)

      # randomly exclude surveys within specified categories
      filtered <- community_info %>% filter(!(survey_id %in% sampled$survey_id))


      # summarise by point in filtered dataset
      sp_pointdens_filtered <- filtered %>%
        tidyr::pivot_longer(cols = -c(.data[[survey_id]], .data[[point_id]], .data[[town]],  .data[[round]], .data[[cycle]]),
                            names_to = "species", values_to = "count") %>%
        dplyr::group_by(point_id, .data[["species"]]) %>%
        dplyr::summarise(count = sum(count)) %>%
        dplyr::filter(count > 0) %>%
        dplyr::group_by(.data[["species"]]) %>%
        dplyr::summarise(count = n())

      # append to sp_pointdens
      sp_pointdens <- sp_pointdens %>% left_join(sp_pointdens_filtered)
      names(sp_pointdens)[2+i] <- paste0("iter", i) # rename col to iteration no.

    }



  # Random exclusion by POINT
  }else if(exclude_level == "point"){

    # group by points
    points <- community_info %>%
      dplyr::group_by(point_id, town, round) %>%
      dplyr::summarise()

    # SUBSET dataset
    for (i in 1:rep) {

      # randomly exclude by points within specified categories
      points_filtered <- points[-sample(which((points$town %in% specify_towns) &
                                                (points$round %in% specify_rounds)),
                                              exclude_num),]

      # summarise by point in filtered dataset
      sp_pointdens_filtered <- community_info %>%
        dplyr::inner_join(points_filtered, by = c("point_id", "town", "round")) %>%
        tidyr::pivot_longer(cols = -c(.data[[survey_id]], .data[[point_id]], .data[[town]],  .data[[round]], .data[[cycle]]),
                            names_to = "species", values_to = "count") %>%
        dplyr::group_by(point_id, .data[["species"]]) %>%
        dplyr::summarise(count = sum(count)) %>%
        dplyr::filter(count > 0) %>%
        dplyr::group_by(.data[["species"]]) %>%
        dplyr::summarise(count = n())

      # append to sp_pointdens
      sp_pointdens <- sp_pointdens %>% left_join(sp_pointdens_filtered)
      names(sp_pointdens)[2+i] <- paste0("iter", i) # rename col to iteration no.

    }

  }

  return(sp_pointdens)
}
