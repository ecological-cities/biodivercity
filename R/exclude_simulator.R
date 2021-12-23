#'Simulate multiple scenarios with animal surveys randomly excluded
#'
#'Randomly exclude either animal surveys or sampling points (and all their associated surveys) from a
#'species community matrix. Each column of the community matrix should represent a unique species,
#'while each row represents a single unique survey.
#'
#'@param community_matrix Dataframe of the species community matrix (one survey per row).
#'It should a column containing the unique IDs for the animal surveys conducted (e.g. `survey_id`).
#'@param survey_ref Dataframe containing information of the surveys conducted
#'(e.g. `survey_id`, `point_id`, `period`, `area`, `cycle`). Values in the column
#'  `survey_id` should correspond to those in `community_matrix`.
#'@param exclude_num Number of surveys/points to exclude (remove) from the full dataset `community_matrix`.
#'Value should be a single integer.
#'@param exclude_level Whether to exclude based on `"survey"` or `"point"`. Defaults to `"survey"`.
#'@param specify_areas Specify the `area`(s) to randomly exclude surveys from (character string or vector).
#'Defaults to `NULL`, which excludes surveys randomly across all areas.
#'@param specify_points Specify the `point_id`(s) of sampling points to randomly exclude surveys from (character string or vector).
#'Defaults to `NULL`, which excludes surveys randomly across all points.
#'@param specify_periods Specify the survey `period`(s) to exclude surveys from (character string or vector). Defaults to `NULL`,
#'which excludes surveys randomly across all periods.
#'@param specify_cycles Specify the survey `cycle`(s) to exclude surveys from (character string or vector). Defaults to `NULL`,
#'which excludes surveys randomly across all cycles.
#'@param area_ignore Ignore specified `area`(s) from the simulation and output (character string or vector). Defaults to `NULL`,
#'which includes all areas in the simulation.
#'@param period_ignore Ignore specified `period` from the simulation and output (character string or vector). Defaults to `NULL`,
#'which includes all periods in the simulation.
#'@param cycle_ignore Ignore specified `cycles`(s) from the simulation and output (character string or vector). Defaults to `NULL`,
#'which includes all cycles in the simulation.
#'@param survey_id Column name of the unique identifier for each survey in `community_matrix` and
#'`survey_ref`. Defaults to `survey_id`.
#'@param point_id Column name of the unique identifier for each sampling point in `survey_ref`.
#'Defaults to `survey_id`.
#'@param area Column name of the area specified in `survey_ref`.
#'Defaults to `area`.
#'@param period Column name of the survey period specified in `survey_ref`.
#'Defaults to `period`.
#'@param cycle Column name of the survey cycle specified in `survey_ref`.
#'Defaults to `cycle`.
#'@param rep Specify number of repetitions (scenarios) to simulate. Defaults to `100`.
#'
#'@return A dataframe containing the results of each iteration (column).
#'The count of sampling points with each species (row) is shown. The column `full`
#'shows the breakdown of counts within the original dataset.
#'
#'
#'@import checkmate
#'@importFrom dplyr left_join inner_join group_by summarise filter n slice_sample
#'@importFrom tidyr pivot_longer
#'@importFrom rlang .data
#'
#'@examples
#'data(animal_observations)
#'data(animal_surveys)
#'
#'# filter animal observations to taxon of interest
#'birds <- filter_obs(observations = animal_observations,
#'                    survey_ref = animal_surveys,
#'                    specify_taxon = "Aves")
#'
#'# convert animal observations to community matrix
#'birds <- as.data.frame.matrix(xtabs(abundance ~ survey_id + species, data = birds))
#'birds <- cbind(survey_id = rownames(birds), birds) # convert rownames to col
#'
#'# run function
#'set.seed(123)
#'exclude_simulator(birds, animal_surveys,
#'                  exclude_num = 15, exclude_level = "survey", # exclude 15 surveys
#'                  specify_cycles = 5, # exclude those in cycle 5 only
#'                  period_ignore = "2", # remove all data from period 2
#'                  rep = 10) # 10 scenarios only
#'
#'@export
exclude_simulator <- function(community_matrix, survey_ref,
                              exclude_level = "survey",
                              exclude_num,
                              specify_areas = NULL, specify_points = NULL, specify_periods = NULL, specify_cycles = NULL,
                              area_ignore = NULL, period_ignore = NULL, cycle_ignore = NULL,
                              survey_id = "survey_id", point_id = "point_id",
                              area = "area", period = "period", cycle = "cycle",
                              rep = 100){

  # Error checking ------------------

  coll <- checkmate::makeAssertCollection()

  # data type
  checkmate::assert_data_frame(community_matrix, add = coll)
  checkmate::assert_data_frame(survey_ref, add = coll)

  # colnames
  checkmate::assert_subset(survey_id, choices = colnames(survey_ref), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(survey_id, choices = colnames(community_matrix), empty.ok = FALSE, add = coll)

  # subsetting
  checkmate::assert_subset(area_ignore, choices = as.character(unique(survey_ref[[area]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(period_ignore, choices = as.character(unique(survey_ref[[period]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(cycle_ignore, choices = as.character(unique(survey_ref[[cycle]])), empty.ok = TRUE, add = coll)

  checkmate::assert_subset(specify_areas, choices = as.character(unique(survey_ref[[area]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(specify_points, choices = as.character(unique(survey_ref[[point_id]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(specify_periods, choices = as.character(unique(survey_ref[[period]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(as.character(specify_cycles), choices = as.character(unique(survey_ref[[cycle]])), empty.ok = TRUE, add = coll)

  checkmate::assert_subset(exclude_level, choices = c("survey", "point"), empty.ok = TRUE, add = coll)

  # numbers
  checkmate::assert_number(rep, lower = 0, add = coll)
  checkmate::assert_number(exclude_num, lower = 0, null.ok = FALSE, add = coll)

  # check if community matrix (numbers only)
  if(!checkmate::test_data_frame(community_matrix[ , -which(names(community_matrix) %in% c("survey_id"))],
                               types = "numeric")){stop("All columns in community_matrix except 'survey_id' should be numeric")}
  checkmate::assert_data_frame(community_matrix[ , -which(names(community_matrix) %in% c("survey_id"))],
                               types = "numeric",
                               any.missing = FALSE, # ensure not empty
                               all.missing = FALSE,
                               .var.name = "community_matrix")


  checkmate::reportAssertions(coll)


  # Calculations ------------------

  # add cols with key info
  suppressMessages(community_info <- community_matrix %>%
                     dplyr::left_join(survey_ref %>% select(.data[[survey_id]], .data[[point_id]], .data[[area]], .data[[period]], .data[[cycle]])))

  # subset data if specified
  if(!is.null(area_ignore)){ # to particular area
  community_info <- community_info[-which(community_info$area %in% area_ignore),]
  }
  if(!is.null(period_ignore)){
    community_info <- community_info[-which(community_info$period %in% period_ignore),]
  }
  if(!is.null(cycle_ignore)){
    community_info <- community_info[-which(community_info$cycle %in% cycle_ignore),]
  }

  # specify all for specify_* if null
  if(is.null(specify_areas)){
    specify_areas <- unique(survey_ref[[area]])
  }
  if(is.null(specify_points)){
    specify_points <- unique(survey_ref[[point_id]])
  }
  if(is.null(specify_periods)){
    specify_periods <- unique(survey_ref[[period]])
  }
  if(is.null(specify_cycles)){
    specify_cycles <- unique(survey_ref[[cycle]])
  }


  # FULL data - summarise by point instead of survey
  sp_pointdens <- community_info %>%
    tidyr::pivot_longer(cols = -c(.data[[survey_id]], .data[[point_id]], .data[[area]], .data[[period]], .data[[cycle]]),
                        names_to = "species", values_to = "count") %>%
    dplyr::group_by(.data[[point_id]], .data[["species"]]) %>% # counts per point per species
    dplyr::summarise(count = sum(count)) %>%
    dplyr::filter(count > 0) %>% # remove points with 0 for particular species
    dplyr::group_by(.data[["species"]]) %>% # no. of points per species
    dplyr::summarise(full = dplyr::n())



  # Random exclusion by SURVEY
  if(exclude_level == "survey"){

    # SUBSET dataset
    for (i in 1:rep) {

      # sample
      sampled <- community_info %>%

        # filtered subset to sample from
        dplyr::filter((community_info$area %in% specify_areas) &
                        (community_info$point_id %in% specify_points) &
                        (community_info$period %in% specify_periods) &
                        (community_info$cycle %in% specify_cycles)) %>%
        dplyr::slice_sample(n = exclude_num)

      # randomly exclude surveys within specified categories
      filtered <- community_info %>% dplyr::filter(!(survey_id %in% sampled$survey_id))


      # summarise by point in filtered dataset
      sp_pointdens_filtered <- filtered %>%
        tidyr::pivot_longer(cols = -c(.data[[survey_id]], .data[[point_id]], .data[[area]],  .data[[period]], .data[[cycle]]),
                            names_to = "species", values_to = "count") %>%
        dplyr::group_by(point_id, .data[["species"]]) %>%
        dplyr::summarise(count = sum(count)) %>%
        dplyr::filter(count > 0) %>%
        dplyr::group_by(.data[["species"]]) %>%
        dplyr::summarise(count = dplyr::n())

      # append to sp_pointdens
      sp_pointdens <- sp_pointdens %>% dplyr::left_join(sp_pointdens_filtered, by = "species")
      names(sp_pointdens)[2+i] <- paste0("iter", i) # rename col to iteration no.

    }



  # Random exclusion by POINT
  }else if(exclude_level == "point"){

    # group by points
    points <- community_info %>%
      dplyr::group_by(point_id, area, period) %>%
      dplyr::summarise()

    # SUBSET dataset
    for (i in 1:rep) {

      # randomly exclude by points within specified categories
      points_filtered <- points[-sample(which((points$area %in% specify_areas) &
                                                (points$period %in% specify_periods)),
                                              exclude_num),]

      # summarise by point in filtered dataset
      sp_pointdens_filtered <- community_info %>%
        dplyr::inner_join(points_filtered, by = c("point_id", "area", "period")) %>%
        tidyr::pivot_longer(cols = -c(.data[[survey_id]], .data[[point_id]], .data[[area]],  .data[[period]], .data[[cycle]]),
                            names_to = "species", values_to = "count") %>%
        dplyr::group_by(point_id, .data[["species"]]) %>%
        dplyr::summarise(count = sum(count)) %>%
        dplyr::filter(count > 0) %>%
        dplyr::group_by(.data[["species"]]) %>%
        dplyr::summarise(count = n())

      # append to sp_pointdens
      sp_pointdens <- sp_pointdens %>% dplyr::left_join(sp_pointdens_filtered, by = "species")
      names(sp_pointdens)[2+i] <- paste0("iter", i) # rename col to iteration no.

    }

  }

  return(sp_pointdens)
}
