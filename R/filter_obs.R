#'Filter species observations based on specified criteria
#'
#'Filters species observations from biodiversity surveys based on specified criteria.
#'
#'@param observations Dataframe of species observations.
#'It should include columns for `survey_id`, `town`, `round`, `priority`.
#'@param survey_ref Dataframe of all surveys conducted. Values in the column
#'  `survey_id` should correspond to those in the `observations`.
#'@param specify_town Specify the `town`(s). Defaults to `NULL`, which includes all towns.
#'@param specify_round Specify the survey `round`(s). Defaults to `NULL`, which includes all rounds.
#'@param specify_priority Specify the `priority` taxon/taxa. Defaults to `NULL`, which includes all taxa.
#'@param survey_id Column name of the unique identifier for each survey in `observations` and
#'`survey_ref`. Defaults to `survey_id`.
#'@param point_id Column name of the unique identifier for each point in `observations` and
#'`survey_ref`. Defaults to `point_id`.
#'@param town Column name of the town specified in `observations` and `survey_ref`.
#'Defaults to `town`.
#'@param round Column name of the sampling round specified in `observations` and `survey_ref`.
#'Defaults to `round`.
#'@param priority Column name of the priority taxon specified in `observations` and `survey_ref`.
#'Defaults to `priority`.
#'
#'@return The dataframe `observations` subset according to specified criteria.
#'
#'@import checkmate
#'@import dplyr
#'@importFrom rlang .data
#'
#'@export
filter_obs <- function(observations, survey_ref,
                       specify_town = NULL, specify_round = NULL, specify_priority = NULL,
                       survey_id = "survey_id", point_id = "point_id",
                       town = "town", round = "round", priority = "priority"){

  # Error checking ------------------

  coll <- checkmate::makeAssertCollection()

  # data type
  checkmate::assert_data_frame(observations, add = coll)
  checkmate::assert_data_frame(survey_ref, add = coll)

  # colnames
  checkmate::assert_subset(survey_id, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(survey_id, choices = colnames(survey_ref), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(point_id, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(point_id, choices = colnames(survey_ref), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(town, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(town, choices = colnames(survey_ref), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(round, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(round, choices = colnames(survey_ref), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(priority, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(priority, choices = colnames(survey_ref), empty.ok = FALSE, add = coll)

  # subsetting
  checkmate::assert_subset(as.character(specify_town), choices = as.character(unique(observations[[town]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(as.character(specify_round), choices = as.character(unique(observations[[round]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(as.character(specify_priority), choices = as.character(unique(observations[[priority]])), empty.ok = TRUE, add = coll)

  checkmate::assert_subset(as.character(specify_town), choices = as.character(unique(survey_ref[[town]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(as.character(specify_round), choices = as.character(unique(survey_ref[[round]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(as.character(specify_priority), choices = as.character(unique(survey_ref[[priority]])), empty.ok = TRUE, add = coll)


  checkmate::reportAssertions(coll)


  # Calculations ------------------

  # specify all for specify_* if null
  if(is.null(specify_town)){
    specify_town <- unique(observations[[town]])
  }
  if(is.null(specify_round)){
    specify_round <- unique(observations[[round]])
  }
  if(is.null(specify_priority)){
    specify_priority <- unique(observations[[priority]])
  }


  # subset
  svy_subset <- survey_ref %>%
    dplyr::filter((.data[[town]] %in% specify_town) & (.data[[round]] %in% specify_round)) %>%
    dplyr::filter((.data[[priority]] %in% specify_priority))

  survey_id_subset <- svy_subset[[survey_id]]
  point_id_subset <- unique(svy_subset[[point_id]])

  obs_subset <- observations %>%
    dplyr::filter((.data[[town]] %in% specify_town) & (.data[[round]] %in% specify_round)) %>%
    dplyr::filter((.data[[priority]] %in% specify_priority)) %>%

    # adjust factor levels based on svy_subset & account for surveys with 0 observations
    dplyr::mutate(survey_id = factor(.data[[survey_id]],
                                     levels = survey_id_subset)) %>%
    dplyr::mutate(point_id = factor(.data[[point_id]],
                                     levels = point_id_subset))

  return(obs_subset)
}
