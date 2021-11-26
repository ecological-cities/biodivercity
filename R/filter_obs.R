#'Filter species observations based on specified criteria
#'
#'Filters species observations from biodiversity surveys based on specified criteria.
#'
#'@param observations Dataframe of species observations.
#'It should include columns for `survey_id`, `town`, `round`, `priority`.
#'@param survey_ref Dataframe of all surveys conducted. Values in the column
#'  `survey_id` should correspond to those in the `observations`.
#'@param specify_area Specify the `area`(s). Defaults to `NULL`, which includes all towns.
#'@param specify_period Specify the survey `period`(s). Defaults to `NULL`, which includes all rounds.
#'@param specify_taxon Specify the `taxon` taxon/taxa. Defaults to `NULL`, which includes all taxa.
#'@param survey_id Column name of the unique identifier for each survey in `observations` and
#'`survey_ref`. Defaults to `survey_id`.
#'@param point_id Column name of the unique identifier for each point in `observations` and
#'`survey_ref`. Defaults to `point_id`.
#'@param area Column name of the town specified in `observations` and `survey_ref`.
#'Defaults to `town`.
#'@param period Column name of the sampling round specified in `observations` and `survey_ref`.
#'Defaults to `round`.
#'@param taxon Column name of the priority taxon specified in `observations` and `survey_ref`.
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
                       specify_area = NULL, specify_period = NULL, specify_taxon = NULL,
                       survey_id = "survey_id", point_id = "point_id",
                       area = "town", period = "round", taxon = "priority"){

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
  checkmate::assert_subset(area, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(area, choices = colnames(survey_ref), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(period, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(period, choices = colnames(survey_ref), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(taxon, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(taxon, choices = colnames(survey_ref), empty.ok = FALSE, add = coll)

  # subsetting
  checkmate::assert_subset(as.character(specify_area), choices = as.character(unique(observations[[area]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(as.character(specify_period), choices = as.character(unique(observations[[period]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(as.character(specify_taxon), choices = as.character(unique(observations[[taxon]])), empty.ok = TRUE, add = coll)

  checkmate::assert_subset(as.character(specify_area), choices = as.character(unique(survey_ref[[area]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(as.character(specify_period), choices = as.character(unique(survey_ref[[period]])), empty.ok = TRUE, add = coll)
  checkmate::assert_subset(as.character(specify_taxon), choices = as.character(unique(survey_ref[[taxon]])), empty.ok = TRUE, add = coll)


  checkmate::reportAssertions(coll)


  # Calculations ------------------

  # specify all for specify_* if null
  if(is.null(specify_area)){
    specify_area <- unique(observations[[area]])
  }
  if(is.null(specify_period)){
    specify_period <- unique(observations[[period]])
  }
  if(is.null(specify_taxon)){
    specify_taxon <- unique(observations[[taxon]])
  }


  # subset
  svy_subset <- survey_ref %>%
    dplyr::filter((.data[[area]] %in% specify_area) & (.data[[period]] %in% specify_period)) %>%
    dplyr::filter((.data[[taxon]] %in% specify_taxon))

  survey_id_subset <- svy_subset[[survey_id]]
  point_id_subset <- unique(svy_subset[[point_id]])

  obs_subset <- observations %>%
    dplyr::filter((.data[[area]] %in% specify_area) & (.data[[period]] %in% specify_period)) %>%
    dplyr::filter((.data[[taxon]] %in% specify_taxon)) %>%

    # adjust factor levels based on svy_subset & account for surveys with 0 observations
    dplyr::mutate(survey_id = factor(.data[[survey_id]],
                                     levels = survey_id_subset)) %>%
    dplyr::mutate(point_id = factor(.data[[point_id]],
                                     levels = point_id_subset))

  return(obs_subset)
}
