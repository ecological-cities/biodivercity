#'Extract species accumulation curves based on specified criteria
#'
#'Wrapper function to `filter_obs()` which filters species observations from biodiversity surveys, based on specified criteria.
#'Subsequently forms community matrix, and removes taxon group-level (genus/family)
#'if all species within group are observed in the town (& round), using the function `check_taxongrps()`.
#'Finally, runs the function `specaccum()` in package `vegan`, and extracts output
#'data. Data on both the biodiversity surveys and their species observations (and respective abundances)
#'must be provided.
#'
#'@param observations Dataframe of species observations.
#'It should include columns for `survey_id`, `town`, `round`, `priority`, `species` and `abundance`.
#'@param survey_ref Dataframe of all surveys conducted. Values in the column
#'  `survey_id` should correspond to those in the `observations`.
#'@param specify_town Specify the `town`.
#'@param specify_round Specify the survey `round`.
#'@param specify_priority Specify the `priority` taxon.
#'@param survey_id Column name of the unique identifier for each survey in `observations` and
#'`survey_ref`. Defaults to `survey_id`.
#'@param town Column name of the town specified in `observations` and `survey_ref`.
#'Defaults to `town`.
#'@param round Column name of the sampling round specified in `observations` and `survey_ref`.
#'Defaults to `round`.
#'@param priority Column name of the priority taxon specified in `observations` and `survey_ref`.
#'Defaults to `priority`.
#'@param species Column name of the species specified in `observations`.
#'Defaults to `species`.
#'@param family Column name of the family specified in `observations`.
#'Defaults to `family`. Used to remove family-level records in the `species` column,
#'if all species within the group are observed within the filtered dataset.
#'@param genus Column name of the genus specified in `observations`.
#'Defaults to `genus`.  Used to remove genus-level records in the `species` column,
#'if all species within the group are observed within the filtered dataset.
#'@param abundance Column name of the species abundance specified in `observations`.
#'Defaults to `abundance`.
#'
#'@return A dataframe with columns for the `sites`, species `richness` and `sd` (standard deviation)
#'of results, as well as the information specified in `specify_town`, `specify_round` and `specify_priority`.
#'
#'@import checkmate
#'@import dplyr
#'@importFrom vegan specaccum
#'@importFrom rlang .data
#'@importFrom stats xtabs
#'
#'@export
sac_extractor <- function(observations, survey_ref,
                          specify_town, specify_round, specify_priority,
                          survey_id = "survey_id",
                          town = "town", round = "round", priority = "priority",
                          species = "species", genus = "genus", family = "family",
                          abundance = "abundance"){

  # Error checking ------------------

  coll <- checkmate::makeAssertCollection()

  # colnames
  checkmate::assert_subset(species, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(genus, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(family, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(abundance, choices = colnames(observations), empty.ok = FALSE, add = coll)

  checkmate::reportAssertions(coll)


  # Calculations ------------------

  # subset
  obs_subset <- filter_obs(observations, survey_ref,
                           specify_town = specify_town,
                           specify_round = specify_round,
                           specify_priority = specify_priority,
                           survey_id = survey_id,
                           town = town, round = round, priority = priority)



  # run check_taxongrps()
  rmspp <- check_taxongrps(obs_subset, level = "town")


  # overwrite colnames if names are different from default
  obs_subset <- obs_subset %>%
    dplyr::mutate(abundance = .data[[abundance]]) %>%
    dplyr::mutate(species = .data[[species]])


  # community matrix
  community <- as.data.frame.matrix(xtabs(abundance ~ survey_id + species,
                                          data = obs_subset))


  # exclude genus/family-lvl records present in rmspp
  community <- community[, !(colnames(community) %in% rmspp$name)]


  # species accumulation
  sac <- vegan::specaccum(community, method = "exact", # find the mean sp richness
                          conditioned = FALSE,  # estimation of SD NOT conditional on data for exact SAC
                          gamma = "chao") # extrapolation mtd

  # extract data
  sac_data <- data.frame(town = specify_town,
                         round = specify_round,
                         priority = specify_priority,
                         sites = sac$sites,
                         richness = sac$richness,
                         sd = sac$sd)

  return(sac_data)
}
