#'Extract site species richness normalised to highest common survey effort.
#'
#'Extract species richness for all sites supplied at the highest common survey effort.
#'Wrapper function to `sac_extractor()` and `filter_obs()`
#'
#'@param observations Dataframe or list of dataframes of species observations.
#'It should include columns for `survey_id`, `town`, `round`, `priority`, `species` and `abundance`.
#'@param survey_ref Dataframe or list of dataframes of all surveys conducted. Values in the column
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
#'@return A dataframe with columns for the `sites`, species `richness` or `mean` species richness and `sd` (standard deviation)
#'of results, as well as the information specified in `specify_town`, `specify_round` and `specify_priority`.
#'
#'@import checkmate
#'@import dplyr
#'@importFrom vegan specaccum
#'@importFrom rlang .data
#'@importFrom stats xtabs
#'
#'@export
standard_SR <- function(observations, survey_ref,
                        specify_town = NULL,
                        specify_round = NULL,
                        specify_priority = NULL,
                        survey_id = "survey_id",
                        town = "town", round = "round", priority = "priority",
                        species = "species", genus = "genus", family = "family",
                        abundance = "abundance"){

  if(class(observations) %in% c('vctrs_list_of', 'vctrs_vctr', 'list')) {
    results <- foreach(i = seq_along(observations),
                       .packages = "tidyverse") %dopar% {

                         source("../../R/filter_obs.R")
                         source("../../R/check_taxongrps.R")
                         source("../../R/sac_extractor.R")

                         unique <- survey_ref[[i]] %>%
                           distinct(town, round, priority)

                         if(!is.null(specify_priority)){unique <- unique %>% filter(priority = specify_priority)}
                         if(!is.null(specify_round)){unique <- unique %>% filter(round = specify_round)}
                         if(!is.null(specify_town)){unique <- unique %>% filter(town = specify_town)}

                         results <- data.frame()

                         for(j in 1:nrow(unique)){

                           sac_data <-  sac_extractor(observations = observations[[i]],
                                                      survey_ref = survey_ref[[i]],
                                                      specify_town = unique$town[j],
                                                      specify_round = unique$round[j],
                                                      specify_priority = unique$priority[j])

                           if(nrow(sac_data) > 1){ # deal w bug where some iterations only have 1 data point for amphibians...
                             results <- results %>%
                               bind_rows(sac_data)
                           }
                           rm(sac_data)
                         }
                         results
                       }

    results <- results %>%
      map_dfr(~ bind_rows(.), .id = "iteration") %>%
      mutate(iteration = as.numeric(iteration)) %>%
      group_by(town, round, priority, sites) %>%
      summarise(mean = mean(richness),
                sd = sd(richness, na.rm = TRUE))

    summary <- results %>%
      group_by(town, round, priority) %>%
      summarise(n_max = max(sites)) %>%
      group_by(town, round, priority) %>%
      summarise(sites = min(n_max)) %>%
      group_by(priority) %>%
      summarise(sites = min(sites)) %>%
      inner_join(results)

    return(summary)

  } else {

    unique <- survey_ref %>%
      distinct(town, round, priority)

    if(!is.null(specify_priority)){unique <- unique %>% filter(priority = specify_priority)}
    if(!is.null(specify_round)){unique <- unique %>% filter(round = specify_round)}
    if(!is.null(specify_town)){unique <- unique %>% filter(town = specify_town)}

    results <- data.frame()

    for(j in 1:nrow(unique)){

      sac_data <-  sac_extractor(observations = observations,
                                 survey_ref = survey_ref,
                                 specify_town = unique$town[j],
                                 specify_round = unique$round[j],
                                 specify_priority = unique$priority[j])

      if(nrow(sac_data) > 1){ # deal w bug where some iterations only have 1 data point for amphibians...
        results <- results %>%
          bind_rows(sac_data)
      }
      rm(sac_data)
    }

    summary <- results %>%
      group_by(town, round, priority) %>%
      summarise(n_max = max(sites)) %>%
      group_by(town, round, priority) %>%
      summarise(sites = min(n_max)) %>%
      group_by(priority) %>%
      summarise(sites = min(sites)) %>%
      inner_join(results)

    return(summary)
  }
}
