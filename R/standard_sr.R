#'Extract site species richness normalised to highest common survey effort.
#'
#'Extract species richness for all sites supplied at the highest common survey effort.
#'Wrapper function to `calculate_sac()` and `filter_observations()`
#'
#'@param observations Dataframe or list of dataframes of species observations.
#'It should include columns for `survey_id`, `area`, `period`, `taxon`, `species` and `abundance`.
#'@param survey_ref Dataframe or list of dataframes of all surveys conducted. Values in the column
#'  `survey_id` should correspond to those in the `observations`.
#'@param specify_area Specify the `area`.
#'@param specify_period Specify the survey `period`.
#'@param specify_taxon Specify the `taxon` taxon.
#'@param survey_id Column name of the unique identifier for each survey in `observations` and
#'`survey_ref`. Defaults to `survey_id`.
#'@param area Column name of the area specified in `observations` and `survey_ref`.
#'Defaults to `area`.
#'@param period Column name of the sampling period specified in `observations` and `survey_ref`.
#'Defaults to `period`.
#'@param taxon Column name of the taxon taxon specified in `observations` and `survey_ref`.
#'Defaults to `taxon`.
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
#'of results, as well as the information specified in `specify_area`, `specify_period` and `specify_taxon`.
#'
#'@import dplyr
#'@importFrom purrr map_dfr
#'@importFrom rlang .data
#'
#'@export
standard_sr <- function(observations, survey_ref,
                        specify_area = NULL,
                        specify_period = NULL,
                        specify_taxon = NULL,
                        survey_id = "survey_id",
                        area = "area", period = "period", taxon = "taxon",
                        species = "species", genus = "genus", family = "family",
                        abundance = "abundance"){

  if(class(observations) %in% c('vctrs_list_of', 'vctrs_vctr', 'list')) {

    results_list <- list()

    for(i in seq_along(observations)){

       unique <- survey_ref %>%
         dplyr::distinct(area, period, taxon)

       if(!is.null(specify_taxon)){unique <- unique %>% dplyr::filter(taxon = specify_taxon)}
       if(!is.null(specify_period)){unique <- unique %>% dplyr::filter(period = specify_period)}
       if(!is.null(specify_area)){unique <- unique %>% dplyr::filter(area = specify_area)}

       results <- data.frame()

       for(j in 1:nrow(unique)){

         sac_data <-  calculate_sac(observations = observations,
                                    survey_ref = survey_ref,
                                    specify_area = unique$area[j],
                                    specify_period = unique$period[j],
                                    specify_taxon = unique$taxon[j],
                                    survey_id = survey_id,
                                    area = area, period = period, taxon = taxon,
                                    species = species, genus = genus, family = family,
                                    abundance = abundance)

         if(nrow(sac_data) > 1){ # deal w bug where some iterations only have 1 data point for amphibians...
           results_list[[i]] <- results %>%
             dplyr::bind_rows(sac_data)
         }
         rm(sac_data)
       }
     }

    results_list <- results_list %>%
      purrr::map_dfr(~ dplyr::bind_rows(.), .id = "iteration") %>%
      dplyr::mutate(iteration = as.numeric(.data$iteration)) %>%
      dplyr::group_by(area, period, taxon, .data$sites) %>%
      dplyr::summarise(mean = mean(.data$richness),
                       sd = sd(.data$richness, na.rm = TRUE))

    summary <- results_list %>%
      dplyr::group_by(area, period, taxon) %>%
      dplyr::summarise(n_max = max(.data$sites)) %>%
      dplyr::group_by(area, period, taxon) %>%
      dplyr::summarise(sites = min(.data$n_max)) %>%
      dplyr::group_by(taxon) %>%
      dplyr::summarise(sites = min(.data$sites)) %>%
      dplyr::inner_join(results_list)

    return(summary)

  } else {

    unique <- survey_ref %>%
      dplyr::distinct(area, period, taxon)

    if(!is.null(specify_taxon)){unique <- unique %>% dplyr::filter(taxon = specify_taxon)}
    if(!is.null(specify_period)){unique <- unique %>% dplyr::filter(period = specify_period)}
    if(!is.null(specify_area)){unique <- unique %>% dplyr::filter(area = specify_area)}

    results <- data.frame()

    for(j in 1:nrow(unique)){

      sac_data <-  calculate_sac(observations = observations,
                                 survey_ref = survey_ref,
                                 specify_area = unique$area[j],
                                 specify_period = unique$period[j],
                                 specify_taxon = unique$taxon[j],
                                 survey_id = survey_id,
                                 area = area, period = period, taxon = taxon,
                                 species = species, genus = genus, family = family,
                                 abundance = abundance)

      if(nrow(sac_data) > 1){ # deal w bug where some iterations only have 1 data point for amphibians...
        results <- results %>%
          dplyr::bind_rows(sac_data)
      }
      rm(sac_data)
    }

    summary <- results %>%
      dplyr::group_by(area, period, taxon) %>%
      dplyr::summarise(n_max = max(.data$sites)) %>%
      dplyr::group_by(area, period, taxon) %>%
      dplyr::summarise(sites = min(.data$n_max)) %>%
      dplyr::group_by(taxon) %>%
      dplyr::summarise(sites = min(.data$sites)) %>%
      dplyr::inner_join(results)

    return(summary)
  }
}
