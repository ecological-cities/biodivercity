#'Tally number of species observations based on specified criteria
#'
#'Wrapper function to `filter_observations()` which filters species observations from biodiversity surveys, based on specified criteria.
#'Subsequently tallies the number of species per survey/sampling point (similar to function `specnumber()` in package `vegan`),
#'and extracts output data. If tallied at the level of sampling points (`level = 'point'`),
#'removes taxon group-level (genus/family) if all species within group are observed at the point (& period),
#'using the function `check_taxongrps()`. Data on both the biodiversity surveys and their species observations (and respective abundances)
#'must be provided.
#'
#'@param observations Dataframe of species observations.
#'It should include columns for `survey_id`, `area`, `period`, `taxon`, `species` and `abundance`.
#'@param survey_ref Dataframe of all surveys conducted. Values in the column
#'  `survey_id` should correspond to those in the `observations`.
#'@param specify_area Specify the `area` (character).
#'@param specify_period Specify the survey `period` (character).
#'@param specify_taxon Specify the `taxon` of interest (character).
#'@param level Specify whether to tally by `'survey'` or `'point'` (character).
#'@param survey_id Column name of the unique identifier for each survey in `observations` and
#'`survey_ref`. Defaults to `survey_id`.
#'@param area Column name of the area specified in `observations` and `survey_ref`.
#'Defaults to `area`.
#'@param period Column name of the sampling period specified in `observations` and `survey_ref`.
#'Defaults to `period`.
#'@param taxon Column name of the taxon of interest specified in `observations` and `survey_ref`.
#'Defaults to `taxon`.
#'@param point_id Column name of the unique identifier for each sampling point specified in `observations` and `survey_ref`.
#'Defaults to `point_id`.
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
#'@return A dataframe containing the tally of species per survey/sampling point
#'(depends if `level =` `'survey'` or `'point'`).
#'
#'@import checkmate
#'@import dplyr
#'@importFrom tidyr complete
#'@importFrom rlang .data
#'
#'@export
tally_observations <- function(observations, survey_ref, specify_area, specify_period,
    specify_taxon, level, survey_id = "survey_id", area = "area", period = "period",
    taxon = "taxon", point_id = "point_id", species = "species",
    genus = "genus", family = "family", abundance = "abundance") {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    # colnames
    checkmate::assert_subset(point_id, choices = colnames(observations),
        empty.ok = FALSE, add = coll)
    checkmate::assert_subset(species, choices = colnames(observations),
        empty.ok = FALSE, add = coll)
    checkmate::assert_subset(genus, choices = colnames(observations), empty.ok = FALSE,
        add = coll)
    checkmate::assert_subset(family, choices = colnames(observations),
        empty.ok = FALSE, add = coll)
    checkmate::assert_subset(abundance, choices = colnames(observations),
        empty.ok = FALSE, add = coll)

    checkmate::assert_subset(level, choices = c("survey", "point"), empty.ok = FALSE,
        add = coll)

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    # subset
    obs_subset <- filter_observations(observations, survey_ref, specify_area = specify_area,
        specify_period = specify_period, specify_taxon = specify_taxon,
        survey_id = survey_id, area = area, period = period, taxon = taxon)

    # overwrite colnames if names are different from default
    obs_subset <- obs_subset %>%
        dplyr::mutate(survey_id = .data[[survey_id]]) %>%
        dplyr::mutate(area = .data[[area]]) %>%
        dplyr::mutate(period = .data[[period]]) %>%
        dplyr::mutate(taxon = .data[[taxon]]) %>%
        dplyr::mutate(point_id = .data[[point_id]]) %>%
        dplyr::mutate(species = .data[[species]]) %>%
        dplyr::mutate(genus = .data[[genus]]) %>%
        dplyr::mutate(family = .data[[family]]) %>%
        dplyr::mutate(abundance = .data[[abundance]])

    survey_ref <- survey_ref %>%
        dplyr::mutate(survey_id = .data[[survey_id]]) %>%
        dplyr::mutate(area = .data[[area]]) %>%
        dplyr::mutate(period = .data[[period]]) %>%
        dplyr::mutate(taxon = .data[[taxon]]) %>%
        dplyr::mutate(point_id = .data[[point_id]])



    # SURVEY-LEVEL
    if (level == "survey") {

        # extract surveys w 0 observations, relevant to filtered
        # subset
        zeros <- survey_ref %>%
            dplyr::filter((area %in% specify_area) & (period %in% specify_period) &
                (taxon %in% specify_taxon)) %>%
            dplyr::select(survey_id, area, period, taxon) %>%
            anti_join(obs_subset) %>%
            distinct()


        # No. of species per survey
        sptally <- obs_subset %>%
            dplyr::group_by(area, period, taxon, survey_id, species) %>%
            dplyr::summarise() %>%
            dplyr::group_by(area, period, taxon, survey_id) %>%
            dplyr::summarise(n = n()) %>%
        # fill in points with 0 observations
        ungroup() %>%
            dplyr::full_join(zeros, by = c(survey_id, area, period, taxon)) %>%
            dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>%
            dplyr::arrange(survey_id)


        # # note: same as using vegan::specnumber community_survey <-
        # as.data.frame.matrix(xtabs(abundance ~ survey_id + species,
        # data = obs_subset)) sptally <-
        # vegan::specnumber(community_survey, MARGIN = 1) sptally <-
        # tibble(survey_id = names(sptally), n = sptally) %>%
        # arrange(survey_id)


        # POINT-LEVEL
    } else if (level == "point") {

        # run check_taxongrps()
        rmspp <- check_taxongrps(obs_subset, level = "point")


        # extract surveys w 0 observations, relevant to filtered
        # subset
        zeros <- survey_ref %>%
            dplyr::filter((area %in% specify_area) & (period %in% specify_period) &
                (taxon %in% specify_taxon)) %>%
            dplyr::select(point_id, area, period, taxon) %>%
            anti_join(obs_subset) %>%
            distinct()


        # No. of species per survey or point
        sptally <- obs_subset %>%
            dplyr::group_by(area, period, taxon, point_id, species) %>%
            dplyr::summarise() %>%
        # exclude genus/family lvl records if all sp within grp
        # observed
        dplyr::anti_join(rmspp, by = c(species = "name", "point_id", "period")) %>%

        # final tally
        dplyr::group_by(area, period, taxon, point_id) %>%
            dplyr::summarise(n = n()) %>%
        # fill in points with 0 observations
        dplyr::ungroup() %>%
            dplyr::full_join(zeros, by = c(point_id, area, period, taxon)) %>%
            dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>%
        dplyr::arrange(point_id)


        # # note: same as using vegan::specnumber with community
        # matrix community_point <-
        # as.data.frame.matrix(xtabs(abundance ~ point_id + species,
        # data = obs_subset)) # exclude genus/family-lvl records
        # present in genus_areas & family_areas community_point <-
        # community_point[, !(colnames(community_point) %in%
        # genus_pts$genus)] community_point <- community_point[,
        # !(colnames(community_point) %in% family_pts$family)]
        # sptally <- vegan::specnumber(community_point, MARGIN = 1)
        # sptally <- tibble(point_id = names(sptally), n = sptally)
        # %>% arrange(point_id)

    }

    return(sptally)

}
