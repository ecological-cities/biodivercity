#'Check taxon-group-level records in survey data
#'
#'Show taxon groups where all species within the group are observed,
#'and tally the number of species within the particular group.
#'Includes option to tally either different levels (`area`
#'or `point`; column to be specified by the user).
#'Output dataframe can used for subsequent data processing
#'(e.g. filtered away from the input dataset `observations`).
#'
#'@param observations Dataframe of species observations.
#'It should include columns for `species`, `genus`, `family`, `area`, `period`, and `point_id`.
#'@param level Specify whether to tally by `area` or `point`.
#'@param species Column name of the species name recorded in `observations`.
#'May include group-level records (`genus` or `family`).
#'@param genus Column name of the genus name for recorded `observations`.
#'@param family Column name of the family name for recorded `observations`.
#'@param area Column name of the area of interest specified in `observations`.
#'Defaults to `area`.
#'@param period Column name of the sampling period specified in `observations`.
#'Defaults to `period`.
#'@param point_id Column name of the unique identifier for each point in `observations`. Defaults to `point_id`.
#'
#'@return Dataframe containing the taxon group names where all species within the group are observed.
#'Includes columns for the `period` and `area`/`point_id` (depending on argument `level`), as well as
#'the number (`n`) of species in the particular taxon group.
#'
#'@import checkmate
#'@import dplyr
#'@importFrom rlang .data
#'
#'@examples
#'
#' data(animal_observations)
#' check_taxongrps(observations = animal_observations, level = "area")
#'
#'@export
check_taxongrps <- function(observations, level,
                            species = "species", genus = "genus", family = "family",
                            area = "area", period = "period", point_id = "point_id"){

  # Error checking ------------------

  coll <- checkmate::makeAssertCollection()

  # data type
  checkmate::assert_data_frame(observations, add = coll)

  # colnames
  checkmate::assert_subset(species, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(genus, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(family, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(area, choices = colnames(observations), empty.ok = FALSE, add = coll)
  # checkmate::assert_subset(period, choices = colnames(observations), empty.ok = FALSE, add = coll) # may be absent
  checkmate::assert_subset(point_id, choices = colnames(observations), empty.ok = FALSE, add = coll)

  # level
  checkmate::assert_subset(level, choices = c("area", "point"), empty.ok = FALSE, add = coll)

  checkmate::reportAssertions(coll)


  # Calculations ------------------

  # overwrite colnames if names are different from default
  observations <- observations %>%
    dplyr::mutate(species = .data[[species]]) %>%
    dplyr::mutate(genus = .data[[genus]]) %>%
    dplyr::mutate(family = .data[[family]]) %>%
    dplyr::mutate(area = .data[[area]]) %>%
    { if(period %in% colnames(observations)) dplyr::mutate(., period = .data[[period]]) else . } %>%
    dplyr::mutate(point_id = .data[[point_id]])


  # Summary tables for total no. of genus/family
  genus_all <- observations %>%
    dplyr::ungroup() %>%
    dplyr::distinct(species, genus) %>%
    dplyr::filter(species != genus & !is.na(genus)) %>% # dont count if species name is genus-lvl or NA
    dplyr::group_by(genus) %>%
    dplyr::summarise(n_total = n())

  family_all <- observations %>%
    dplyr::ungroup() %>%
    dplyr::distinct(species, family) %>%
    dplyr::filter(species != family & !is.na(family)) %>% # dont count if species name is family-lvl or NA
    dplyr::group_by(family) %>%
    dplyr::summarise(n_total = n())


  # area/period level: compare with summary tables, filter sp tt n = n_total
  if(level == "area"){

    genus_remove <- observations %>%
      { if(period %in% colnames(observations)) dplyr::group_by(., area, period, species, genus) else dplyr::group_by(., area, species, genus) } %>%
      dplyr::distinct(species, genus) %>%
      dplyr::filter(species != genus & !is.na(genus)) %>% # dont count if species name is genus or NA
      { if(period %in% colnames(observations)) dplyr::group_by(., area, period, genus) else dplyr::group_by(., area, genus) } %>%
      dplyr::summarise(n = n()) %>%
      dplyr::left_join(genus_all, by = "genus") %>%
      dplyr::filter(n == .data$n_total) %>%
      dplyr::select(-.data$n_total) %>%
      dplyr::rename(name = "genus") %>%
      { if(period %in% colnames(observations)) dplyr::rename(., !!period := period) else . } %>% # rename back to original colname
      dplyr::rename(!!area := area)

    family_remove <- observations %>%
      { if(period %in% colnames(observations)) dplyr::group_by(., area, period, species, family) else dplyr::group_by(., area, species, family) } %>%
      dplyr::distinct(species, family) %>%
      dplyr::filter(species != family & !is.na(family)) %>% # dont count if species name is family or NA
      { if(period %in% colnames(observations)) dplyr::group_by(., area, period, family) else dplyr::group_by(., area, family) } %>%
      dplyr::summarise(n = n()) %>%
      dplyr::left_join(family_all, by = "family") %>%
      dplyr::filter(n == .data$n_total) %>%
      dplyr::select(-.data$n_total) %>%
      dplyr::rename(name = "family") %>%
      { if(period %in% colnames(observations)) dplyr::rename(., !!period := period) else . } %>% # rename back to original colname
      dplyr::rename(!!area := area)

    all_remove <- bind_rows(genus_remove, family_remove)


  # point level: compare with summary tables, filter sp tt n = n_total
  }else if(level == "point"){

    genus_remove <- observations %>%
      { if(period %in% colnames(observations)) dplyr::group_by(., point_id, period, species, genus) else dplyr::group_by(., point_id, species, genus) } %>%
      dplyr::distinct(species, genus) %>%
      dplyr::filter(species != genus & !is.na(genus)) %>% # dont count if species name is genus or NA
      { if(period %in% colnames(observations)) dplyr::group_by(., point_id, period, genus) else dplyr::group_by(., point_id, genus) } %>%
      dplyr::summarise(n = n()) %>%
      dplyr::left_join(genus_all, by = "genus") %>%
      dplyr::filter(n == .data$n_total) %>%
      dplyr::select(-.data$n_total) %>%
      dplyr::rename(name = "genus") %>%
      { if(period %in% colnames(observations)) dplyr::rename(., !!period := period) else . } %>%
      dplyr::rename(!!point_id := point_id)

    family_remove <- observations %>%
      { if(period %in% colnames(observations)) dplyr::group_by(., point_id, period, species, family) else dplyr::group_by(., point_id, species, family) } %>%
      dplyr::distinct(species, family) %>%
      dplyr::filter(species != family & !is.na(family)) %>% # dont count if species name is family or NA
      { if(period %in% colnames(observations)) dplyr::group_by(., point_id, period, family) else dplyr::group_by(., point_id, family) } %>%
      dplyr::summarise(n = n()) %>%
      dplyr::left_join(family_all, by = "family") %>%
      dplyr::filter(n == .data$n_total) %>%
      dplyr::select(-.data$n_total) %>%
      dplyr::rename(name = "family") %>%
      { if(period %in% colnames(observations)) dplyr::rename(., !!period := period) else . } %>%
      dplyr::rename(!!point_id := point_id)

    all_remove <- bind_rows(genus_remove, family_remove)

  }

  return(all_remove)
}
