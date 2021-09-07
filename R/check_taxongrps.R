#'Check taxon group-level records for subsequent removal
#'
#'Tally and list all taxon groups where all species within the group are observed,
#'either at the level of individual towns or points (and survey round).
#'
#'@param observations Dataframe of species observations.
#'It should include columns for `species`, `genus`, `family`, `town`, `round`, and `point_id`.
#'@param level Specify whether to tally by `town` or `point`.
#'@param species Column name of the species name recorded in `observations`.
#'May include group-level records (`genus` or `family`).
#'@param genus Column name of the genus name for recorded `observations`.
#'@param family Column name of the family name for recorded `observations`.
#'@param town Column name of the town specified in `observations`.
#'Defaults to `town`.
#'@param round Column name of the sampling round specified in `observations`.
#'Defaults to `round`.
#'@param point_id Column name of the unique identifier for each point in `observations`. Defaults to `point_id`.
#'
#'@return Dataframe containing the taxon group names where all species within the group are observed.
#'Includes columns for the `round` and `town`/`point_id` (depending on argument `level`), as well as
#'the number (`n`) of species in the particular taxon group.
#'
#'@import checkmate
#'@import dplyr
#'@importFrom rlang .data
#'
#'@export
check_taxongrps <- function(observations, level,
                            species = "species", genus = "genus", family = "family",
                            town = "town", round = "round", point_id = "point_id"){

  # Error checking ------------------

  coll <- checkmate::makeAssertCollection()

  # data type
  checkmate::assert_data_frame(observations, add = coll)

  # colnames
  checkmate::assert_subset(species, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(genus, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(family, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(town, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(round, choices = colnames(observations), empty.ok = FALSE, add = coll)
  checkmate::assert_subset(point_id, choices = colnames(observations), empty.ok = FALSE, add = coll)

  checkmate::reportAssertions(coll)


  # Calculations ------------------

  # overwrite colnames if names are different from default
  observations <- observations %>%
    dplyr::mutate(species = .data[[species]]) %>%
    dplyr::mutate(genus = .data[[genus]]) %>%
    dplyr::mutate(family = .data[[family]]) %>%
    dplyr::mutate(town = .data[[town]]) %>%
    dplyr::mutate(round = .data[[round]]) %>%
    dplyr::mutate(point_id = .data[[point_id]])


  # Summary tables for total no. of genus/family
  genus_all <- observations %>%
    dplyr::distinct(species, genus) %>%
    dplyr::filter(species != genus & !is.na(genus)) %>% # dont count if species name is genus-lvl or NA
    dplyr::group_by(genus) %>%
    dplyr::summarise(n_total = n())

  family_all <- observations %>%
    dplyr::distinct(species, family) %>%
    dplyr::filter(species != family & !is.na(family)) %>% # dont count if species name is family-lvl or NA
    dplyr::group_by(family) %>%
    dplyr::summarise(n_total = n())


  # town/round level: compare with summary tables, filter sp tt n = n_total
  if(level == "town"){

    genus_remove <- observations %>%
      dplyr::group_by(town, round, species, genus) %>%
      dplyr::distinct(species, genus) %>%
      dplyr::filter(species != genus & !is.na(genus)) %>% # dont count if species name is genus or NA
      dplyr::group_by(town, round, genus) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::left_join(genus_all) %>%
      dplyr::filter(n == .data$n_total) %>%
      dplyr::select(-.data$n_total) %>%
      dplyr::rename(name = "genus")

    family_remove <- observations %>%
      dplyr::group_by(town, round, species, family) %>%
      dplyr::distinct(species, family) %>%
      dplyr::filter(species != family & !is.na(family)) %>% # dont count if species name is family or NA
      dplyr::group_by(town, round, family) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::left_join(family_all) %>%
      dplyr::filter(n == .data$n_total) %>%
      dplyr::select(-.data$n_total) %>%
      dplyr::rename(name = "family")

    all_remove <- bind_rows(genus_remove, family_remove)


  # point level: compare with summary tables, filter sp tt n = n_total
  }else if(level == "point"){

    genus_remove <- observations %>%
      dplyr::group_by(point_id, round, species, genus) %>%
      dplyr::distinct(species, genus) %>%
      dplyr::filter(species != genus & !is.na(genus)) %>% # dont count if species name is genus or NA
      dplyr::group_by(point_id, round, genus) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::left_join(genus_all) %>%
      dplyr::filter(n == .data$n_total) %>%
      dplyr::select(-.data$n_total) %>%
      dplyr::rename(name = "genus")

    family_remove <- observations %>%
      dplyr::group_by(point_id, round, species, family) %>%
      dplyr::distinct(species, family) %>%
      dplyr::filter(species != family & !is.na(family)) %>% # dont count if species name is family or NA
      dplyr::group_by(point_id, round, family) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::left_join(family_all) %>%
      dplyr::filter(n == .data$n_total) %>%
      dplyr::select(-.data$n_total) %>%
      dplyr::rename(name = "family")

    all_remove <- bind_rows(genus_remove, family_remove)

  }

  return(all_remove)
}
