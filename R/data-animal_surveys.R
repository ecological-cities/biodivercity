#' Animal surveys conducted at sampling points
#'
#' Details about each animal survey conducted at sampling points
#' within six towns (areas) in Singapore: Punggol (PG), Queenstown (QT), Tampines (TP),
#' Jurong West (JW), Bishan (BS) and Woodlands (WL).
#'
#' @docType data
#'
#' @usage data(animal_surveys)
#'
#' @format Dataframe
#'
#' @details The sampling points (`point_id`) were randomly generated within each town (`area`).
#' Four types of animal surveys were conducted at sampling points,
#' corresponding to four animal groups (column `taxon`): Birds (Aves), Butterflies (Lepidoptera),
#' Odonates (Odonata) and Amphibians (Amphibia).
#'
#' Each sampling `period` stretched across a year-long duration, where sampling points
#' were surveyed once every two months (`cycle` adds up to 6 per `period`).
#' Some points were sampled again in the second period (column `resampled` is `TRUE`).
#' All surveys took place between the years 2016 to 2022.
#'
#' @keywords datasets
#'
#' @source Development of a Biodiversity Index for Residential Towns using
#' Biodiversity Field Surveys, 2016–2022. Ministry of National Development Research Fund
#' (MNDRF) Grant. Awarded to the National University of Singapore and the Singapore Housing & Development Board.
#'
#' @seealso
#' * [animal_observations] which contain the time-based records of species abundance for each survey.
#' * [sampling_points] where these animal surveys were conducted.
#' * [sampling_areas] where the sampling points were generated within.
#'
#' @examples
#' head(animal_surveys)
#'
"animal_surveys"
