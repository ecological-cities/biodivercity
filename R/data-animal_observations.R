#' Animal observation records from point surveys
#'
#' Time-based point observations of species abundance at sampling points
#' within six towns (areas) in Singapore: Punggol (PG), Queenstown (QT),
#' Tampines (TP), Jurong West (JW), Bishan (BS) and Woodlands (WL).
#'
#'
#' @docType data
#'
#' @usage data(animal_observations)
#'
#' @format Dataframe
#'
#' @details The sampling points (`point_id`) were randomly generated within each town (`area`).
#' Four types of animal surveys were conducted at sampling points,
#' corresponding to four animal groups (column `taxon`): Birds (Aves), Butterflies (Lepidoptera),
#' Odonates (Odonata) and Amphibians (Amphibia). In addition to the name of the `species` and
#' `abundance` (number observed) at the particular time point, the names of the `family` and `genus`
#' are also made available.
#'
#' Each sampling `period` stretched across a year-long duration, where sampling points
#' were sampled once every two months (`cycle` adds up to 6 per `period`).
#' Some points were sampled again in the second period (column `resampled` is `TRUE`).
#' All surveys took place between the years 2016 to 2022.
#'
#' @keywords datasets
#'
#' @references TBC
#'
#' @source TBC
#'
#' @seealso
#' * [animal_surveys] containing information about the surveys conducted.
#' * [sampling_points] where these animal surveys were conducted.
#' * [sampling_areas] where the sampling points were generated within.
#'
#' @examples
#' head(animal_observations)
#'
"animal_observations"
