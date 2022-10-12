#' Sampling points with landscape data summarised at multiple buffer radii
#'
#' Examples of landscape data summarised at point locations where animal surveys
#' were conducted. These sampling points are within the six towns (areas) in Singapore,
#' during two sampling periods. The six towns were Punggol (PG), Queenstown (QT),
#' Tampines (TP), Jurong West (JW), Bishan (BS) and Woodlands (WL).
#'
#' @docType data
#'
#' @usage data(points_landscape)
#'
#' @format Dataframe containing landscape predictors (columns).
#'
#' @details Data is in 'wide format', i.e. has duplicate rows for the unique identifier
#' of points (column `point_id`), owing to the presence of multiple buffer radii per point
#' (column `radius_m`, in metres). Not all landscape predictors are summarised at all buffer radii.
#' Column names for landscape predictors are prefixed with the following:
#'
#' * `man_`: manually mapped on-site
#' * `osm_`: derived from OpenStreetMap data
#' * `lsm_`: landscape metrics based on land cover classes
#'
#' @keywords datasets
#'
#' @source Development of a Biodiversity Index for Residential Towns using
#' Biodiversity Field Surveys, 2016–2022. Ministry of National Development Research Fund
#' (MNDRF) Grant. Awarded to the National University of Singapore and the Singapore Housing & Development Board.
#'
#' @seealso
#' * [sampling_points] where the animal surveys were conducted.
#'
#' @examples
#' head(points_landscape)
#'
"points_landscape"
