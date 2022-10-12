#' Sampling points where animal surveys were conducted
#'
#' Point locations where animal surveys were conducted within the six towns (areas) in Singapore,
#' during two sampling periods. The six towns were Punggol (PG), Queenstown (QT), Tampines (TP), Jurong West (JW), Bishan (BS)
#' and Woodlands (WL).
#'
#' @docType data
#'
#' @usage data(sampling_points)
#'
#' @format Tabular [`sf`](https://r-spatial.github.io/sf/index.html) object.
#' Each row has a geometry of type `POINT`.
#'
#' @details Points were randomly generated within each town (`area`),
#' stratified by `landcover` type (either `Urban` or `Forest`).
#' Each sampling `period` stretched across a year-long duration;
#' some points were sampled in both periods.
#'
#' @keywords datasets
#'
#' @source Development of a Biodiversity Index for Residential Towns using
#' Biodiversity Field Surveys, 2016–2022. Ministry of National Development Research Fund
#' (MNDRF) Grant. Awarded to the National University of Singapore and the Singapore Housing & Development Board.
#'
#' @seealso
#' * [sampling_areas] where these sampling points were generated within.
#' * [animal_surveys] containing information about the surveys conducted at these sampling points.
#'
#' @examples
#' head(sampling_points)
#'
"sampling_points"
