#' Sub-areas of interest where sampling points were generated
#'
#' Forest patches within the six towns (areas) in Singapore: Punggol (PG), Queenstown (QT),
#' Tampines (TP), Jurong West (JW), Bishan (BS) and Woodlands (WL).
#'
#' @docType data
#'
#' @usage data(sampling_forests)
#'
#' @format Tabular [`sf`](https://r-spatial.github.io/sf/index.html) object.
#' Each row has a geometry of type `POLYGON`.
#' For each sampling `period` (column), all forests patches within each area (town) were surveyed.
#'
#' @keywords datasets
#'
#' @source Development of a Biodiversity Index for Residential Towns using Biodiversity Field Surveys.
#' 14 June 2016, 6.5 years, Ministry of National Development Research Fund (MNDRF) Grant.
#' Awarded to the National University of Singapore (host institution) and the Singapore Housing & Development Board.
#'
#' @seealso
#' * [sampling_areas] are the encompassing areas of interest where these forest patches are located within.
#'
#' @examples
#' head(sampling_forests)
#'
"sampling_forests"
