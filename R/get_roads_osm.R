#'Get road lines from OpenStreetMap
#'
#'Download and process OpenStreetMap (OSM) road lines within a specified geographical `place`,
#'from the [Geofabrik database](https://download.geofabrik.de). It is a wrapper around
#'functions in the package [`osmextract`](https://docs.ropensci.org/osmextract/index.html), and
#'processes the downloaded files for subsequent analyses. Refer to package `osmextract` for
#'more details and options for input arguments when downloading the data.
#'
#'OSM lines are filtered by key-value attributes, where `highway:` is
#'`motorway`, `trunk`, `primary`, `secondary`, `tertiary`, `unclassified` or `residential`,
#'or their respective `*_link` values.
#'
#'@param place `sf` object (with projected coordinate reference system). Geographical area to match with the (`.osm.pbf`) file in the data archive.
#'Argument passed to `osmextract::oe_match()`.
#'@param date Date of OSM data snapshot to download. Refer to https://download.geofabrik.de
#'for the specific dates available. Defaults to `NULL` (download the latest available data).
#'@param dir_raw character. Directory to download the raw unprocessed OSM data. Passed to
#'argument `download_directory` in `osmextract::oe_read()`.
#'@param filename character (optional). File path to export output data (GeoJSON format).
#'@param ... Other arguments passed to `osmextract::oe_read()`.
#'
#'@return The processed road lines (`sf` object).
#'
#'@import checkmate
#'@importFrom sf st_transform st_as_sf st_make_valid st_as_text st_geometry st_crs st_is_empty st_write
#'@importFrom osmextract oe_download_directory oe_match oe_read
#'@importFrom dplyr filter
#'@importFrom rlang .data
#'
#'@export
get_roads_osm <- function(place, date = NULL, dir_raw = osmextract::oe_download_directory(),
    filename = NULL, ...) {

    # Error checking ------------------

    coll <- checkmate::makeAssertCollection()

    # data type
    checkmate::assert_date(date, any.missing = FALSE, all.missing = FALSE,
        len = 1, null.ok = TRUE, add = coll)

    # file paths
    checkmate::assert_character(filename, min.len = 1, any.missing = FALSE,
        all.missing = FALSE, null.ok = TRUE, add = coll)

    checkmate::reportAssertions(coll)


    # Calculations ------------------

    # form url link ----
    link <- osmextract::oe_match(place, provider = "geofabrik")$url  # download region

    if (!is.null(date)) {
        date <- format(date, "%y%m%d")
        link <- gsub("latest", date, link)  # change download date if provided
    }


    # parameters to filter data after download ----
    # https://wiki.openstreetmap.org/wiki/Key:highway
    osmkeys <- c("lanes", "width", "maxheight", "maxspeed", "oneway", "surface",
        "sidewalk", "ref", "destination") # highway is already a default key

    q <- "SELECT * FROM 'lines' WHERE highway IN ('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential', 'motorway_link', 'trunk_link', 'primary_link', 'secondary_link', 'tertiary_link')"
    # special: highway:service (e.g. road to carparks, etc.)

    # to extract features that intersect bounding box (geographic
    # crs)
    bb <- sf::st_transform(place, st_crs(4326)) %>%
        sf::st_geometry() %>%
        sf::st_as_text()



    # download and filter data ---- filter by st_read() instead of
    # using vectortranslate_option
    results <- osmextract::oe_read(link, download_directory = dir_raw,
        layer = "lines", extra_tags = osmkeys, force_vectortranslate = TRUE,
        query = q, wkt_filter = bb)


    # clean up ---- remove empty geoms, transform back to same crs as
    # 'place'
    suppressWarnings(results <- results %>%
        dplyr::filter(!sf::st_is_empty(.)) %>%
        sf::st_transform(sf::st_crs(place)) %>%
        sf::st_make_valid())


    # export ----
    if (!is.null(filename)) {
        sf::st_write(results, filename, driver = "GeoJSON", delete_dsn = TRUE,
            overwrite = TRUE)
    }

    rm(link, osmkeys, q, bb)

    return(results)
}













