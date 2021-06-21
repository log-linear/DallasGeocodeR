
# Geocoder Service URL
.base_url <- "https://gis.dallascityhall.com/wwwgis/rest/services/ToolServices"

.sleep <- 1  # Time in seconds to wait between loops

#' Convert City of Dallas addresses to lat/long coordinates
#'
#' Find lat/long coordinates for a given set of addresses using the City of
#' Dallas's public \href{https://gis.dallascityhall.com/wwwgis/rest/services/
#' ToolServices/DallasStreetsLocator/GeocodeServer/geocodeAddresses}{
#' geocodeAddresses service}. See ArcGIS's \href{https://developers.arcgis.com/
#' rest/geocode/api-reference/geocoding-geocode-addresses.htm}{documentation
#' page} for additional details.
#'
#' @param street character vector of street addresses to geocode.
#' @param city optional, character vector of city names to geocode.
#' @param zip optional, vector of postal codes to geocode.
#' @param id optional, numeric vector uniquely identifying each address.
#'   providing this argument note that each integer \emph{must} be unique.
#' @param server GeocodeServer to use. Defaults to DallasStreetsLocator.
#'   Refer to the \href{https://gis.dallascityhall.com/wwwgis/rest/services/
#'   ToolServices}{ToolServices} API directory for details on each server.
#' @param out_sr Spatial Reference ID (WKID) for the desired lat/long
#'   coordinate system. Defaults to 4326 (WGS84). If using with one of the City
#'   of Dallas's MapServers, you may wish to set this to 2276, the default WKID
#'   for many Dallas services. Refer to ArcGIS's \href{https://developers.arcgis
#'   .com/rest/services-reference/enterprise/projected-coordinate-systems.htm}{
#'   Projected} and \href{https://developers.arcgis.com/rest/services-reference/
#'   enterprise/geographic-coordinate-systems.htm}{Geographic} coordinate system
#'   documentation pages for a full list of valid WKIDs.
#' @param output output of this function. If \code{latlong} (default), will
#'   return a dataframe of lat/long coordinates and addresses. If \code{all},
#'   will return the full JSON response from the server as a list.
#'
#' @return A dataframe or list of geocoded address results.
#'
#' @examples
#' addresses <- data.frame(
#'   street = c('8525 Garland Rd', '1500 Marilla St', '3809 Grand Avenue'),
#'   city = c('Dallas', 'Dallas', 'Dallas'),
#'   zip = c(75218, 75201, 75210)
#' )
#' geocode_addresses(addresses$street, addresses$city, addresses$zip)
#'
#' geocode_addresses(addresses$street)
#'
#' @export
geocode_addresses <- function(street, city = NULL, zip = NULL, id =
  seq_along(street), server = c("DallasStreetsLocator", "ParcelLocator",
  "AccountPointsLocator", "AccountpointsStreetLocator"), out_sr = 4326,
  output = c("latlong", "all"))
{
  n_addresses <- length(street)

  # Input validation
  street[is.na(street)] <- ""

  if (is.null(city)) city <- rep("", n_addresses)
  else city[is.na(city)] <- ""

  if (is.null(zip)) zip <- rep("", n_addresses)
  else {
    zip <- as.character(zip)
    zip[is.na(zip)] <- ""
  }

  id <- as.numeric(id)
  server <- match.arg(server)
  output <- match.arg(output)

  # Main function loop
  results <- list()
  geocoder_url <- paste(.base_url, server, "GeocodeServer", "geocodeAddresses",
                        sep = "/")
  indices <- seq_along(id)
  batches <- split(indices, ceiling(indices / 1000))
  batch_cnt <- 1

  for (batch in batches) {
    print(paste("Processing batch", batch_cnt, "of", length(batches)))

    # Create JSON payload
    nested_list <- list(
      records = lapply(batch,
                       function(i) list(attributes = list(OBJECTID = id[[i]],
                                                          Street = street[[i]],
                                                          City = city[[i]],
                                                          ZIP = zip[[i]])))
    )
    json <- rjson::toJSON(nested_list)
    params <- list(
      addresses = json,
      f = "json",
      outSR = out_sr
    )

    # Submit request
    request <- httr::POST(url = geocoder_url, body = params, encode = "form")

    # Extract content
    response <- httr::content(request, "parsed", "application/json")
    if (!is.null(response$error)) stop(response$error$message)

    results[[batch_cnt]] <- response

    Sys.sleep(.sleep)
    batch_cnt <- batch_cnt + 1
  }

  if (output == "latlong") {
    # Pre-allocate function output variables
    latitude <- numeric(n_addresses)
    longitude <- numeric(n_addresses)
    score <- numeric(n_addresses)
    status <- character(n_addresses)
    matched_address <- character(n_addresses)
    address_type <- character(n_addresses)

    idx <- 1
    for (i in seq_along(results)) {
      locations <- results[[i]]$locations

      for (j in seq_along(locations)) {
        latitude[idx] <- as.numeric(locations[[j]]$location$y)
        longitude[idx] <- as.numeric(locations[[j]]$location$x)
        score[idx] <- locations[[j]]$attributes$Score
        status[idx] <- locations[[j]]$attributes$Status
        matched_address[idx] <- locations[[j]]$address
        address_type[idx] <- locations[[j]]$attributes$Addr_type

        idx <-  idx + 1
      }
    }

    results <- data.frame(id, latitude, longitude, score, status,
                          matched_address, address_type)
  }

  return(results)
}

#' Convert lat/long coordinates to City of Dallas addresses
#'
#' Convert lat/long coordinates to City of Dallas addresses using the
#' City of Dallas's public \href{https://gis.dallascityhall.com/wwwgis/rest/
#' services/ToolServices/DallasStreetsLocator/GeocodeServer/reverseGeocode}{
#' reverseGeocode service}. See ArcGIS's \href{https://developers.arcgis.com/
#' rest/geocode/api-reference/geocoding-reverse-geocode.htm}{documentation page}
#' for additional details.
#'
#' @param latitude numeric vector of latitude coordinate to reverse geocode
#' @param longitude numeric vector of longitude coordinate to reverse geocode
#' @param intersection boolean value specifying whether the geocode service
#'   should return the nearest street intersection or the nearest address.
#' @param out_sr Spatial Reference ID (WKID) for the given lat/long
#'   coordinate system. Defaults to 4326, which corresponds to WGS84. If using
#'   with one of the City of Dallas's MapServers, you may wish to set this to
#'   2276, the default WKID for many Dallas services. Refer to ArcGIS's
#'   \href{https://developers.arcgis.com/rest/services-reference/enterprise/
#'   projected-coordinate-systems.htm}{Projected} and \href{https://developers.
#'   arcgis.com/rest/services-reference/enterprise/geographic-coordinate-systems
#'   .htm}{Geographic} coordinate system documentation pages for a full list of
#'   valid WKIDs.
#'
#' @return A dataframe containing the reverse-geocoded address
#'
#' @examples
#' reverse_geocode(32.8217, -96.7163)
#'
#' addresses <- data.frame(
#'   street = c('8525 Garland Rd', '1500 Marilla St', '3809 Grand Avenue'),
#'   city = c('Dallas', 'Dallas', 'Dallas'),
#'   zip = c(75218, 75201, 75210)
#' ) #'
#' coords <- geocode_addresses(addresses$street, addresses$city, addresses$zip)
#' reverse_geocode(coords$latitude[[2]], coords$longitude[[2]]
#'
#' @export
reverse_geocode <- function(latitude, longitude, intersection = F, sr = 4326) {

  # Input validation
  latitude <- as.numeric(latitude)
  longitude <- as.numeric(longitude)
  intersection <- ifelse(intersection == T, "true", "false")

  # Main function loop
  geocoder_url <- paste(.base_url, "DallasStreetsLocator", "GeocodeServer",
                        "reverseGeocode", sep = "/")
  n_coords <- length(latitude)
  address <- character(n_coords)

  for (i in 1:n_coords) {

    # Create JSON payload
    nested_list <- list(y = latitude[[i]], x = longitude[[i]],
                        spatialReference = list(wkid = sr))
    json <- rjson::toJSON(nested_list)
    params <- list(
      location = json,
      returnIntersection = intersection,
      f = "json"
    )

    # Submit request
    request <- httr::POST(geocoder_url, body = params, encode = "form")

    # Extract content
    response <- httr::content(request, "parsed", "application/json")

    tryCatch(
      address[[i]] <- response$address$Match_addr,
      error = function(cond) address[[i]] <- NA
    )

    Sys.sleep(.sleep)
  }

  # Final result
  results <- data.frame(address, latitude, longitude)
  return(results)
}

#' Find address candidates for City of Dallas addresses
#'
#' Find probable City of Dallas addresses for a given address using the City of
#' Dallas's public \href{https://gis.dallascityhall.com/wwwgis/rest/services/
#' ToolServices/DallasStreetsLocator/GeocodeServer/findAddressCandidates}{
#' findAddressCandidates service}. See ArcGIS's \href{https://developers.arcgis
#' .com/rest/geocode/api-reference/geocoding-find-address-candidates.htm}{
#' documentation page} for additional details on this service.
#'
#' @param street character vector of street addresses to match against
#' @param city optional, character vector of city names to match against
#' @param zip optional, vector of postal codes to match against
#' @param max_locs optional, maximum number of address candidates to return
#' @param server GeocodeServer to use. Defaults to DallasStreetsLocator.
#'   Refer to the \href{https://gis.dallascityhall.com/wwwgis/rest/services/
#'   ToolServices}{ToolServices} API directory for details on each server.
#' @param out_sr Spatial Reference ID (WKID) for the desired lat/long
#'   coordinate system. Defaults to 4326 (WGS84). If using with one of the City
#'   of Dallas's MapServers, you may wish to set this to 2276, the default WKID
#'   for many Dallas services. Refer to ArcGIS's \href{https://developers.arcgis
#'   .com/rest/services-reference/enterprise/projected-coordinate-systems.htm}{
#'   Projected} and \href{https://developers.arcgis.com/rest/services-reference/
#'   enterprise/geographic-coordinate-systems.htm}{Geographic} coordinate system
#'   documentation pages for a full list of valid WKIDs.
#'
#' @return A dataframe of address candidates
#'
#' @examples
#' addresses <- data.frame(
#'   street = c('8525 Garland Rd', '1500 Marilla St', '3809 Grand Avenue'),
#'   city = c('Dallas', 'Dallas', 'Dallas'),
#'   zip = c(75218, 75201, 75210)
#' )
#' find_address_candidates(addresses$street, addresses$city, addresses$zip)
#'
#' find_address_candidates("1500 Marilla St", max_locs = 1)
#'
#' @export
find_address_candidates <- function(street, city = NULL, zip = NULL,
  max_locs = NULL, server = c("DallasStreetsLocator", "ParcelLocator",
  "AccountPointsLocator", "AccountpointsStreetLocator"), out_sr = 4326)
{
  n_addresses <- length(street)

  street[is.na(street)] <- ""

  if (is.null(city)) city <- rep("", n_addresses)
  else city[is.na(city)] <- ""

  if (is.null(zip)) zip <- rep("", n_addresses)
  else zip[is.na(zip)] <- ""

  server <- match.arg(server)

  # Main function loop
  geocoder_url <- paste(.base_url, server, "GeocodeServer",
                        "findAddressCandidates", sep = "/")
  addresses <- list()

  for (i in 1:n_addresses) {

    # Submit request
    params <- list(
      Street = street[[i]],
      City = city[[i]],
      ZIP = zip[[i]],
      outSR = out_sr,
      f = "json"
    )
    if (!is.null(max_locs)) params <- c(params, maxLocations = max_locs)

    request <- httr::POST(geocoder_url, body = params, encode = "form")

    # Extract content
    response <- httr::content(request, "parsed", "application/json")
    if (!is.null(response$error)) stop(response$error$message)

    candidates <- response$candidates
    n_candidates <- length(candidates)
    if (n_candidates == 0) stop("No address candidates found.")

    candidate <- numeric(n_candidates)
    address <- character(n_candidates)
    latitude <- numeric(n_candidates)
    longitude <- numeric(n_candidates)
    score <- numeric(n_candidates)

    for (j in 1:n_candidates) {
      candidate[j] <- j
      address[j] <- candidates[[j]]$address
      latitude[j] <- candidates[[j]]$location$y
      longitude[j] <- candidates[[j]]$location$x
      score[j] <- candidates[[j]]$score
    }

    # Convert nested list attributes into dataframe
    addresses[[i]] <- data.frame(candidate, address, latitude, longitude,
                                 score)
    Sys.sleep(.sleep)
  }
  results <- data.frame(do.call(rbind.data.frame, addresses))

  return(results)
}
