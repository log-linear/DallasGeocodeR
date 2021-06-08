
# Geocoder Service URL
.base_url <- "https://gis.dallascityhall.com/wwwgis/rest/services/ToolServices"

#' Convert City of Dallas addresses to lat/long coordinates
#'
#' Calculate lat/long coordinates for a given set of addresses using the
#' City of Dallas's public \href{https://gis.dallascityhall.com/wwwgis/rest/
#' services/ToolServices/DallasStreetsLocator/GeocodeServer/geocodeAddresses}{
#' geocodeAddresses service}. See ArcGIS's
#' \href{https://developers.arcgis.com/rest/geocode/api-reference/
#' geocoding-geocode-addresses.htm}{documentation page} for additional details.
#'
#' @param street character vector of street addresses to geocode.
#' @param city optional, character vector of city names to geocode.
#' @param zip optional, character vector of postal codes to geocode.
#' @param id optional, numeric vector uniquely identifying each address, If
#'   providing this argument (e.g., to join onto an existing dataframe), note
#'   that each integer must be unique.
#' @param server GeocodeServer to use. Defaults to DallasStreetsLocator.
#'   Refer to the \href{https://gis.dallascityhall.com/wwwgis/rest/services/
#'   ToolServices}{ToolServices} API directory for details on each server.
#' @param out_sr Spatial Reference ID (WKID) for the desired lat/long
#'   coordinate system. Defaults to 4326, which corresponds to
#'   \href{https://wiki.gis.com/wiki/index.php/WGS84}{WGS84}. If the coordinates
#'   will be used with the City of Dallas's MapServers, you may wish to set this
#'   to 2276, the default WKID for many Dallas services. Refer to the
#'   \href{https://developers.arcgis.com/rest/services-reference/enterprise/
#'   projected-coordinate-systems.htm}{Projected} and \href{https://developers.
#'   arcgis.com/rest/services-reference/enterprise/geographic-coordinate-systems
#'   .htm}{Geographic} coordinate system documentation pages for a full list of
#'   valid WKIDs.
#' @param output Function output. If \code{latlong} (default), function will
#'   return a dataframe of lat/long coordinates, addresses, and a few other
#'   fields. If \code{all}, will return a list corresponding to the full JSON
#'   response from the server.
#'
#' @return A dataframe or list of geocoded address results.
#'
#' @examples
#' addresses <- data.frame(
#'   street = c('8525 Garland Rd', '1500 Marilla St', '3809 Grand Avenue'),
#'   city = c('Dallas', 'Dallas', 'Dallas'),
#'   zip = c('75218', '75201', '75210')
#' )
#'
#' geocode_addresses(addresses$street, addresses$city, addresses$zip)
#' geocode_addresses(addresses$street)
#'
#' @export
geocode_addresses <- function(street, city = NULL, zip = NULL, id = seq(street),
  server = c("DallasStreetsLocator", "ParcelLocator", "AccountPointsLocator",
  "AccountpointsStreetLocator"), out_sr = 4326, output = c("latlong", "all"))
{
  sleep <- 2  # Time in seconds to wait between loops
  n_addresses <- length(street)

  # Input validation
  street[is.na(street)] <- ""

  if (is.null(city)) city <- rep("", n_addresses)
  else city[is.na(city)] <- ""

  if (is.null(zip)) zip <- rep("", n_addresses)
  else zip[is.na(zip)] <- ""

  id <- as.numeric(id)
  server <- match.arg(server)
  output <- match.arg(output)

  # Pre-allocate function output variables
  if (output == "latlong") {
    object_id <- numeric(n_addresses)
    latitude <- numeric(n_addresses)
    longitude <- numeric(n_addresses)
    score <- numeric(n_addresses)
    status <- character(n_addresses)
    address <- character(n_addresses)
    address_type <- character(n_addresses)
  }
  else if (output == "all") results <- list()

  # Main function loop
  geocoder_url <- paste(.base_url, server, "GeocodeServer", "geocodeAddresses",
                        sep = "/")
  batches <- split(id, ceiling(seq(id) / 1000))
  batch_cnt <- 1

  for (batch in batches) {
    print(paste("Processing batch", batch_cnt, "of", length(batches)))

    batch_size <- length(batch)

    # Create JSON payload
    nested_list <- list(
      records = lapply(seq(batch),
                       function(i) list(attributes = list(OBJECTID = batch[[i]],
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

    if (output == "latlong") {
      locations <- response$locations

      for (i in 1:batch_size) {
        idx <- batch[[i]]

        object_id[idx] <- idx
        latitude[idx] <- locations[[i]]$location$y
        longitude[idx] <- locations[[i]]$location$x
        score[idx] <- locations[[i]]$attributes$Score
        status[idx] <- locations[[i]]$attributes$Status
        address[idx] <- locations[[i]]$address
        address_type[idx] <- locations[[i]]$attributes$Addr_type
      }

    }
    else if (output == "all") results[[batch_cnt]] <- response

    Sys.sleep(sleep)
    batch_cnt <- batch_cnt + 1
  }

  if (output == "latlong") {
    results <- data.frame(id = object_id, latitude, longitude, score,
                          status, address, address_type)
  }

  return(results)
}

#' Convert lat/long coordinates to City of Dallas addresses
#'
#' Convert lat/long coordinates to City of Dallas addresses using the
#' City of Dallas's public \href{https://gis.dallascityhall.com/wwwgis/rest/
#' services/ToolServices/DallasStreetsLocator/GeocodeServer/reverseGeocode}{
#' reverseGeocode service}. See ArcGIS's
#' \href{https://developers.arcgis.com/rest/geocode/api-reference/
#' geocoding-reverse-geocode.htm}{documentation page} for
#' additional details.
#'
#' @param latitude scalar, latitude coordinate to reverse geocode
#' @param longitude scalar, longitude coordinate to reverse geocode
#' @param intersection boolean value specifying whether the geocode service
#'   should return the nearest street intersection or the nearest address.
#' @param sr Spatial Reference ID (WKID) for the provided lat/long
#'   coordinate system. Defaults to 4326, which corresponds to
#'   \href{https://wiki.gis.com/wiki/index.php/WGS84}{WGS84}. If the addresses
#'   will be used with the City of Dallas's MapServers, you may wish to set this
#'   to 2276, the default WKID for many Dallas services. Refer to the
#'   \href{https://developers.arcgis.com/rest/services-reference/enterprise/
#'   projected-coordinate-systems.htm}{Projected} and \href{https://developers.
#'   arcgis.com/rest/services-reference/enterprise/geographic-coordinate-systems
#'   .htm}{Geographic} coordinate system documentation pages for a full list of
#'   valid WKIDs.
#'
#' @return A dataframe containingt the reverse-geocoded address
#'
#' @examples
#' reverse_geocode(32.8217, -96.7163)
#'
#' addresses <- data.frame(
#'   street = c('8525 Garland Rd', '1500 Marilla St', '3809 Grand Avenue'),
#'   city = c('Dallas', 'Dallas', 'Dallas'),
#'   zip = c('75218', '75201', '75210')
#' )
#'
#' coords <- geocode_addresses(addresses$street, addresses$city, addresses$zip)
#' reverse_geocode(coords$latitude[[2]], coords$longitude[[2]]
#'
#' @export
reverse_geocode <- function(latitude, longitude, intersection = F, sr = 4326) {
  sleep <- 1  # Time in seconds to wait between loops

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
    if (!is.null(response$error)) stop(response$error$message)

    address[[i]] <- response$address$Match_addr

    Sys.sleep(sleep)
  }

  # Final result
  results <- data.frame(address, latitude, longitude)
  return(results)
}

#' Find address candidates for City of Dallas addresses
#'
#' Generate a list of probable City of Dallas addresses for a given address
#' using the City of Dallas's public \href{https://gis.dallascityhall.com/
#' wwwgis/rest/services/ToolServices/DallasStreetsLocator/GeocodeServer/
#' findAddressCandidates}{findAddressCandidates}. Note that this operation can
#' only process one address per function call. See ArcGIS's
#' \href{https://developers.arcgis.com/rest/geocode/api-reference/
#' geocoding-find-address-candidates.htm}{documentation page} for additional
#' details on this service.
#'
#' @param street street address to match against
#' @param city optional, city to match against
#' @param zip optional, postal code to match against
#' @param max_locs optional, maximum number of address candidates to return
#' @param server GeocodeServer to use. Defaults to DallasStreetsLocator.
#'   Refer to the \href{https://gis.dallascityhall.com/wwwgis/rest/services/
#'   ToolServices}{ToolServices} API directory for details on each server.
#' @param out_sr Spatial Reference ID (WKID) for the desired lat/long
#'   coordinate system. Defaults to 4326, which corresponds to
#'   \href{https://wiki.gis.com/wiki/index.php/WGS84}{WGS84}. If the coordinates
#'   will be used with the City of Dallas's MapServers, you may wish to set this
#'   to 2276, the default WKID for many Dallas services. Refer to the
#'   \href{https://developers.arcgis.com/rest/services-reference/enterprise/
#'   projected-coordinate-systems.htm}{Projected} and \href{https://developers.
#'   arcgis.com/rest/services-reference/enterprise/geographic-coordinate-systems
#'   .htm}{Geographic} coordinate system documentation pages for a full list of
#'   valid WKIDs.
#'
#' @return A dataframe of identified address candidates
#'
#' @examples
#' find_address_candidates("1500 Marilla St", max_locs = 1)
#'
#' @export
find_address_candidates <- function(street, city = NULL, zip = NULL,
  max_locs = NULL, server = c("DallasStreetsLocator", "ParcelLocator",
  "AccountPointsLocator", "AccountpointsStreetLocator"), out_sr = 4326)
{
  sleep <- 1  # Time in seconds to wait between loops
  n_addresses <- length(street)

  street[is.na(street)] <- ""

  if (is.null(city)) city <- rep("", n_addresses)
  else city[is.na(city)] <- ""

  if (is.null(zip)) zip <- rep("", n_addresses)
  else zip[is.na(zip)] <- ""

  server <- match.arg(server)

  # Main function loop
  geocoder_url <- paste(.base_url, server, "GeocodeServer", "reverseGeocode",
                        sep = "/")
  addresses <- list()

  for (i in 1:n_addresses) {

    # Submit request
    params <- list(
      SingleLine = paste(street[[i]], city[[i]], zip[[i]], sep = ", "),
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
    Sys.sleep(sleep)
  }
  results <- rbind.data.frame(addresses)

  return(results)
}
