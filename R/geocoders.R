
# Geocoder Service URL
.base_url <- "https://gis.dallascityhall.com/wwwgis/rest/services/ToolServices"

#' Convert City of Dallas addresses to lat/long coordinates
#'
#' Calculate lat/long coordinates for a given set of addresses using the
#' City of Dallas's public \href{https://gis.dallascityhall.com/wwwgis/rest/
#' services/ToolServices/DallasStreetsLocator/GeocodeServer/geocodeAddresses}{
#' geocodeAddresses service}. Note that this service can geocode a
#' \strong{maximum 1000 addresses} per function call. See ArcGIS's
#' \href{https://developers.arcgis.com/rest/geocode/api-reference/
#' geocoding-geocode-addresses.htm}{documentation page} for additional details.
#'
#' @param street character vector of street addresses to geocode.
#' @param city optional, character vector of city names to geocode.
#' @param zip optional, character vector of postal codes to geocode.
#' @param id optional, numeric vector uniquely identifying each address, If
#'   providing this argument (e.g., to join onto an existing dataframe), note
#'   that each integer must be unique.
#' @param server GeocodeServer to use. Refer to the
#'   \href{https://gis.dallascityhall.com/wwwgis/rest/services/
#'   ToolServices}{ToolServices} API directory for details on each server.
#' @param out_sr Spatial Reference ID (WKID) for the desired lat/long
#'   coordinate system. Defaults to 4326, which corresponds to
#'   \href{https://wiki.gis.com/wiki/index.php/WGS84}{WGS84}. If the coordinates
#'   will be used with the City of Dallas's MapServers, you may wish to set this
#'   to \strong{2276}, the default WKID for many Dallas services. Refer to the
#'   \href{https://developers.arcgis.com/rest/services-reference/enterprise/
#'   projected-coordinate-systems.htm}{Projected} and \href{https://developers.
#'   arcgis.com/rest/services-reference/enterprise/geographic-coordinate-systems
#'   .htm}{Geographic} coordinate system documentation pages for a full list of
#'   valid WKIDs.
#'
#' @return A \code{data.frame} of geocoded address results. Includes original
#'   address alongside generated lat/long coordinates.
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
  "AccountpointsStreetLocator"), out_sr = 4326)
{
  batch_size <- 1000
  n_addresses <- length(street)

  # Input validation
  if (n_addresses > batch_size) {
    stop(paste("geocode_addresses can only process a maximum",
               batch_size,
               "addresses at once."))
  }

  street[is.na(street)] <- ""

  if (is.null(city)) city <- rep("", n_addresses)
  else city[is.na(city)] <- ""

  if (is.null(zip)) zip <- rep("", n_addresses)
  else zip[is.na(zip)] <- ""

  id <- as.numeric(id)

  server = match.arg(server)

  # Create JSON payload
  nested_list <- list(
    records = lapply(seq(id),
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
  geocoder_url <- paste(.base_url, server, "GeocodeServer", "geocodeAddresses",
                        sep = "/")
  request <- httr::POST(
    url = geocoder_url,
    body = params,
    encode = "form"
  )

  # Extract content
  response <- httr::content(request, "parsed", "application/json")
  if (!is.null(response$error)) stop(response$error$message)

  locations <- response$locations
  attributes <- lapply(
    seq(id),
    function(i) list(
      id = id[[i]],
      latitude = locations[[i]]$location$y,
      longitude = locations[[i]]$location$x,
      score = locations[[i]]$attributes$Score,
      status = locations[[i]]$attributes$Status,
      address = locations[[i]]$address,
      address_type = locations[[i]]$attributes$Addr_type
    )
  )

  # Convert nested list attributes into dataframe
  results <- data.frame(do.call(rbind.data.frame, attributes))
  return(results)
}

#' Convert lat/long coordinates to City of Dallas addresses
#'
#' Convert lat/long coordinates to City of Dallas addresses using the
#' City of Dallas's public \href{https://gis.dallascityhall.com/wwwgis/rest/
#' services/ToolServices/DallasStreetsLocator/GeocodeServer/reverseGeocode}{
#' reverseGeocode service}. Note that this service can only convert a single
#' set of coordinates per function call. See ArcGIS's
#' \href{https://developers.arcgis.com/rest/geocode/api-reference/
#' geocoding-reverse-geocode.htm}{documentation page} for
#' additional details.
#'
#' @param latitude scalar, latitude coordinate to reverse geocode
#' @param longitude scalar, longitude coordinate to reverse geocode
#' @param intersection logical boolean value specifying whether the
#'   geocode service should return the nearest street intersection or the
#'   nearest address to the given point. Default is FALSE.
#' @param server GeocodeServer to use. Refer to the
#'   \href{https://gis.dallascityhall.com/wwwgis/rest/services/
#'   ToolServices}{ToolServices} API directory for details on each server.
#' @param sr Spatial Reference ID (WKID) for the provided lat/long
#'   coordinate system. Defaults to 4326, which corresponds to
#'   \href{https://wiki.gis.com/wiki/index.php/WGS84}{WGS84}. If the addresses
#'   will be used with the City of Dallas's MapServers, you may wish to set this
#'   to \strong{2276}, the default WKID for many Dallas services. Refer to the
#'   \href{https://developers.arcgis.com/rest/services-reference/enterprise/
#'   projected-coordinate-systems.htm}{Projected} and \href{https://developers.
#'   arcgis.com/rest/services-reference/enterprise/geographic-coordinate-systems
#'   .htm}{Geographic} coordinate system documentation pages for a full list of
#'   valid WKIDs.
#'
#' @return A single row \code{data.frame} of the reverse-geocoded address with
#'   columns \code{street}, \code{city}, and \code{zip}
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
reverse_geocode <- function(latitude, longitude, intersection = F,
  server = c("DallasStreetsLocator", "ParcelLocator", "AccountPointsLocator",
  "AccountpointsStreetLocator"), sr = 4326)
{
  # Input validation
  if (any(length(latitude) > 1, length(longitude) > 1,
          length(intersection) > 1)) {
    stop(paste("This operation can only process one set of coordinates",
               "at a time. All arguments must be of length 1."))
  }
  latitude <- as.numeric(latitude)
  longitude <- as.numeric(longitude)
  intersection <- ifelse(intersection == T, "true", "false")
  server <- match.arg(server)

  # Create JSON payload
  nested_list <- list(
    y = latitude, x = longitude, spatialReference = list(wkid = sr)
  )
  json <- rjson::toJSON(nested_list)
  params <- list(
    location = json,
    outSR = sr,
    returnIntersection = intersection,
    f = "json"
  )

  # Submit request
  geocoder_url <- paste(.base_url, server, "GeocodeServer", "reverseGeocode",
                        sep = "/")
  request <- httr::POST(geocoder_url, body = params, encode = "form")

  # Extract content
  response <- httr::content(request, "parsed", "application/json")
  if (!is.null(response$error)) stop(response$error$message)

  # Final result
  results <- data.frame(
    address = response$address$Match_addr,
    latitude = latitude,
    longitude = longitude
  )
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
#' @param server GeocodeServer to use. Refer to the
#'   \href{https://gis.dallascityhall.com/wwwgis/rest/services/
#'   ToolServices}{ToolServices} API directory for details on each server.
#' @param out_sr Spatial Reference ID (WKID) for the desired lat/long
#'   coordinate system. Defaults to 4326, which corresponds to
#'   \href{https://wiki.gis.com/wiki/index.php/WGS84}{WGS84}. If the coordinates
#'   will be used with the City of Dallas's MapServers, you may wish to set this
#'   to \strong{2276}, the default WKID for many Dallas services. Refer to the
#'   \href{https://developers.arcgis.com/rest/services-reference/enterprise/
#'   projected-coordinate-systems.htm}{Projected} and \href{https://developers.
#'   arcgis.com/rest/services-reference/enterprise/geographic-coordinate-systems
#'   .htm}{Geographic} coordinate system documentation pages for a full list of
#'   valid WKIDs.
#'
#' @return a \code{data.frame} of all address candidates and respective lat/long
#'   coordinates and address matching scores
#'
#' @examples
#' find_address_candidates("1500 Marilla St", max_locs = 1)
#'
#' @export
find_address_candidates <- function(street, city = NULL, zip = NULL,
  max_locs = NULL, server = c("DallasStreetsLocator", "ParcelLocator",
  "AccountPointsLocator", "AccountpointsStreetLocator"), out_sr = 4326)
{
  # Input validation
  if (any(length(street) > 1, length(city) > 1, length(zip) > 1)) {
    stop(paste("This operation can only process one address ",
               "at a time. All arguments must be of length 1."))
  }

  if (is.null(street)) street <- ""
  else street <- as.character(street)

  if (is.null(city)) city <- ""
  else city <- as.character(city)

  if (is.null(zip)) zip <- ""
  else zip <- as.numeric(zip)

  if (is.null(max_locs)) max_locs <- ""
  else max_locs <- paste0("&maxLocations=", max_locs)

  server <- match.arg(server)

  # Submit request
  geocoder_url <- paste(.base_url, server, "GeocodeServer",
                        "findAddressCandidates", sep = "/")
  post_url <- gsub(" ", "%20",
                   paste0(geocoder_url,
                          "?Street=", street,
                          "&City=", city,
                          "&ZIP=", zip,
                          "&outSR=", out_sr,
                          max_locs,
                          "&f=json"))
  request <- httr::POST(url = post_url)

  # Extract content
  response <- httr::content(request, "parsed", "application/json")
  if (!is.null(response$error)) stop(response$error$message)

  candidates <- response$candidates
  attributes <- lapply(
    seq(candidates),
    function(i) list(
      candidate = i,
      address = candidates[[i]]$address,
      latitude = candidates[[i]]$location$y,
      longitude = candidates[[i]]$location$x,
      score = candidates[[i]]$score
    )
  )

  # Convert nested list attributes into dataframe
  results <- data.frame(do.call(rbind.data.frame, attributes))
  return(results)
}
