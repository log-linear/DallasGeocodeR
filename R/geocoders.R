
# Geocoder Service URL
service_url <- paste0(
  "https://gis.dallascityhall.com/wwwgis/rest/services",
  "/ToolServices/DallasStreetsLocator/GeocodeServer/"
)

#' Convert City of Dallas addresses to lat/long coordinates
#'
#' Calculate lat/long coordinates for a given set of addresses using the
#' \href{https://gis.dallascityhall.com/wwwgis/rest/services/ToolServices/
#' DallasStreetsLocator/GeocodeServer/geocodeAddresses}{City of Dallas's public
#' geocoding service}. Note that this service can geocode a \strong{maximum 1000
#' addresses} per request. Calling this function on more than 1000 addresses
#' will return an error. See ArcGIS's \href{https://developers.arcgis.com/rest/
#' geocode/api-reference/geocoding-geocode-addresses.htm}{Geocode Addresses}
#' documentation page for additional details.
#'
#' @param street character vector of street addresses.
#' @param city optional, character vector of city names.
#' @param zip optional, character vector of postal codes.
#' @param id optional, numeric vector uniquely identifying each address,
#'   defaults to \code{seq(street)} if not provided. If providing this
#'   argument (e.g., to join onto an existing dataframe), note that each
#'   integer in the vector must be unique.
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
geocode_addresses <- function(street, city = NULL, zip = NULL, id = NULL) {

  geocoder_url <- paste0(service_url, "geocodeAddresses")
  batch_size <- 1000
  n_addresses <- length(street)

  # Input validation
  if (n_addresses > batch_size) {
    stop(paste("COD Geocoder can only process a maximum",
               batch_size,
               "addresses at once."))
  }

  street[is.na(street)] <- ""

  if (is.null(city)) city <- rep("", n_addresses)
  else city[is.na(city)] <- ""

  if (is.null(zip)) zip <- rep("", n_addresses)
  else zip[is.na(zip)] <- ""

  if (is.null(id)) id <- seq(street)
  else id <- as.numeric(id)

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
    f = "json"
  )

  # Submit request
  request <- httr::POST(
    url = geocoder_url,
    body = params,
    encode = "form"
  )

  # Extract content
  response <- httr::content(request, "parsed", "application/json")
  locations <- response$locations
  attributes <- lapply(
    seq(id),
    function(i) list(
      id = id[[i]],
      latitude = locations[[i]]$attributes$X,
      longitude = locations[[i]]$attributes$Y,
      score = locations[[i]]$score,
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
#' \href{https://gis.dallascityhall.com/wwwgis/rest/services/ToolServices/
#' DallasStreetsLocator/GeocodeServer/reverseGeocode}{City of Dallas's public
#' reverse geocoding service}. Note that this service can only convert a single
#' set of coordinates at a time. See ArcGIS's
#' \href{https://developers.arcgis.com/rest/geocode/api-reference/
#' geocoding-reverse-geocode.htm}{Reverse Geocode} documentation page for
#' additional details.
#'
#' @param latitude scalar, latitude coordinate to reverse geocode,
#' @param longitude scalar, longitude coordinate to reverse geocode
#' @param intersection logical boolean value specifying whether the
#'   geocode service should return the nearest street intersection or the
#'   nearest address to the given point. Default is FALSE.
#' @param distance scalar, maximum distance in meters from coordinates
#'   within which a matching address should be searched. Default value is 0.
#'
#' @return A single row \code{data.frame} of the reverse-geocoded address with
#'   columns \code{street}, \code{city}, and \code{zip}
#'
#' @examples
#' reverse_geocode(2516412.847819, 6986517.2125469996)
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
                            distance = 0) {

  geocoder_url <- paste0(service_url, "reverseGeocode")

  # Input validation
  if (any(length(latitude) > 1, length(longitude) > 1, length(distance) > 1,
          length(intersection) > 1)) {
    stop(paste("This operation can only process one set of coordinates",
               "at a time. All arguments must be of length 1."))
  }
  latitude <- as.numeric(latitude)
  longitude <- as.numeric(longitude)
  distance <- ifelse(is.null(distance) | distance < 1,
                     "",
                     paste0("&distance=", distance))
  intersection <- ifelse(intersection == T, "true", "false")

  # Submit request
  post_url = paste0(geocoder_url,
                    "?location=", latitude, ",", longitude,
                    distance,
                    "&returnIntersection=", intersection,
                    "&f=json")
  request <- httr::POST(post_url)

  # Extract content
  response <- httr::content(request, "parsed", "application/json")
  if (!is.null(response$error)) {
    stop(paste0(response$error$message, " Please verify that the provided ",
                "coordinates are valid latitude and longitude values."))
  }

  # Final result
  results <- data.frame(
    street = response$address$Street,
    city = response$address$City,
    zip = response$address$ZIP,
    latitude = latitude,
    longitude = longitude
  )
  return(results)
}

#' Find address candidates for City of Dallas addresses
#'
#' Identify matching City of Dallas addresses for a given address using the
#' \href{https://gis.dallascityhall.com/wwwgis/rest/services/ToolServices/
#' DallasStreetsLocator/GeocodeServer/reverseGeocode}{City of Dallas's public
#' address candidate service}. Note that this operation can only process one
#' address at a time. See ArcGIS's
#' \href{https://developers.arcgis.com/rest/geocode/api-reference/
#' geocoding-find-address-candidates.htm}{findAddressCandidates} documentation
#' page for additional details
#'
#' @param street street address to match against. Must be a single string (i.e.
#'   a character vector of length 1)
#' @param city optional, city to match against. Must be a single string (i.e.
#'   a character vector of length 1)
#' @param zip optional, postal code to match against. Must be a single string
#'   (i.e. a character vector of length 1)
#' @param max_locs optional, maximum number of address candidates to return.
#'   Must be a single scalar value.
#'
#' @return a \code{data.frame} of all address candidates and respective lat/long
#'   coordinates and address matching scores
#'
#' @examples
#' find_address_candidates("1500 Marilla St", max_locs = 1)
#'
#' @export
find_address_candidates <- function(street, city = NULL, zip = NULL,
                                    max_locs = NULL) {

  geocoder_url <- paste0(service_url, "findAddressCandidates")

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

  post_url <- gsub(" ", "%20", paste0(geocoder_url,
                                                       "?Street=", street,
                                                       "&City=", city,
                                                       "&ZIP=", zip,
                                                       max_locs,
                                                       "&f=json"))
  request <- httr::POST(url = post_url)

  # Extract content
  response <- httr::content(request, "parsed", "application/json")
  candidates <- response$candidates
  attributes <- lapply(
    seq(candidates),
    function(i) list(
      candidate = i,
      address = candidates[[i]]$address,
      latitude = candidates[[i]]$location$x,
      longitude = candidates[[i]]$location$y,
      score = candidates[[i]]$score
    )
  )

  # Convert nested list attributes into dataframe
  results <- data.frame(do.call(rbind.data.frame, attributes))
  return(results)
}

#===============================================================================
# Test code
#===============================================================================
addresses <- data.frame(
  street = c('8525 Garland Rd', '1500 Marilla St', '3809 Grand Avenue'),
  city = c('Dallas', 'Dallas', 'Dallas'),
  zip = c('75218', '75201', '75210')
)

test <- geocode_addresses(addresses$street, addresses$city, addresses$zip)
test <- geocode_addresses(addresses$street)
test2 <- reverse_geocode(test$latitude[[1]], test$longitude[[1]])

test3 <- find_address_candidates(addresses$street[[1]], addresses$city[[1]], addresses$zip[[1]])
test3 <- find_address_candidates(addresses$street[[1]], max_locs = 1)
