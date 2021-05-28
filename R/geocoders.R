
#' Geocode addresses using the City of Dallas's public geocoding service
#'
#' Calculate lat/long coordinates for a given set of addresses using the City of
#' Dallas's public geocoding service. Note that this service can geocode a
#' \strong{maximum 1000 addresses} per request. Calling this function on more
#' than 1000 addresses will return an error. See the
#' \href{https://gis.dallascityhall.com/wwwgis/rest/services/ToolServices/
#' DallasStreetsLocator/GeocodeServer}{ArcGIS REST API endpoint} for additional
#' details.
#'
#' @param street character vector of street addresses.
#' @param city optional, character vector of city names.
#' @param zip optional, character vector of postal codes.
#' @param id optional, numeric vector uniquely identifying each address,
#'   defaults to \code{seq(street)} if not provided. If providing this
#'   argument, note that each integer in the vector must be unique.
#' @return A \code{data.frame} of geocoded address results. Includes original
#'   address alongside generated lat/long coordinates.
#' @examples
#' addresses <- data.frame(
#'   street = c('8525 Garland Rd', '1500 Marilla St', '3809 Grand Avenue'),
#'   city = c('Dallas', 'Dallas', 'Dallas'),
#'   zip = c('75218', '75201', '75210')
#' )
#'
#' geocode_addresses(addresses$street, addresses$city, addresses$zip)
#' geocode_addresses(addresses$street)
geocode_addresses <- function(street, city = NULL, zip = NULL, id = NULL) {

  batch_size <- 1000
  geocoder_url <- paste0(
    "https://gis.dallascityhall.com/wwwgis/rest/services",
    "/ToolServices/DallasStreetsLocator/GeocodeServer/geocodeAddresses"
  )
  n_addresses <- length(street)

  # Check that total addresses <= server maximum
  if (n_addresses > batch_size) {
    stop(paste("COD Geocoder can only process a maximum",
               batch_size,
               "addresses at once."))
  }

  # Input validation
  street <- stringr::str_replace_na(street, "")

  # Handle optional args if not provided
  if (is.null(city)) city <- rep("", n_addresses)
  else city <- stringr::str_replace_na(city, "")

  if (is.null(zip)) zip <- rep("", n_addresses)
  else zip <- stringr::str_replace_na(zip, "")

  if (is.null(id)) id <- seq(street)
  else id <- as.numeric(id)

  # Create JSON payload
  nested_list <- list(
    records = lapply(id,
                     function(i) list(attributes = list(OBJECTID = id[[i]],
                                                        Street = street[[i]],
                                                        City = city[[i]],
                                                        ZIP = zip[[i]])))
  )
  json <- rjson::toJSON(nested_list)

  # Finalize formatted POST parameters
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

  # Get relevant attributes
  locations <- response$locations

  attributes <- lapply(
    id,
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
  results <- data.table::rbindlist(attributes)
  return(results)
}

#' Reverse Geocode addresses using the City of Dallas's public geocoding service
#'
#' Determine addresses from lat/long coordinates using the City of Dallas's
#' public geocoding service. See the
#' \href{https://gis.dallascityhall.com/wwwgis/rest/services/ToolServices/
#' DallasStreetsLocator/GeocodeServer}{ArcGIS REST API endpoint} for additional
#' details.
#'
#' @param latitude scalar, latitude coordinate to reverse geocode,
#' @param longitude scalar, longitude coordinate to reverse geocode
#' @param intersection logical boolean value specifying whether the
#'   geocode service should return the nearest street intersection or the
#'   nearest address to the given point. Default is FALSE.
#' @param distance scalar, maximum distance in meters from coordinates
#'   within which a matching address should be searched. Default value is 0.
#' @return A single row \code{data.frame} of the reverse-geocoded address with
#'   columns /code{street}, /code{city}, and /code{zip}
reverse_geocode <- function(latitude, longitude, intersection = F,
                            distance = 0) {

  geocoder_url <- paste0(
    "https://gis.dallascityhall.com/wwwgis/rest/services",
    "/ToolServices/DallasStreetsLocator/GeocodeServer/reverseGeocode"
  )

  # Input validation
  if (any(length(latitude) > 1, length(longitude) > 1, length(distance) > 1,
          length(intersection) > 1)) {
    stop("All arguments must be of length 1.")
  }

  latitude <- as.numeric(latitude)
  longitude <- as.numeric(longitude)
  distance <- ifelse(is.null(distance) | distance < 1,
                     "",
                     paste0("&distance=", distance))
  intersection <- ifelse(intersection == T, "true", "false")

  # Format Request URL
  req_url <- paste0(geocoder_url,
                    "?location=", latitude, ",", longitude,
                    distance,
                    "&returnIntersection=", intersection,
                    "&f=json")

  # Submit request
  request <- httr::POST(url = req_url)

  # Extract content
  response <- httr::content(request, "parsed", "application/json")

  # Final result
  result <- data.frame(
    street = response$address$Street,
    city = response$address$City,
    zip = response$address$ZIP,
    latitude = latitude,
    longitude = longitude
  )
  return(result)
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
