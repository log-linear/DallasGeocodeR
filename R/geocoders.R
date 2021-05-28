
geocode_addresses <- function(street, city = NULL, zip = NULL) {

  batch_size <- 1000  # Hardcoded server limit
  geocoder_url <- "https://gis.dallascityhall.com/wwwgis/rest/services/ToolServices/DallasStreetsLocator/GeocodeServer/geocodeAddresses"

  n_addresses <- length(street)
  if (n_addresses > batch_size) {
    stop(paste("geocode_addresses can only process a maximum",
               batch_size,
               "addresses at once."))
  }

  # Set city/zip to empty strings if not provided
  if (is.null(city)) city <- rep("", n_addresses)
  if (is.null(zip)) zip <- rep("", n_addresses)

  # Handle any missing values
  street <- stringr::str_replace_na(street, "")
  city <- stringr::str_replace_na(city, "")
  zip <- stringr::str_replace_na(zip, "")

  # Create JSON payload
  nested_list <- list(
    records = lapply(seq(street),
                     function(i) list(attributes = list(OBJECTID = i,
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
    seq(locations),
    function(i) list(
      id = locations[[i]]$attributes$ResultID,
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

#===============================================================================
# Test code
#===============================================================================
addresses <- data.frame(
  street = c('5500 Reiger Ave', '1500 Marilla St', '9231 Pinewood Dr'),
  city = c('Dallas', 'Dallas', 'Dallas'),
  zip = c('75214', '75201', '75243')
)

test <- geocode_addresses(addresses$street, addresses$city, addresses$zip)
test <- geocode_addresses(addresses$street)
