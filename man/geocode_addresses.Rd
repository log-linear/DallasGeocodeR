% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geocoders.R
\name{geocode_addresses}
\alias{geocode_addresses}
\title{Convert City of Dallas addresses to lat/long coordinates}
\usage{
geocode_addresses(
  street,
  city = NULL,
  zip = NULL,
  id = seq_along(street),
  server = c("DallasStreetsLocator", "ParcelLocator", "AccountPointsLocator",
    "AccountpointsStreetLocator"),
  out_sr = 4326,
  output = c("latlong", "all")
)
}
\arguments{
\item{street}{character vector of street addresses to geocode.}

\item{city}{optional, character vector of city names to geocode.}

\item{zip}{optional, vector of postal codes to geocode.}

\item{id}{optional, numeric vector uniquely identifying each address.
providing this argument note that each integer \emph{must} be unique.}

\item{server}{GeocodeServer to use. Defaults to DallasStreetsLocator.
Refer to the \href{https://gis.dallascityhall.com/wwwgis/rest/services/
ToolServices}{ToolServices} API directory for details on each server.}

\item{out_sr}{Spatial Reference ID (WKID) for the desired lat/long
coordinate system. Defaults to 4326 (WGS84). If using with one of the City
of Dallas's MapServers, you may wish to set this to 2276, the default WKID
for many Dallas services. Refer to ArcGIS's \href{https://developers.arcgis
.com/rest/services-reference/enterprise/projected-coordinate-systems.htm}{
Projected} and \href{https://developers.arcgis.com/rest/services-reference/
enterprise/geographic-coordinate-systems.htm}{Geographic} coordinate system
documentation pages for a full list of valid WKIDs.}

\item{output}{output of this function. If \code{latlong} (default), will
return a dataframe of lat/long coordinates and addresses. If \code{all},
will return the full JSON response from the server as a list.}
}
\value{
A dataframe or list of geocoded address results.
}
\description{
Find lat/long coordinates for a given set of addresses using the City of
Dallas's public \href{https://gis.dallascityhall.com/wwwgis/rest/services/
ToolServices/DallasStreetsLocator/GeocodeServer/geocodeAddresses}{
geocodeAddresses service}. See ArcGIS's \href{https://developers.arcgis.com/
rest/geocode/api-reference/geocoding-geocode-addresses.htm}{documentation
page} for additional details.
}
\examples{
addresses <- data.frame(
  street = c('8525 Garland Rd', '1500 Marilla St', '3809 Grand Avenue'),
  city = c('Dallas', 'Dallas', 'Dallas'),
  zip = c(75218, 75201, 75210)
)
geocode_addresses(addresses$street, addresses$city, addresses$zip)

geocode_addresses(addresses$street)

}
