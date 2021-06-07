# DallasGeocodeR

## Overview

DallasGeocodeR is an R package for interfacing with the City of Dallas's [public 
geocoding services](https://gis.dallascityhall.com/wwwgis/rest/services/ToolServices),
a set of REST APIs powered by ArcGIS. The DallasGeocodeR package 
provides convenient wrapper functions for several key operations:

- [geocodeAddresses](https://developers.arcgis.com/rest/geocode/api-reference/geocoding-geocode-addresses.htm) -
For calculating geographic coordinates (latitude/longitude) from street 
addresses or intersections
- [reverseGeocode](https://developers.arcgis.com/rest/geocode/api-reference/geocoding-reverse-geocode.htm) -
For the opposite operation: determining street addresses or intersections from
latitude and longitude coordinates
- [findAddressCandidates](https://developers.arcgis.com/rest/geocode/api-reference/geocoding-find-address-candidates.htm) -
For finding probable addresses and lat/long coordinates for a given location
  
## Installation

The easiest way to install DallasGeocodeR is directly through git via the 
`remotes` package:

```R
# install.packages("remotes")
remotes::install_github("log-linear/DallasGeocodeR")
```

## Usage

### Geocoding addresses 

```R
library(DallasGeocodeR)

addresses <- data.frame(
  street = c('8525 Garland Rd', '1500 Marilla St', '3809 Grand Avenue'),
  city = c('Dallas', 'Dallas', 'Dallas'),
  zip = c('75218', '75201', '75210')
)

geocode_addresses(addresses$street, addresses$city, addresses$zip)
#> id latitude longitude score status                        address  address_type
#> 2   1  2516413   6986517   100      M 8525 GARLAND RD, DALLAS, 75218 StreetAddress
#> 21  2  2491525   6969612   100      M 1500 MARILLA ST, DALLAS, 75201 StreetAddress
#> 3   3  2502618   6971124   100      M  3809 GRAND AVE, DALLAS, 75210 StreetAddress

geocode_addresses(addresses$street)
#> id latitude longitude score status                        address  address_type
#> 2   1  2516413   6986517   100      M 8525 GARLAND RD, DALLAS, 75218 StreetAddress
#> 21  2  2491525   6969612   100      M 1500 MARILLA ST, DALLAS, 75201 StreetAddress
#> 3   3  2502618   6971124   100      M  3809 GRAND AVE, DALLAS, 75210 StreetAddress
```

### Reverse geocoding lat/long coordinates

```R
library(DallasGeocodeR)

reverse_geocode(32.8217, -96.7163) 
#>            street   city   zip latitude longitude
#> 1 8525 GARLAND RD DALLAS 75218  2516413   6986517
```

### Finding address candidates

```R
library(DallasGeocodeR)

find_address_candidates("1500 Marilla St")
#> candidate                        address latitude longitude score
#> 2          1 1500 MARILLA ST, DALLAS, 75201  2491525   6969612   100
#> 21         2 1501 MARILLA ST, DALLAS, 75201  2491514   6969650    79
```