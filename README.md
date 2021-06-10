# DallasGeocodeR

## Overview

DallasGeocodeR is an R package for interfacing with the City of Dallas's [public 
geocoding services](https://gis.dallascityhall.com/wwwgis/rest/services/ToolServices),
a set of REST APIs powered by ArcGIS. The DallasGeocodeR package provides 
convenient wrapper functions for several key operations:

- [geocodeAddresses](https://developers.arcgis.com/rest/geocode/api-reference/geocoding-geocode-addresses.htm) -
For converting addresses or intersections into geographic (latitude and
longitude) coordinates 
- [reverseGeocode](https://developers.arcgis.com/rest/geocode/api-reference/geocoding-reverse-geocode.htm) -
For converting latitude and longitude coordinates into street addresses
- [findAddressCandidates](https://developers.arcgis.com/rest/geocode/api-reference/geocoding-find-address-candidates.htm) -
For finding matching addresses for a given location
  
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
#>    id latitude longitude score status                        address  address_type
#> 2   1  2516413   6986517   100      M 8525 GARLAND RD, DALLAS, 75218 StreetAddress
#> 21  2  2491525   6969612   100      M 1500 MARILLA ST, DALLAS, 75201 StreetAddress
#> 3   3  2502618   6971124   100      M  3809 GRAND AVE, DALLAS, 75210 StreetAddress

geocode_addresses(addresses$street)
#>    id latitude longitude score status                        address  address_type
#> 2   1  2516413   6986517   100      M 8525 GARLAND RD, DALLAS, 75218 StreetAddress
#> 21  2  2491525   6969612   100      M 1500 MARILLA ST, DALLAS, 75201 StreetAddress
#> 3   3  2502618   6971124   100      M  3809 GRAND AVE, DALLAS, 75210 StreetAddress
```
### Reverse geocoding

```R
reverse_geocode(coords$latitude, coords$longitude)
#>                          address latitude longitude
#> 1 8525 GARLAND RD, DALLAS, 75218 32.82167 -96.71625
#> 2 1500 MARILLA ST, DALLAS, 75201 32.77634 -96.79815
#> 3  3809 GRAND AVE, DALLAS, 75210 32.78000 -96.76198
```

### Finding address candidates

```R
find_address_candidates(addresses$street)
#>   candidate                        address latitude longitude score
#> 1         1 8525 GARLAND RD, DALLAS, 75218 32.82167 -96.71625   100
#> 2         2 8526 GARLAND RD, DALLAS, 75218 32.82156 -96.71600    79
#> 3         1 1500 MARILLA ST, DALLAS, 75201 32.77634 -96.79815   100
#> 4         2 1501 MARILLA ST, DALLAS, 75201 32.77644 -96.79819    79
#> 5         1  3809 GRAND AVE, DALLAS, 75210 32.78000 -96.76198   100
#> 6         2  3810 GRAND AVE, DALLAS, 75210 32.78011 -96.76233    79
```