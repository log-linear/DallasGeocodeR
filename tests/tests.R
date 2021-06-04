#===============================================================================
# Test code
#===============================================================================
rm(list = ls())
# install.packages("devtools")
devtools::load_all()
addresses <- data.frame(
  street = c('8525 Garland Rd', '1500 Marilla St', '3809 Grand Avenue'),
  city = c('Dallas', 'Dallas', 'Dallas'),
  zip = c('75218', '75201', '75210')
)

#-------------------------------------------------------------------------------
# geocode_addresses
#-------------------------------------------------------------------------------
test01 <- geocode_addresses(addresses$street, addresses$city, addresses$zip)
test02 <- geocode_addresses(addresses$street, addresses$city, addresses$zip, server = "ParcelLocator")
test03 <- geocode_addresses(addresses$street, addresses$city, addresses$zip, server = "AccountPointsLocator")
test04 <- geocode_addresses(addresses$street, addresses$city, addresses$zip, server = "AccountpointsStreetLocator")
test05 <- geocode_addresses(addresses$street)
test06 <- geocode_addresses(addresses$street, server = "ParcelLocator")
test07 <- geocode_addresses(addresses$street, server = "AccountPointsLocator")
test08 <- geocode_addresses(addresses$street, server = "AccountpointsStreetLocator")

# Check that server arg must be of specified set
# geocode_addresses(addresses$street, addresses$city, addresses$zip, server = ""),

#-------------------------------------------------------------------------------
# reverse_geocode
#-------------------------------------------------------------------------------
test09 <- reverse_geocode(test01$longitude[[1]], test01$latitude[[1]])
test10 <- reverse_geocode(test01$longitude[[1]], test01$latitude[[1]], server = "ParcelLocator")
test11 <- reverse_geocode(test01$longitude[[1]], test01$latitude[[1]], server = "AccountPointsLocator")
test12 <- reverse_geocode(test01$longitude[[1]], test01$latitude[[1]], server = "AccountpointsStreetLocator")

#-------------------------------------------------------------------------------
# find_address_candidates
#-------------------------------------------------------------------------------
test13 <- find_address_candidates(addresses$street[[1]], addresses$city[[1]], addresses$zip[[1]])
test14 <- find_address_candidates(addresses$street[[1]], max_locs = 1)
test15 <- find_address_candidates(addresses$street[[1]], max_locs = 1, server = "ParcelLocator")
test16 <- find_address_candidates(addresses$street[[1]], max_locs = 1, server = "AccountPointsLocator")
test17 <- find_address_candidates(addresses$street[[1]], max_locs = 1, server = "AccountpointsStreetLocator")