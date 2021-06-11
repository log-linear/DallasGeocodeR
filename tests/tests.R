#===============================================================================
# Test code
#===============================================================================
rm(list = ls())
# install.packages("devtools")
devtools::load_all()
addresses <- data.frame(
  street = c('8525 Garland Rd', '1500 Marilla St', '3809 Grand Avenue', 'test'),
  city = c('Dallas', 'Dallas', 'Dallas', 'test'),
  zip = c(75218, 75201, 75210, 90210)
)

#-------------------------------------------------------------------------------
# geocode_addresses
#-------------------------------------------------------------------------------
test01 <- geocode_addresses(addresses$street, addresses$city, addresses$zip)

# Test with id parameter
set.seed(5555)
ids <- sample(c(1:9999, sample(1:9999, 999, replace = F)), nrow(addresses))
test0101 <- geocode_addresses(addresses$street, addresses$city, addresses$zip, id = ids)

# Test other servers
test010 <- geocode_addresses(addresses$street, addresses$city, addresses$zip, output = "all")
test02 <- geocode_addresses(addresses$street, addresses$city, addresses$zip, server = "ParcelLocator")
test03 <- geocode_addresses(addresses$street, addresses$city, addresses$zip, server = "AccountPointsLocator")
test04 <- geocode_addresses(addresses$street, addresses$city, addresses$zip, server = "AccountpointsStreetLocator")
test05 <- geocode_addresses(addresses$street)
test06 <- geocode_addresses(addresses$street, server = "ParcelLocator")
test07 <- geocode_addresses(addresses$street, server = "AccountPointsLocator")
test08 <- geocode_addresses(addresses$street, server = "AccountpointsStreetLocator")

# Check that server arg must be of specified set
# geocode_addresses(addresses$street, addresses$city, addresses$zip, server = ""),

# Test for > 1000 addresses
# addresses <- read.csv("test_addresses.csv")
# addresses <- addresses[1:2500,]
# addresses$ids <- sample(c(1:9999, sample(1:9999, 999, replace = F)), nrow(addresses))
# test080 <- geocode_addresses(addresses$Address, id = addresses$ids)

#-------------------------------------------------------------------------------
# reverse_geocode
#-------------------------------------------------------------------------------
test09 <- reverse_geocode(test01$latitude, test01$longitude)

# Test intersections
test090 <- reverse_geocode(test01$latitude, test01$longitude, intersection = T)

#-------------------------------------------------------------------------------
# find_address_candidates
#-------------------------------------------------------------------------------
test13 <- find_address_candidates(addresses$street, addresses$city, addresses$zip)
test14 <- find_address_candidates(addresses$street, max_locs = 1)
test15 <- find_address_candidates(addresses$street, server = "ParcelLocator")
test16 <- find_address_candidates(addresses$street, server = "AccountPointsLocator")
test17 <- find_address_candidates(addresses$street, server = "AccountpointsStreetLocator")

