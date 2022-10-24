#Cville Rental Database approximation

# packages
library(readxl)
library(readr)
library(tidyverse)
library(tidygeocoder)
library(stringi) 
library(fedmatch)

# working directory
setwd("~/git/rental-database")

# cville
parcel_details <- read_csv("data/original/cville/Parcel_Area_Details.csv")
# non-unique parcel IDs: 490168000, 230035000, 550073110 
# same entrym, but for address
# drop the ones with NA address
parcel_details <- parcel_details %>% filter(!(OBJECTID %in% c("104086")))
parcel_details <- parcel_details[-(is.na(parcel_details$OwnerAddress) & parcel_details$ParcelNumber == "230035000"),]
parcel_details <- parcel_details[-(is.na(parcel_details$OwnerAddress) & parcel_details$ParcelNumber == "550073110"),]

residential <- read_csv("data/original/cville/Real_Estate_(Residential_Details).csv")

# merge
residential_parcels <- merge(parcel_details, residential, by.x = "ParcelNumber", by.y = "ParcelNumber", 
                             no.dups = FALSE, all = T) 

# restrict residential properties to only livable places
usecodes_incl <- c("Apartments 1-10 units", "Apartments 11-20 Units", "Apartments over 20 units",
                  "Condo Main", "Condominium", "Condo Common Area", "Duplex", "Quadplex",
                  "Rooming House", "Single Family", "Single Family Attached", 
                  "Single Family-1 Conversion", "Single Family-2 Conversion", 
                  "Single Family-3 Conversion", "Triplex") 

residential_parcels <- residential_parcels %>% subset(UseCode %in% usecodes_incl)

# rsidential properties IDs
residential_parcels <- residential_parcels %>% mutate(ID = row_number())

#--------------------------------------------------------------------------
# identify owners who do not live in the properties they own
#-------------------------------------------------------------------------

# replace NAs in Unit col with empty string
residential_parcels$Unit.y[is.na(residential_parcels$Unit.y) == T] <- ""

# full parcel address
residential_parcels$full_address <- paste(
  str_trim(residential_parcels$StreetNumber.y),
  str_trim(residential_parcels$StreetName.y),
  str_trim(residential_parcels$Unit.y),
  "Charlottesville",
  "VA")
# remove white spaces
residential_parcels$full_address <- str_squish(residential_parcels$full_address)

# recover the full parcel address
# installed google api key
readRenviron("~/.Renviron")
Sys.getenv("GOOGLEGEOCODE_API_KEY")

#geocode the addresses
residential_parcels_lonlat <- residential_parcels %>%
  geocode(full_address,
          method = 'google',
          lat = latitude ,
          long = longitude,
          full_results = T)

# save the geocoded dataset
savepath = "data/working/cville/"
#readr::write_csv(residential_parcels_lonlat, xzfile(paste0(savepath,"residential_parcels_geocoded.csv.xz"), compression = 9))

# upload the geocoded data
residential_parcels_lonlat <- readr::read_csv(paste0(savepath,"residential_parcels_geocoded.csv.xz"))

# use regex to extract the zipcode
residential_parcels$parcel_zip_code_5 <- stri_extract_last_regex(
  residential_parcels_lonlat$formatted_address, "\\d{5}+")
# wrongly geocoded
residential_parcels$parcel_zip_code_5[residential_parcels$parcel_zip_code_5 == "46117"] <- "22902"

# format owners zip to 5 digits
residential_parcels$OwnerZipCode_5 <- gsub(residential_parcels$OwnerZipCode, 
                                 pattern="-[0-9]{0,9}", replacement = "")
residential_parcels$OwnerZipCode_5 <- substr(residential_parcels$OwnerZipCode_5, 1,5)

# owners that live in a different zip
owners_difzip <- residential_parcels %>% subset(as.numeric(OwnerZipCode_5) != as.numeric(parcel_zip_code_5))
# exclude if the owners zip is NA
owners_difzip <- owners_difzip[!is.na(owners_difzip$OwnerZipCode),]

#-----------------------------------------------------------------------------------------------
# for the owners living in the same zip code
# 1. standardize owners address
# 2. compare parcl and owner's address using fuzzy match
#-----------------------------------------------------------------------------------------------

# drop observations with owners not living in the same zip
residential_parcels <- residential_parcels %>% subset(!(ID %in% owners_difzip$ID))

# owners full address
residential_parcels$owner_full_address <- paste(
  str_trim(residential_parcels$OwnerAddress),
  str_trim(residential_parcels$OwnerCityState),
  str_trim(residential_parcels$OwnerZipCode_5))
# remove white spaces
residential_parcels$owner_full_address <- str_squish(residential_parcels$owner_full_address)

# geocode owners addresses
owners_addresses <- residential_parcels %>%
  geocode(owner_full_address,
          method = 'google',
          lat = latitude ,
          long = longitude,
          full_results = T)
owners_addresses_cols <- owners_addresses %>% select(c("ID", owner_lat = "latitude", 
                                                       owner_log = "longitude", 
                                                       owner_format_address = "formatted_address")) 
parcel_addresses_cols <- residential_parcels_lonlat %>% select(c(propID = "ID", parcel_lat = "latitude", 
                                                       parcel_log = "longitude", 
                                                       parcel_format_address = "formatted_address")) %>%
  filter(!(propID %in% owners_difzip$ID))

# use fuzzy match to find which of owner addresses are not the same as property addresses 
result <- merge_plus(
  data1 = owners_addresses_cols,
  match_type = "fuzzy",
  data2 = parcel_addresses_cols, by.x = "owner_format_address", by.y = "parcel_format_address",
  unique_key_1 = "ID", unique_key_2 = "propID",
  fuzzy_settings = build_fuzzy_settings(method = "wgt_jaccard", nthread = 2,
                                        maxDist = 0.01))


#matched_addresses <- result$matches
non_matched_own_adr <- result$data1_nomatch
non_matched_own_adr <- left_join(non_matched_own_adr, parcel_addresses_cols, by=c("ID"="propID"))

#-----------------------------------------------------------------------------------------------
# identify non-person owners
#-----------------------------------------------------------------------------------------------