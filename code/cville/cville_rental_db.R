#Cville Rental Database approximation

# packages
library(readxl)
library(readr)
library(tidyverse)
library(tidygeocoder)
library(stringi) 

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
#savepath = "data/working/cville/"
#readr::write_csv(residential_parcels_lonlat, xzfile(paste0(savepath,"residential_parcels_geocoded.csv.xz"), compression = 9))

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
owners_difzip <- residential_parcels %>% subset(OwnerZipCode != parcel_zip_code_5)
# exclude if the owners zip is NA
owners_difzip <- owners_difzip[!is.na(owners_difzip$OwnerZipCode),]

#-----------------------------------------------------------------------------------------------
# for the owners living in the same zip code
# 1. standardize owners address
# 2. compare parcl and owner's address using fuzzy match
#-----------------------------------------------------------------------------------------------