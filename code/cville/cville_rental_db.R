# Cville Rental Database approximation

# packages
library(readxl)
library(readr)
library(tidyverse)
library(tidygeocoder)
library(stringi) 
library(fedmatch)
library(sf)
library(leaflet)
library(htmlwidgets)
library(webshot)

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
residential_parcels <- merge(parcel_details, residential, by.x = "ParcelNumber", 
                             by.y = "ParcelNumber", 
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
# 2. compare parcel and owner's address using fuzzy match
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


#readr::write_csv(owners_addresses, xzfile(paste0(savepath,"full_owner_adr_same_zip.csv.xz"), compression = 9))

# upload the geocoded data
owners_addresses <- readr::read_csv(paste0(savepath,"full_owner_adr_same_zip.csv.xz"))


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

# get all the owners who do not live at the property addresses
df2 <- residential_parcels[residential_parcels$ID %in% non_matched_own_adr$ID,]
df2 <- df2 %>% select(-owner_full_address)
own_not_live <- rbind(owners_difzip, df2)

# save the dataset of owners not living at the properties they own
#readr::write_csv(own_not_live, xzfile(paste0(savepath,"own_not_live.csv.xz"), compression = 9))

# upload the rental db
own_not_live <- readr::read_csv(paste0(savepath,"own_not_live.csv.xz"))

# load regex to identify non-person owners
source('code/cville/non_res_pla_pattern.R')
# Comment: mislabels owners couples as a non-person owner
# Exclude owners which has "&" in their name
# Include "trust", "trustee" and "trustees" as a non-person owners
matched <- grepl(x = own_not_live$OwnerName, pattern = non_res_pla_pattern)
own_not_live["non-person_owner"] <- as.numeric(matched)

#df <- own_not_live[,c("OwnerName", "non-person_owner")]
#df1 <- df[df$`non-person_owner` == 1,]

#-----------------------------------------------------------------------------------------------
# Map 
#-----------------------------------------------------------------------------------------------

# read in shape files
parcel_shapes <- st_read("data/original/cville/parcel_shapes/Parcel_Area_Details.shp")

# merge to owners who don't live in the properties they own
own_not_live <- left_join(own_not_live, parcel_shapes[,c("ParcelNumb", "geometry")], 
                          by=c("ParcelNumber"="ParcelNumb"))

# initilize the map
map_parcels <- leaflet(parcel_shapes, options = leafletOptions(attributionControl = FALSE)) %>%
  setView(-78.47, 38.03, 13) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addScaleBar("bottomleft") %>%
  addMapPane("lines", zIndex = 410) %>%
  addMapPane("points", zIndex = 411) %>%
  addPolygons(data = parcel_shapes$geometry,
              fillColor = "grey",
              fillOpacity = 0.5, stroke = FALSE, group = "Parcels"
  ) 
#%>% hideGroup("Parcels")

# add the layer of access scores to the map
pal <- colorFactor(c("#009E73", "#CC79A7"), own_not_live$`non-person_owner`)
map <- map_parcels %>%
  addControl("Non-person owners (Owners do not live in the properties they own)", "topright") %>%
  addLegend("bottomright", pal, own_not_live$`non-person_owner`, 
            title = "Ownership Type", opacity = .7,
            labFormat = function(type, cuts, p) {  # Here's the trick
              paste0(c("Person", "Non-person"))
            }) %>%
  addPolygons(data=own_not_live$geometry,
              fillColor = pal(own_not_live$`non-person_owner`), fillOpacity = .7, weight = 1, 
              color = "#000",
              #highlightOptions = highlightOptions(color = "#fff"), group = "Nonperson",
              label = paste0(
                "Parcel ID: ", own_not_live$ParcelNumber, ", Owner: ", own_not_live$OwnerName,
                ", Address: ", own_not_live$full_address))

# save as a html page
saveWidget(map, file="data/working/cville/cville_rental_property_map.html")
# saving a map as an image 
webshot("data/working/cville/cville_rental_property_map.html", file = "data/working/cville/cville_rental_property_map.png",
       cliprect = "viewport")

