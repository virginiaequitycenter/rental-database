# Explore Albemarle County Data

# packages
library(readxl)

# working directory
setwd("~/git/rental-database")

# albemarle
parcel_info <- read_excel("data/original/albemarle/GIS_View_Redacted_ParcelInfo.xlsx")
card_info <- read_excel("data/original/albemarle/GIS_CardLevelData_new.xlsx")

# Residential vs Commercial (in card data)
table(card_info$CardType)