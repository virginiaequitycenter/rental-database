import pandas as pd
import matplotlib.pyplot as plt

# Read in the data
parcel_level_data = pd.read_csv("../../data/original/albemarle/GIS_View_Redacted_ParcelInfo.xlsx", sep=",")
card_level_data = pd.read_csv("../../data/original/albemarle/GIS_CardLevelData_new.xlsx", sep=",")
other_parcel_chars = pd.read_csv("../../data/original/albemarle/CityView_View_OtherParcelCharacteristics.txt", sep=",")

# Filter the data for buildings (not just plots of land)
filtered_parcel_level_data = parcel_level_data[parcel_level_data['HouseNo'].notnull()]

# Filter for homes with a different address from the owner address
# Checks only for mismatch in first 6 characters to prevent typo mismatch
filtered_parcel_level_data = filtered_parcel_level_data[filtered_parcel_level_data['PropStreet'].str[:6] != filtered_parcel_level_data['OwnerAddress'].str[:6]]

# Combine filtered parcel level info with other parcel characteristics
aggregate_df = pd.merge(filtered_parcel_level_data, other_parcel_chars, on='ParcelID')

# Filter out non-residential properties
aggregate_df = aggregate_df[aggregate_df['LandUsePrimary'].str[:4] == 'Resi']
