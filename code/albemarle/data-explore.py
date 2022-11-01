import pandas as pd
import matplotlib.pyplot as plt
import geopandas
import fiona

# Function for assigning cmap value to property
def corporateownedcondition(row):
    if ('LLC' in row['Owner']):
        val = 1
    elif ('CORP' in row['Owner']):
        val = 1
    elif ('INC' in row['Owner']):
        val = 1
    elif ('COMPANY' in row['Owner']):
        val = 1
    else:
        val = 0
    return val

# Read in the data
parcel_level_data = pd.read_csv("../../data/original/albemarle/GIS_View_Redacted_ParcelInfo.xlsx", sep=",")
card_level_data = pd.read_csv("../../data/original/albemarle/GIS_CardLevelData_new.xlsx", sep=",")
other_parcel_chars = pd.read_csv("../../data/original/albemarle/CityView_View_OtherParcelCharacteristics.txt", sep=",")
parcel_shapes = geopandas.read_file("parcels_shape_current/Parcels_current.shp")

# Filter the data for buildings (not just plots of land)
filtered_parcel_level_data = parcel_level_data[parcel_level_data['HouseNo'].notnull()]

# Filter for homes with a different address from the owner address
# Checks only for mismatch in first 6 characters to prevent typo mismatch
filtered_parcel_level_data = filtered_parcel_level_data[filtered_parcel_level_data['PropStreet'].str[:6] != filtered_parcel_level_data['OwnerAddress'].str[:6]]

# Combine filtered parcel level info with other parcel characteristics
aggregate_df = pd.merge(filtered_parcel_level_data, other_parcel_chars, on='ParcelID')

# Filter out non-residential properties
aggregate_df = aggregate_df[aggregate_df['LandUsePrimary'].str[:4] == 'Resi']
# Filter out null owner fields
aggregate_df = aggregate_df[aggregate_df['Owner'].notnull()]
# Filter out farms
aggregate_df = aggregate_df[~aggregate_df['Owner'].str.contains('FARM')]
# Create column for corporate ownership
aggregate_df = aggregate_df.apply(corporateownedcondition, axis=1)

# Save the resulting dataframe to a csv (no shapefile info)
aggregate_df.to_csv('../../data/working/albemarle/potential_rental_properties_albemarle.csv')

# Merge the parcel shape information into the filtered data
aggregate_geo_df = pd.merge(geodata, aggregate_df, how='right', left_on='PIN', right_on='ParcelID')

# Create figure with color for corporate/individual ownership
# corporate = green
# individual = pink
# need to add legend to plot
fig, ax = plt.subplots()
aggregate_geo_df.plot(ax=ax, column='CorporateOwned',cmap='PiYG')

plt.savefig('../../albemarle_rental_property_map.png')
