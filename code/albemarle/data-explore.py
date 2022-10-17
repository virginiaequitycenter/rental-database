import pandas as pd
import matplotlib.pyplot as plt

parcel_level_data = pd.read_csv("../../data/original/albemarle/GIS_View_Redacted_ParcelInfo.xlsx", sep=",")
card_level_data = pd.read_csv("../../data/original/albemarle/GIS_CardLevelData_new.xlsx", sep=",")
other_parcel_chars = pd.read_csv("../../data/original/albemarle/CityView_View_OtherParcelCharacteristics.txt", sep=",")

aggregate_df = pd.concat([parcel_level_data, card_level_data, other_parcel_chars], axis=1, join='outer')
