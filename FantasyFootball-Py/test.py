# import matplotlib.pyplot as plt
import pandas as pd
# from Tools.scripts.make_ctype import values
# from contourpy._contourpy import Linear
# from sklearn.linear_model import LinearRegression
#
# # Train Test Split
# from sklearn.model_selection import train_test_split

databaseFolder = r'Y:\Fantasy Football\Database'

# Step 0: Input and clean data
roster = pd.read_parquet(databaseFolder + r'\roster.parquet')
playerStatsYearly = pd.read_parquet(databaseFolder + r'\playerStatsYearly.parquet')

print('test')
