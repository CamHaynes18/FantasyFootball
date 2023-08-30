import pandas as pd


fantasyFootballFolder = r'Z:\Fantasy Football'

# Step 0: Input and clean data
roster = pd.read_parquet(fantasyFootballFolder + r'\Database\roster.parquet')
playerStatsYearly = pd.read_parquet(fantasyFootballFolder + r'\Database\playerStatsYearly.parquet')

playerStatsYearly = playerStatsYearly[playerStatsYearly['league'] == 'NFL']

print('test')
