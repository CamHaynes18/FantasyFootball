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

t = roster[['athlete_id', 'name', 'ncaa_position', 'agility_score', 'all_time_ras', 'boa', 'bos', 'boy',
            'burst_score', 'combine_ht', 'combine_wt', 'combine_bmi', 'conference', 'draft_age', 'draft_pick',
            'draft_round', 'early_declare', 'forty', 'hass', 'ppg_career', 'ppg_first_3yr', 'pre_draft_grade',
            'ras', 'rating', 'speed_score', 'stars', 'top6', 'top12', 'top24']].copy()

t2 = playerStatsYearly.dropna(subset=['athlete_id']).copy()
t2 = t2[['athlete_id', 'season', 'team', 'air_yards_share', 'ayptp', 'dom', 'games', 'racr', 'receiving_air_yards',
         'receiving_epa', 'receiving_yards', 'receiving_tds', 'receiving_tds_ms', 'receiving_yards',
         'receiving_yards_after_catch', 'receiving_yards_ms', 'receptions', 'receptions_ms', 'ryptpa', 'target_share',
         'targets', 'tdptp', 'w_dom', 'wopr', 'year_in_league', 'ypr', 'yptouch', 'yptp']]

t3 = t2.merge(t, how='left')

print('test')
