roster <- arrow::read_parquet(paste(databasePath, 'roster.parquet', sep = ''))

playerStatsWeekly <- arrow::read_parquet('Y:/Fantasy Football/Database/playerStatsWeekly.parquet')
#teamStatsWeekly <- arrow::read_parquet('Y:/Fantasy Football/Database/teamStatsWeekly.parquet')
playerStatsYearly <- arrow::read_parquet(paste(databasePath, 'playerStatsYearly.parquet', sep = ''))
teamStatsYearly <- arrow::read_parquet(paste(databasePath, 'teamStatsYearly.parquet', sep = ''))


# playerStatsNcaaWeekly <- arrow::read_parquet('Y:/Fantasy Football/Database/playerStatsNcaaWeekly.parquet')
# teamStatsNcaaWeekly <- arrow::read_parquet('Y:/Fantasy Football/Database/teamStatsNcaaWeekly.parquet')
# playerStatsNcaaYearly <- arrow::read_parquet('Y:/Fantasy Football/Database/playerStatsNcaaYearly.parquet')
# teamStatsNcaaYearly <- arrow::read_parquet('Y:/Fantasy Football/Database/teamStatsNcaaYearly.parquet')



# Incomplete
## 

# Ideas
## Update/Create database - weekly during season, monthly during offseason



### Create Database ###



### Inseason Analysis ###

myfunc_wr_trends(6, current_week, year, pbp, ps_overall, roster)

# run and save weekly projections
weekly_projections <- calculate_weekly_projections()



### Offseason Analysis ###

# receiving trends table
myfunc_yearly_wr_trends("QB", year, roster)

# year 2 breakouts
myfunc_rookie_wr_breakouts(ps_overall, roster)

nflverse_download(players, folder_path = '/Output/test.csv', file_type = 'csv')
nflverse_releases() %>%
  select(release_name)


