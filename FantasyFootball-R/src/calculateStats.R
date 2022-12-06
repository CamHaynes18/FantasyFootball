### Calculate year in each league ###
# Changes playerStatsYearly
t <- roster %>%
  select(gsis_id, rookie_year) %>%
  dplyr::filter(is.na(gsis_id) == FALSE & is.na(rookie_year) == FALSE)
playerStatsYearly <- left_join(playerStatsYearly, t, na_matches = "never")

t <- roster %>%
  select(athlete_id, freshman_year) %>%
  dplyr::filter(is.na(athlete_id) == FALSE & is.na(freshman_year) == FALSE)
playerStatsYearly <- left_join(playerStatsYearly, t, na_matches = "never")
playerStatsYearly$year_in_league <- if_else(playerStatsYearly$league == 'NFL', playerStatsYearly$season - playerStatsYearly$rookie_year + 1, playerStatsYearly$season - playerStatsYearly$freshman_year + 1)
playerStatsYearly$year_in_league <- if_else(playerStatsYearly$year_in_league <= 0, NA, playerStatsYearly$year_in_league)
playerStatsYearly <- playerStatsYearly %>%
  select(-rookie_year, -freshman_year)
###


### Calculate first 3 years/career avg fantasy points per game ###
# Changes roster
t <- playerStatsYearly %>%
  dplyr::filter(league == 'NFL' & is.nan(fantasy_points_hppr) == FALSE) %>%
  select(gsis_id, fantasy_points_hppr, games) %>%
  group_by(gsis_id) %>%
  mutate(ppg_career = sum(fantasy_points_hppr) / sum(games)) %>%
  select(-fantasy_points_hppr, -games)
t <- distinct(t, gsis_id, .keep_all = TRUE)
roster <- left_join(roster, t, na_matches = "never")

t <- playerStatsYearly %>%
  dplyr::filter(season == maxYear & games >= 4 & league == 'NFL' & is.nan(fantasy_points_hppr) == FALSE) %>%
  select(gsis_id, fantasy_points_hppr, games) %>%
  group_by(gsis_id) %>%
  mutate(ppg_last_yr = sum(fantasy_points_hppr) / sum(games)) %>%
  select(-fantasy_points_hppr, -games)
t <- distinct(t, gsis_id, .keep_all = TRUE)
roster <- left_join(roster, t, na_matches = "never")

t <- playerStatsYearly %>%
  dplyr::filter(season > (maxYear - 3) & league == 'NFL' & is.nan(fantasy_points_hppr) == FALSE) %>%
  select(gsis_id, fantasy_points_hppr, games) %>%
  group_by(gsis_id) %>%
  mutate(ppg_last_3yr = sum(fantasy_points_hppr) / sum(games)) %>%
  select(-fantasy_points_hppr, -games)
t <- distinct(t, gsis_id, .keep_all = TRUE)
roster <- left_join(roster, t, na_matches = "never")

t <- playerStatsYearly %>%
  dplyr::filter(year_in_league <= 3 & league == 'NFL' & is.nan(fantasy_points_hppr) == FALSE) %>%
  select(gsis_id, fantasy_points_hppr, games) %>%
  group_by(gsis_id) %>%
  mutate(ppg_first_3yr = sum(fantasy_points_hppr) / sum(games)) %>%
  select(-fantasy_points_hppr, -games)
t <- distinct(t, gsis_id, .keep_all = TRUE)
roster <- left_join(roster, t, na_matches = "never")
###

### Calculate rank of each years fantasy finishes ###
# Changes playerStatsYearly
t <- roster %>%
  select(gsis_id, position) %>%
  dplyr::filter(is.na(gsis_id) == FALSE & is.na(position) == FALSE)

t <- left_join(playerStatsYearly, t, na_matches = "never") %>%
  dplyr::filter(league == 'NFL' & is.na(gsis_id) == FALSE & is.na(position) == FALSE & is.na(season) == FALSE & is.nan(fantasy_points_hppr) == FALSE) %>%
  group_by(season, position) %>%
  mutate(pos_rank = rank(-fantasy_points_hppr)) %>%
  select(season, gsis_id, pos_rank, position)

playerStatsYearly <- left_join(playerStatsYearly, t, na_matches = "never") %>%
  select(-position)
###

### Calculate # of top 6/12/24 fantasy finishes ###
# Changes roster
t <- playerStatsYearly %>%
  dplyr::filter(league == 'NFL' & pos_rank <= 6) %>%
  add_count(gsis_id) %>%
  select(gsis_id, n) %>%
  rename(top6 = n)
t <- distinct(t, gsis_id, .keep_all = TRUE)
roster <- left_join(roster, t, na_matches = "never")
t <- playerStatsYearly %>%
  dplyr::filter(league == 'NFL' & pos_rank <= 12) %>%
  add_count(gsis_id) %>%
  select(gsis_id, n) %>%
  rename(top12 = n)
t <- distinct(t, gsis_id, .keep_all = TRUE)
roster <- left_join(roster, t, na_matches = "never")
t <- playerStatsYearly %>%
  dplyr::filter(league == 'NFL' & pos_rank <= 24) %>%
  add_count(gsis_id) %>%
  select(gsis_id, n) %>%
  rename(top24 = n)
t <- distinct(t, gsis_id, .keep_all = TRUE)
roster <- left_join(roster, t, na_matches = "never")
###



### Calculate Advanced Stats ###



### Changes playerStatsYearly - Requires playerStatsYearly ###

# YPA, YPC, YPR, YPT, Completion %, Int Rate, TD Rate, TD-Int Rate
t <- playerStatsYearly %>%
  #dplyr::filter(league == "NCAA" & is.na(athlete_id) == FALSE) %>%
  group_by(athlete_id, gsis_id, season) %>%
  mutate(ypa = passing_yards / attempts,
         ypc = rushing_yards / carries,
         ypr = receiving_yards / receptions,
         ypt = sum(rushing_yards + receiving_yards) / sum(carries + receptions),
         comp_perc = completions / attempts,
         int_rate = interceptions / attempts,
         td_rate = passing_tds / attempts,
         td_int_rate = td_rate / int_rate,
         aya = (sum(passing_yards) + (20 * sum(passing_tds)) - (45 * sum(interceptions))) / sum(attempts)) %>%
  select(athlete_id, gsis_id, season, ypa, ypc, ypr, ypt, comp_perc, int_rate, td_rate, td_int_rate, aya)
t$ypa <- gsub(Inf, NA, t$ypa)
t$ypc <- gsub(Inf, NA, t$ypc)
t$ypr <- gsub(Inf, NA, t$ypr)
t$ypt <- gsub(Inf, NA, t$ypt)
t$comp_perc <- gsub(Inf, NA, t$comp_perc)
t$int_rate <- gsub(Inf, NA, t$int_rate)
t$td_rate <- gsub(Inf, NA, t$td_rate)
t$td_int_rate <- gsub(Inf, NA, t$td_int_rate)
t$aya <- gsub(Inf, NA, t$aya)
t <- na_if(t, NaN)
t <- distinct(t, .keep_all = TRUE)
playerStatsYearly <- left_join(playerStatsYearly, t)



### Changes playerStatsYearly - Requires playerStatsYearly, teamStatsYearly ###

# Receiving Yards per Team Pass Attempt & Average Yards per Team Play
teamStatsTemp <- teamStats %>%
  select(-games)
t <- left_join(playerStats, teamStatsTemp) %>%
  dplyr::filter(league == 'NCAA' & is.na(athlete_id) == FALSE) %>%
  group_by(athlete_id) %>%
  mutate(ncaa_ryptpa = sum(receiving_yards) / sum(attempts_team),
         ncaa_ayptp = sum(rushing_yards + receiving_yards) / sum(carries_team + attempts_team)) %>%
  select(athlete_id, ncaa_ryptpa, ncaa_ayptp)
t <- distinct(t, athlete_id, .keep_all = TRUE)
recruiting <- left_join(recruiting, t, na_matches = "never")



### Changes playerStatsYearly - Requires playerStatsYearly, teamStatsYearly, roster ###

# Dominator Rating
playerInfoTemp <- playerInfo %>%
  select(espn_id, name, position) %>%
  rename(athlete_id = espn_id)
teamStatsTemp <- teamStats %>%
  select(-games)
t <- playerStats %>%
  dplyr::filter(league == 'NCAA' & season == 2021)
t <- left_join(t, playerInfoTemp) %>%
  dplyr::filter(position == 'WR')
t <- left_join(t, teamStatsTemp) %>%
  dplyr::filter(league == "NCAA" & is.na(athlete_id) == FALSE)
group_by(athlete_id, season) %>%
  mutate(dom = sum(receiving_yards) / sum(attempts_team),
         dom = sum(rushing_yards + receiving_yards) / sum(carries_team + attempts_team)) %>%
  select(athlete_id, dom)
t <- distinct(t)

arrow::write_parquet(playerStatsYearly, paste(databasePath, 'playerStatsYearly.parquet', sep = ''))



### Changes roster - Requires roster ###

# BMI
roster$bmi <- (roster$wt * 0.453592) / ((roster$ht * 0.0254) ^ 2)
roster$combine_bmi <- (roster$combine_wt * 0.453592) / ((roster$combine_ht * 0.0254) ^ 2)

# Early Declares
roster$early_declare <- ifelse(roster$rookie_year - roster$freshman_year <= 3, 1, NA)

# Speed, Burst, Agility Scores
roster$speed_score <- (roster$combine_wt * 200) / ((roster$forty) ^ 4)
roster$burst_score <- 89.117 + 31.137 * ((roster$broad_jump - min(roster$broad_jump, na.rm = TRUE)) / (max(roster$broad_jump, na.rm = TRUE) - min(roster$broad_jump, na.rm = TRUE))) + ((roster$vertical - min(roster$vertical, na.rm = TRUE)) / (max(roster$vertical, na.rm = TRUE) - min(roster$vertical, na.rm = TRUE)))
roster$agility_score <- roster$cone + roster$shuttle

# QBR
# t <- nflreadr::load_espn_qbr(league = 'college', seasons = TRUE) %>%
#   dplyr::select(-week, -week_text, -name_display, -player_uid, -player_guid, -name_first, -name_last, -name_short, -age, -team_name, -team_short_name, -exp_sack, -penalty, -qbr_raw, -sack, -slug, -team_id, -team_uid, -headshot_href) %>%
#   rename(qbr = qbr_total,
#          espn_id = player_id)
# t <- distinct(t, espn_id, .keep_all = TRUE)
# t <- roster %>%
#   dplyr::filter(is.na(espn_id) == FALSE & is.na(athlete_id) == FALSE & position == 'QB')

arrow::write_parquet(roster, paste(databasePath, 'roster.parquet', sep = ''))



### Archive ###

# Early College (Freshman/Sophomore) Best College Season Total Yards to Recruiting Table using Left Join by athlete_id
# t <- playerStats %>%
#   dplyr::filter(league == 'NCAA' & year_in_league <= 2 & is.na(athlete_id) == FALSE) %>%
#   select(athlete_id, year_in_league, rushing_yards, receiving_yards)
# t[is.na(t)] <- 0
# t <- t%>%
#   group_by(athlete_id, year_in_league) %>%
#   mutate(ncaa_ec_bcs_yards = rushing_yards + receiving_yards) %>%
#   select(athlete_id, year_in_league, ncaa_ec_bcs_yards)
# t <- t %>%
#   group_by(athlete_id) %>%
#   mutate(ncaa_ec_bcs_yards = max(ncaa_ec_bcs_yards)) %>%
#   select(-year_in_league)
# t <- distinct(t)
# recruiting <- left_join(recruiting, t, na_matches = "never")
# playerInfo <- left_join(playerInfo, t, na_matches = "never")
