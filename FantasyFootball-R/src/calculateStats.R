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
         yptouch = sum(rushing_yards + receiving_yards) / sum(carries + receptions),
         comp_perc = completions / attempts,
         int_rate = interceptions / attempts,
         td_rate = passing_tds / attempts,
         td_int_rate = td_rate / int_rate,
         aya = (sum(passing_yards) + (20 * sum(passing_tds)) - (45 * sum(interceptions))) / sum(attempts)) %>%
  select(athlete_id, gsis_id, season, ypa, ypc, ypr, ypt, comp_perc, int_rate, td_rate, td_int_rate, aya)
t$ypa <- gsub(Inf, NA, t$ypa)
t$ypc <- gsub(Inf, NA, t$ypc)
t$ypr <- gsub(Inf, NA, t$ypr)
t$yptouch <- gsub(Inf, NA, t$yptouch)
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
# teamStatsTemp <- teamStats %>%
#   select(-games)
# t <- left_join(playerStats, teamStatsTemp) %>%
#   dplyr::filter(league == 'NCAA' & is.na(athlete_id) == FALSE) %>%
#   group_by(athlete_id) %>%
#   mutate(ncaa_ryptpa = sum(receiving_yards) / sum(attempts_team),
#          ncaa_ayptp = sum(rushing_yards + receiving_yards) / sum(carries_team + attempts_team)) %>%
#   select(athlete_id, ncaa_ryptpa, ncaa_ayptp)
# t <- distinct(t, athlete_id, .keep_all = TRUE)
# recruiting <- left_join(recruiting, t, na_matches = "never")



### Changes playerStatsYearly - Requires playerStatsYearly, roster ###

# QBR
t <- nflreadr::load_espn_qbr(league = 'college', seasons = TRUE) %>%
  dplyr::select(-week, -week_text, -name_display, -player_uid, -player_guid, -name_first, -name_last, -name_short, -age, -team_name, -team_short_name, -exp_sack, -penalty, -qbr_raw, -sack, -slug, -team_id, -team_uid, -headshot_href) %>%
  rename(qbr = qbr_total,
         espn_id = player_id)
t <- distinct(t, espn_id, season, .keep_all = TRUE)

t2 <- roster %>%
  dplyr::filter(is.na(athlete_id) == FALSE & is.na(espn_id) == FALSE) %>%
  select(athlete_id, espn_id)

t3 <- left_join(t, t2) %>%
  dplyr::filter(is.na(athlete_id) == FALSE) %>%
  select(athlete_id, season, qbr)

t2 <- roster %>%
  dplyr::filter((position == 'QB' | ncaa_position == 'QB') & is.na(athlete_id) == FALSE) %>%
  select(athlete_id)

t4 <- left_join(t2, anti_join(t, t3) %>% select(espn_id, season, qbr) %>% rename(athlete_id = espn_id)) %>%
  dplyr::filter(is.na(athlete_id) == FALSE & is.na(season) == FALSE)

t3 <- distinct(bind_rows(t3, t4), athlete_id, season, .keep_all = TRUE)
playerStatsYearly <- left_join(playerStatsYearly, t3)


### Changes playerStatsYearly - Requires playerStatsYearly, teamStatsYearly, roster ###

# Dominator Rating
t <- playerStatsYearly %>%
  dplyr::filter(league == 'NCAA' & is.na(athlete_id) == FALSE) %>%
  select(athlete_id, team, season, rushing_yards, rushing_tds, receiving_yards, receiving_tds)
t2 <- teamStatsYearly %>%
  dplyr::filter(league == 'NCAA') %>%
  select(team, season, rushing_yards_team, rushing_tds_team, receiving_yards_team, receiving_tds_team)
t3 <- roster %>%
  dplyr::filter(is.na(athlete_id) == FALSE) %>%
  select(athlete_id, position, ncaa_position, rec_position)
t3$ncaa_position <- if_else(is.na(t3$ncaa_position) == TRUE, if_else(is.na(t3$position) == FALSE, t3$position, t3$rec_position), t3$ncaa_position)
t3 <- t3 %>%
  dplyr::filter(ncaa_position == 'RB' | ncaa_position == 'WR' | ncaa_position == 'TE') %>%
  select(-position, -rec_position)

t <- inner_join(inner_join(t, t2), t3)
t$dom <- if_else(t$ncaa_position == 'RB', (((t$rushing_yards + t$receiving_yards) / (t$rushing_yards_team + t$receiving_yards_team)) + ((t$rushing_tds + t$receiving_tds) / (t$rushing_tds_team + t$receiving_tds_team))) / 2, ((t$receiving_yards / t$receiving_yards_team) + (t$receiving_tds / t$receiving_tds_team)) / 2)
t$w_dom <- ifelse(t$ncaa_position == 'WR' | t$ncaa_position == 'TE', (0.8 * (t$receiving_yards / t$receiving_yards_team) + 0.2 * (t$receiving_tds / t$receiving_tds_team)), NA)

t <- distinct(t, .keep_all = TRUE)
playerStatsYearly <- left_join(playerStatsYearly, t %>%
                                 select(athlete_id, season, dom, w_dom))

t <- t %>%
  dplyr::filter(ncaa_position == 'RB') %>%
  select(athlete_id, team, season, rushing_yards, rushing_tds, receiving_yards, receiving_tds)
t2 <- t %>%
  select(-athlete_id) %>%
  group_by(team, season) %>%
  summarise_all(sum, na.rm=T)
colnames(t2) <- paste(colnames(t2), 'team', sep = '_')
t2 <- t2 %>%
  rename(team = team_team,
         season = season_team)
t <- inner_join(t, t2)

t$bf_dom <- (((t$rushing_yards + t$receiving_yards) / (t$rushing_yards_team + t$receiving_yards_team)) + ((t$rushing_tds + t$receiving_tds) / (t$rushing_tds_team + t$receiving_tds_team))) / 2
t <- distinct(t, .keep_all = TRUE)
playerStatsYearly <- left_join(playerStatsYearly, t %>%
                                 select(athlete_id, season, bf_dom))

arrow::write_parquet(playerStatsYearly, paste(databasePath, 'playerStatsYearly.parquet', sep = ''))



### Changes roster - Requires roster ###

# BMI
roster$bmi <- (roster$wt * 0.453592) / ((roster$ht * 0.0254) ^ 2)
roster$combine_bmi <- (roster$combine_wt * 0.453592) / ((roster$combine_ht * 0.0254) ^ 2)

# Early Declares
roster$early_declare <- ifelse(roster$rookie_year - roster$freshman_year <= 3, 1, NA)

# Speed, Burst, Agility Scores
roster$speed_score <- (roster$combine_wt * 200) / ((roster$forty) ^ 4)
roster$hass <- ifelse(roster$position == 'WR', roster$speed_score * (roster$combine_ht / 73), ifelse(roster$position == 'TE', roster$speed_score * (roster$combine_ht / 76.4), NA))
roster$burst_score <- 89.117 + 31.137 * ((roster$broad_jump - min(roster$broad_jump, na.rm = TRUE)) / (max(roster$broad_jump, na.rm = TRUE) - min(roster$broad_jump, na.rm = TRUE))) + ((roster$vertical - min(roster$vertical, na.rm = TRUE)) / (max(roster$vertical, na.rm = TRUE) - min(roster$vertical, na.rm = TRUE)))
roster$agility_score <- roster$cone + roster$shuttle

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
