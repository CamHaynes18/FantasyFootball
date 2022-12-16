### Calculate Advanced Stats ###



### Changes playerStatsYearly - Requires playerStatsYearly ###

# YPA, YPC, YPR, YPT, Completion %, Int Rate, TD Rate, TD-Int Rate
playerStatsYearly$ypa <- gsub(Inf, NA, playerStatsYearly$passing_yards / playerStatsYearly$attempts)
playerStatsYearly$ypc <- gsub(Inf, NA, playerStatsYearly$rushing_yards / playerStatsYearly$carries)
playerStatsYearly$ypr <- gsub(Inf, NA, playerStatsYearly$receiving_yards / playerStatsYearly$receptions)
playerStatsYearly$yptouch <- gsub(Inf, NA, (playerStatsYearly$rushing_yards + playerStatsYearly$receiving_yards) / (playerStatsYearly$carries + playerStatsYearly$receptions))
playerStatsYearly$comp_perc <- gsub(Inf, NA, playerStatsYearly$completions / playerStatsYearly$attempts)
playerStatsYearly$int_rate <- gsub(Inf, NA, playerStatsYearly$interceptions / playerStatsYearly$attempts)
playerStatsYearly$td_rate <- gsub(Inf, NA, playerStatsYearly$passing_tds / playerStatsYearly$attempts)
playerStatsYearly$aya <- gsub(Inf, NA, (playerStatsYearly$passing_yards + (20 * playerStatsYearly$passing_tds)) - ((45 * playerStatsYearly$interceptions) / playerStatsYearly$attempts))
playerStatsYearly$td_int_rate <- gsub(Inf, NA, as.numeric(playerStatsYearly$td_rate) / as.numeric(playerStatsYearly$int_rate))

playerStatsYearly <- na_if(playerStatsYearly, NaN)



### Changes playerStatsYearly - Requires playerStatsYearly, teamStatsYearly ###

# RYPTPA, AYPTP, 
t <- teamStatsYearly %>%
  select(team, season, completions_team, attempts_team, passing_yards_team, passing_tds_team, carries_team, rushing_yards_team, rushing_tds_team, receptions_team, receiving_yards_team, receiving_tds_team)
t <- left_join(playerStatsYearly, t, na_matches = "never")

t$carries_ms = gsub(Inf, NA, t$carries / t$carries_team)
t$rushing_yards_ms = gsub(Inf, NA, t$rushing_yards / t$rushing_yards_team)
t$rushing_tds_ms = gsub(Inf, NA, t$rushing_tds / t$rushing_tds_team)
t$ypc_over_team <- gsub(Inf, NA, (t$rushing_yards / t$carries) - ((t$rushing_yards_team - t$rushing_yards) / (t$carries_team - t$carries)))
t$receptions_ms = gsub(Inf, NA, t$receptions / t$receptions_team)
t$receiving_yards_ms = gsub(Inf, NA, t$receiving_yards / t$receiving_yards_team)
t$receiving_tds_ms = gsub(Inf, NA, t$receiving_tds / t$receiving_tds_team)
t$ryptpa = gsub(Inf, NA, t$receiving_yards / t$attempts_team)
t$yptp = gsub(Inf, NA, (t$passing_yards + t$rushing_yards + t$receiving_yards) / (t$carries_team + t$attempts_team))
t$ayptp = gsub(Inf, NA, (t$passing_yards + t$rushing_yards + (2 * t$receiving_yards)) / (t$carries_team + t$attempts_team))
t$tdptp = gsub(Inf, NA, (t$passing_tds + t$rushing_tds + t$receiving_tds) / (t$carries_team + t$attempts_team))

t <- na_if(t, NaN) %>%
  select(athlete_id, gsis_id, season, carries_ms, rushing_yards_ms, rushing_tds_ms, ypc_over_team, receptions_ms, receiving_yards_ms, receiving_tds_ms, ryptpa, yptp, ayptp, tdptp)

playerStatsYearly <- left_join(playerStatsYearly, t)



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



### Changes roster - Requires roster ###

# BMI
roster$bmi <- (roster$wt * 0.453592) / ((as.numeric(roster$ht) * 0.0254) ^ 2)
roster$combine_bmi <- (roster$combine_wt * 0.453592) / ((roster$combine_ht * 0.0254) ^ 2)

# Early Declares
roster$early_declare <- ifelse(roster$rookie_year - roster$freshman_year <= 3, 1, NA)

# Speed, Burst, Agility Scores
roster$speed_score <- (roster$combine_wt * 200) / ((roster$forty) ^ 4)
roster$hass <- ifelse(roster$position == 'WR', roster$speed_score * (roster$combine_ht / 73), ifelse(roster$position == 'TE', roster$speed_score * (roster$combine_ht / 76.4), NA))
roster$burst_score <- 89.117 + 31.137 * ((roster$broad_jump - min(roster$broad_jump, na.rm = TRUE)) / (max(roster$broad_jump, na.rm = TRUE) - min(roster$broad_jump, na.rm = TRUE))) + ((roster$vertical - min(roster$vertical, na.rm = TRUE)) / (max(roster$vertical, na.rm = TRUE) - min(roster$vertical, na.rm = TRUE)))
roster$agility_score <- roster$cone + roster$shuttle



### Changes roster - Requires roster, playerStatsYearly ###

# Breakout Year, Breakout Age 
t <- playerStatsYearly %>%
  dplyr::filter(is.na(athlete_id) == FALSE & (dom >= 0.15 | qbr >= 50)) %>%
  select(athlete_id, season, year_in_league, dom, qbr)

t2 <- roster %>%
  dplyr::filter(is.na(athlete_id) == FALSE) %>%
  select(athlete_id, birth_date, position, ncaa_position, rec_position)
t2$ncaa_position <- if_else(is.na(t2$ncaa_position) == TRUE, if_else(is.na(t2$position) == FALSE, t2$position, t2$rec_position), t2$ncaa_position)
t2 <- t2 %>%
  dplyr::filter(ncaa_position == 'QB' | ncaa_position == 'RB' | ncaa_position == 'WR' | ncaa_position == 'TE') %>%
  select(-position, -rec_position)

t <- left_join(t, t2) %>%
  dplyr::filter(ncaa_position == 'QB' | ncaa_position == 'RB' | (ncaa_position == 'WR'  & dom >= 0.2) | ncaa_position == 'TE')


t <- t[order(t$season),]
t <- distinct(t, athlete_id, .keep_all = TRUE)
t$boy <- t$year_in_league
t$bos <- t$season
t$boa <- lubridate::time_length(difftime(as.Date(paste(t$season, '09-01', sep = '-')), t$birth_date), 'years')
t <- t %>%
  select(athlete_id, bos, boy, boa)

roster <- left_join(roster, t)

rm(t, t2, t3, t4)



# Write to Parquet
arrow::write_parquet(playerStatsYearly, paste(databasePath, 'playerStatsYearly.parquet', sep = ''))
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
