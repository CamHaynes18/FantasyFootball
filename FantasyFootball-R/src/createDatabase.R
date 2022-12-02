### Build Player & Team Stats Database ###
# load weekly NFL player stats back to 1999
print('Create Database Started')
rm(playerStatsWeekly, teamStatsWeekly, playerStatsYearly, teamStatsYearly)
gc(verbose = FALSE, reset = TRUE)
future::plan('multisession', workers = 8)

maxYear <- nflreadr::get_latest_season()
playerStatsWeekly <- nflreadr::load_player_stats(TRUE, 'offense') %>%
  dplyr::filter(is.na(player_id) == FALSE & (position == "QB" | position == "RB" | position == "WR" | position == "TE")) %>%
  dplyr::rename(gsis_id = player_id,
                team = recent_team) %>%
  select(-player_name, -player_display_name, -position, -position_group, -headshot_url)
playerStatsWeekly$fantasy_points_hppr <- rowMeans(subset(playerStatsWeekly, select = c(fantasy_points, fantasy_points_ppr)), na.rm = TRUE)
playerStatsWeekly$league <- 'NFL'

teamStatsWeekly <- playerStatsWeekly %>%
  select(-gsis_id, -season_type, -league) %>%
  group_by(team, season, week) %>%
  summarise_all(sum, na.rm=T)
colnames(teamStatsWeekly) <- paste(colnames(teamStatsWeekly), 'team', sep = '_')
teamStatsWeekly <- teamStatsWeekly %>%
  rename(team = team_team,
         season = season_team,
         week = week_team)
teamStatsWeekly$league <- 'NFL'

print('NFL Weekly Stats Loaded')


# load yearly NFL player stats back to 1999
#playerStatsYearly <- nflfastR::calculate_player_stats(nflreadr::load_pbp(seasons = ), weekly = FALSE)
playerStatsYearly <- playerStatsWeekly %>%
  group_by(gsis_id, season) %>%
  summarise(games = n())
t <- playerStatsWeekly %>%
  select(-team, -week, -season_type, -league) %>%
  group_by(gsis_id, season) %>%
  summarise_all(sum, na.rm=T)
playerStatsYearly <- inner_join(playerStatsYearly, t, na_matches = "never")
playerStatsYearly$league <- 'NFL'

teamStatsYearly <- teamStatsWeekly %>%
  group_by(team, season) %>%
  summarise(games = n()) #max(week)
t <- teamStatsWeekly %>%
  select(-week, -league) %>%
  group_by(team, season) %>%
  summarise_all(sum, na.rm=T)
teamStatsYearly <- inner_join(teamStatsYearly, t)
teamStatsYearly$league <- 'NFL'

print('NFL Yearly Stats Loaded')


# load college player stats back to 2004
currentWeek <- nflreadr::get_current_week()
for(year in maxYear:2004)
{
  if (year == maxYear)
  {
    t <- cfbfastR::cfbd_stats_season_player(year, season_type = "both", team = NULL, conference = NULL, start_week = 1, end_week = 1, category = "passing") %>%
      select(athlete_id, team, passing_completions, passing_att, passing_yds, passing_td, passing_int)
    t2 <- cfbfastR::cfbd_stats_season_player(year, season_type = "both", team = NULL, conference = NULL, start_week = 1, end_week = 1, category = "rushing") %>%
      select(athlete_id, team, rushing_car, rushing_yds, rushing_td)
    t <- full_join(t, t2, na_matches = "never")
    t2 <- cfbfastR::cfbd_stats_season_player(year, season_type = "both", team = NULL, conference = NULL, start_week = 1, end_week = 1, category = "receiving") %>%
      select(athlete_id, team, receiving_rec, receiving_yds, receiving_td)
    t <- full_join(t, t2, na_matches = "never")
    t2 <- cfbfastR::cfbd_stats_season_player(year, season_type = "both", team = NULL, conference = NULL, start_week = 1, end_week = 1, category = "kickReturns") %>%
      select(athlete_id, team, kick_returns_no, kick_returns_yds, kick_returns_td)
    t <- full_join(t, t2, na_matches = "never")
    t2 <- cfbfastR::cfbd_stats_season_player(year, season_type = "both", team = NULL, conference = NULL, start_week = 1, end_week = 1, category = "puntReturns") %>%
      select(athlete_id, team, punt_returns_no, punt_returns_yds, punt_returns_td)
    playerStatsNcaa <- full_join(t, t2, na_matches = "never")
    playerStatsNcaa$season <- year
    playerStatsNcaa$week <- 1
    playerStatsNcaa$league <- 'NCAA'
    
    # teamStatsNcaa <- cfbfastR::cfbd_stats_season_team(year, season_type = "both", team = NULL, conference = NULL, start_week = 1, end_week = 1) %>%
    #   select(team, conference, season, pass_comps, pass_atts, net_pass_yds, pass_TDs, rush_atts, rush_yds, rush_TDs)
    # teamStatsNcaa$week <- 1
    # teamStatsNcaa$league <- 'NCAA'
    
    startWeek <- 2
    endWeek <- currentWeek
  }
  else if (year <= 2013)
  {
    startWeek <- 1
    endWeek <- 14
  }
  else
  {
    startWeek <- 1
    endWeek <- 15
  }
  for (week in startWeek:endWeek)
  {
    t <- cfbfastR::cfbd_stats_season_player(year, season_type = "both", team = NULL, conference = NULL, start_week = week, end_week = week, category = "passing") %>%
      select(athlete_id, team, passing_completions, passing_att, passing_yds, passing_td, passing_int)
    t2 <- cfbfastR::cfbd_stats_season_player(year, season_type = "both", team = NULL, conference = NULL, start_week = week, end_week = week, category = "rushing") %>%
      select(athlete_id, team, rushing_car, rushing_yds, rushing_td)
    t <- full_join(t, t2, na_matches = "never")
    t2 <- cfbfastR::cfbd_stats_season_player(year, season_type = "both", team = NULL, conference = NULL, start_week = week, end_week = week, category = "receiving") %>%
      select(athlete_id, team, receiving_rec, receiving_yds, receiving_td)
    t <- full_join(t, t2, na_matches = "never")
    if (year >= 2009)
    {
      t2 <- cfbfastR::cfbd_stats_season_player(year, season_type = "both", team = NULL, conference = NULL, start_week = week, end_week = week, category = "kickReturns") %>%
        select(athlete_id, team, kick_returns_no, kick_returns_yds, kick_returns_td)
      t <- full_join(t, t2, na_matches = "never")
    }
    if (!(year == 2016 & week == 15))
    {
      t2 <- cfbfastR::cfbd_stats_season_player(year, season_type = "both", team = NULL, conference = NULL, start_week = week, end_week = week, category = "puntReturns") %>%
        select(athlete_id, team, punt_returns_no, punt_returns_yds, punt_returns_td)
      t <- full_join(t, t2, na_matches = "never")
    }
    t$season <- year
    t$week <- week
    t$league <- 'NCAA'
    playerStatsNcaa <- playerStatsNcaa %>% bind_rows(t)
    
    # teamStatsTemp <- cfbfastR::cfbd_stats_season_team(year, season_type = "both", team = NULL, conference = NULL, start_week = week, end_week = week) %>%
    #   select(team, conference, season, pass_comps, pass_atts, net_pass_yds, pass_TDs, rush_atts, rush_yds, rush_TDs)
    # teamStatsTemp$week <- week
    # teamStatsTemp$league <- 'NCAA'
    # teamStatsNcaa <- teamStatsNcaa %>% bind_rows(teamStatsTemp)
  }
}

playerStatsNcaaWeekly <- playerStatsNcaa %>%
  dplyr::filter(is.na(athlete_id) == FALSE) %>%
  dplyr::rename(completions = passing_completions,
                attempts = passing_att,
                passing_yards = passing_yds,
                passing_tds = passing_td,
                interceptions = passing_int,
                carries = rushing_car,
                rushing_yards = rushing_yds,
                rushing_tds = rushing_td,
                receptions = receiving_rec,
                receiving_yards = receiving_yds,
                receiving_tds = receiving_td)

teamStatsNcaaWeekly <- playerStatsNcaaWeekly %>%
  select(-athlete_id, -league) %>%
  group_by(team, season, week) %>%
  summarise_all(sum, na.rm=T)
colnames(teamStatsNcaaWeekly) <- paste(colnames(teamStatsNcaaWeekly), 'team', sep = '_')
teamStatsNcaaWeekly <- teamStatsNcaaWeekly %>%
  rename(team = team_team,
         season = season_team,
         week = week_team)
teamStatsNcaaWeekly$league <- 'NCAA'

playerStatsNcaaYearly <- playerStatsNcaaWeekly %>%
  group_by(athlete_id, season) %>%
  summarise(games = n())
t <- playerStatsNcaaWeekly %>%
  select(-team, -week, -league) %>%
  group_by(athlete_id, season) %>%
  summarise_all(sum, na.rm=T)
playerStatsNcaaYearly <- inner_join(playerStatsNcaaYearly, t, na_matches = "never")
playerStatsNcaaYearly$league <- 'NCAA'

teamStatsNcaaYearly <- teamStatsNcaaWeekly %>%
  group_by(team, season) %>%
  summarise(games = n())
t <- teamStatsNcaaWeekly %>%
  select(-week, -league) %>%
  group_by(team, season) %>%
  summarise_all(sum, na.rm=T)
teamStatsNcaaYearly <- inner_join(teamStatsNcaaYearly, t, na_matches = "never")
teamStatsNcaaYearly$league <- 'NCAA'

print('NCAA Stats Loaded')

playerStatsWeekly <- playerStatsWeekly %>% bind_rows(playerStatsNcaaWeekly)
playerStatsYearly <- playerStatsYearly %>% bind_rows(playerStatsNcaaYearly)
teamStatsWeekly <- teamStatsWeekly %>% bind_rows(teamStatsNcaaWeekly)
teamStatsYearly <- teamStatsYearly %>% bind_rows(teamStatsNcaaYearly)

rm(year, currentWeek, week, startWeek, endWeek, playerStatsNcaa, playerStatsNcaaWeekly, playerStatsNcaaYearly, teamStatsNcaaWeekly, teamStatsNcaaYearly, t, t2)

arrow::write_parquet(playerStatsWeekly, 'Y:/Fantasy Football/Database/playerStatsWeekly.parquet')
arrow::write_parquet(playerStatsYearly, 'Y:/Fantasy Football/Database/playerStatsYearly.parquet')
arrow::write_parquet(teamStatsWeekly, 'Y:/Fantasy Football/Database/teamStatsWeekly.parquet')
arrow::write_parquet(teamStatsYearly, 'Y:/Fantasy Football/Database/teamStatsYearly.parquet')

print('Player and Team Stats Saved')



### Build & Clean Player Information Database ###
rm(roster)
gc(verbose = FALSE, reset = TRUE)

#maxYear <- nflreadr::get_latest_season()

roster <- nflreadr::load_rosters(maxYear:2000) %>%
  dplyr::filter(is.na(gsis_id) == FALSE & (position == 'QB' | position == 'RB' | position == 'WR' | position == 'TE')) %>%
  select(-depth_chart_position, -jersey_number, -first_name, -last_name, -college, -sportradar_id, -rotowire_id, -fantasy_data_id, -years_exp, -headshot_url, -ngs_position, -week, -game_type, -status_description_abbr, -football_name, -esb_id, -gsis_it_id, -smart_id, -entry_year, -draft_club) %>%
  rename(name = full_name,
         ht = height,
         wt = weight,
         draft_pick = draft_number)
roster <- roster[order(-roster$season),]
roster <- distinct(roster, gsis_id, .keep_all = TRUE)
roster$rookie_year <- as.integer(roster$rookie_year)
roster$draft_pick <- as.integer(roster$draft_pick)


# Load NFL Combine Data, Left Join onto existing players using pfr_id & cfb_id
combine <- nflreadr::load_combine(TRUE) %>%
  dplyr::filter(is.na(pfr_id) == FALSE & (pos == 'QB' | pos == 'RB' | pos == 'WR' | pos == 'TE')) %>%
  select(-player_name, -draft_year, -cfb_id, -pos, -school) %>%
  rename(rookie_year = season,
         combine_wt = wt,
         draft_pick = draft_ovr)
combine <- combine[order(-combine$rookie_year),]
combine <- distinct(combine, pfr_id, .keep_all = TRUE)

# Transform ht into inches
combine <- combine %>%
  separate(ht, c('feet', 'inches'), sep = '-', convert = TRUE, remove = FALSE) %>%
  mutate(combine_ht = 12*feet + inches) %>%
  select(-ht, -feet, -inches)

# Load NFL Draft Data, Left Join onto existing players using pfr_id
draft <- nflreadr::load_draft_picks(TRUE) %>%
  dplyr::filter(is.na(pfr_player_id) == FALSE & (position == 'QB' | position == 'RB' | position == 'WR' | position == 'TE')) %>%
  select(-pfr_player_name, -cfb_player_id, -position, -category, -side, -college, -seasons_started, -w_av, -car_av, -dr_av, -pass_completions, -pass_attempts, -pass_yards, -pass_tds, -pass_ints, -rush_atts, -rush_yards, -rush_tds, -receptions, -rec_yards, -rec_tds, -def_solo_tackles, -def_ints, -def_sacks) %>%
  dplyr::rename(rookie_year = season,
                draft_round = round,
                draft_pick = pick,
                draft_team = team,
                pfr_id = pfr_player_id,
                draft_age = age,
                final_season = to)
draft <- draft[order(-draft$rookie_year),]
draft <- distinct(draft, pfr_id, .keep_all = TRUE)


t <- left_join(draft, combine %>% select(-rookie_year, -draft_team, -draft_round, -draft_pick), na_matches = "never")
t <- t %>% bind_rows(anti_join(combine, draft %>% select(-rookie_year, -draft_team, -draft_round, -draft_pick), na_matches = "never"))

t2 <- inner_join(roster, t %>% select(-gsis_id, -rookie_year, -draft_pick), na_matches = "never")
t2 <- bind_rows(t2, inner_join(anti_join(roster, t2, by = 'pfr_id', na_matches = "never") %>% select(-pfr_id), t %>% select(-rookie_year, -draft_pick), na_matches = "never"))
t2 <- bind_rows(t2, inner_join(anti_join(roster, t2, by = c('pfr_id', 'gsis_id'), na_matches = "never") %>% dplyr::filter(is.na(pfr_id) == TRUE) %>% select(-pfr_id), t %>% select(-gsis_id), by = c('rookie_year', 'draft_pick'), na_matches = "never"))

t3 <- anti_join(roster, t2, by = 'pfr_id', na_matches = "never")
t3 <- anti_join(t3, t2, by = 'gsis_id', na_matches = "never")
t3 <- anti_join(t3, t2, by = c('rookie_year', 'draft_pick'), na_matches = "never")

roster <- t2 %>% bind_rows(t3)
roster$final_season <- ifelse(is.na(roster$final_season), roster$season - roster$rookie_year + 1, roster$final_season)
roster <- distinct(roster, gsis_id, .keep_all = TRUE) %>%
  select(-season)
rm(combine, draft, t, t2, t3)


# Load all NCAA player data
#rm(recruiting)
#gc(verbose = FALSE, reset = TRUE)

rosterNcaa <- cfbfastR::load_cfb_rosters(TRUE) %>%
  dplyr::filter(is.na(athlete_id) == FALSE & (position == 'QB' | position == 'RB' | position == 'WR' | position == 'TE')) %>%
  select(-jersey, -home_city, -home_state, -home_country, -home_latitude, -home_longitude, -home_county_fips, -recruit_ids, -headshot_url) %>%
  rename(ncaa_team = team,
         ncaa_wt = weight,
         ncaa_ht = height,
         ncaa_year = year,
         ncaa_position = position,
         ncaa_final_season = season)
rosterNcaa <- rosterNcaa[order(-rosterNcaa$ncaa_final_season),]
rosterNcaa <- distinct(rosterNcaa, athlete_id, .keep_all = TRUE)
rosterNcaa$name <- paste(rosterNcaa$first_name, rosterNcaa$last_name)
rosterNcaa <- rosterNcaa %>%
  select(-first_name, -last_name)


recruiting <- cfbfastR::cfbd_recruiting_player(maxYear + 1)
for(year in maxYear:2000)
{
  t <- cfbfastR::cfbd_recruiting_player(year)
  recruiting <- recruiting %>% bind_rows(t)
}

recruiting <- recruiting %>%
  dplyr::filter(is.na(athlete_id) == FALSE & (position == 'QB' | position == 'PRO' | position == 'DUAL' | position == 'APB' | position == 'RB' | position == 'WR' | position == 'TE' | position == 'ATH')) %>%
  select(-id, -school, -city, -state_province, -country, -hometown_info_latitude, -hometown_info_longitude, -hometown_info_fips_code) %>%
  dplyr::rename(freshman_year = year,
                rec_position = position,
                rec_ht = height,
                rec_wt = weight)
recruiting <- recruiting[order(-recruiting$freshman_year),]
recruiting <- distinct(recruiting, athlete_id, .keep_all = TRUE)
recruiting$rec_position <- gsub('PRO', 'QB', recruiting$rec_position)
recruiting$rec_position <- gsub('DUAL', 'QB', recruiting$rec_position)


draftNcaa <- cfbfastR::cfbd_draft_picks(maxYear)
for(year in (maxYear - 1):2000)
{
  t <- cfbfastR::cfbd_draft_picks(year)
  draftNcaa <- draftNcaa %>% bind_rows(t)
}
draftNcaa <- draftNcaa %>%
  dplyr::filter(is.na(college_athlete_id) == FALSE & (position == 'Quarterback' | position == 'Running Back' | position == 'Wide Receiver' | position == 'Tight End')) %>%
  select(-nfl_athlete_id, -college_id, -college_team, -nfl_team, -pick, -height, -weight, -hometown_info_city, -hometown_info_state, -hometown_info_country, -hometown_info_latitude, -hometown_info_longitude, -hometown_info_county_fips) %>%
  rename(athlete_id = college_athlete_id,
         conference = college_conference,
         rookie_year = year,
         draft_pick = overall,
         draft_round = round,
         ncaa_position = position)
draftNcaa <- draftNcaa[order(-draftNcaa$rookie_year),]
draftNcaa <- distinct(draftNcaa, athlete_id, .keep_all = TRUE)
draftNcaa$athlete_id <- as.character(draftNcaa$athlete_id)
draftNcaa$ncaa_position <- gsub('Quarterback', 'QB', draftNcaa$ncaa_position)
draftNcaa$ncaa_position <- gsub('Running Back', 'RB', draftNcaa$ncaa_position)
draftNcaa$ncaa_position <- gsub('Wide Receiver', 'WR', draftNcaa$ncaa_position)
draftNcaa$ncaa_position <- gsub('Tight End', 'TE', draftNcaa$ncaa_position)


t <- left_join(rosterNcaa, recruiting %>% select(-name), na_matches = "never")
rosterNcaa <- bind_rows(t, anti_join(recruiting, rosterNcaa %>% select(-name), na_matches = "never"))
t <- left_join(rosterNcaa, draftNcaa %>% select(-name, -ncaa_position), na_matches = "never")
rosterNcaa <- bind_rows(t, anti_join(draftNcaa, rosterNcaa %>% select(-name, -ncaa_position), na_matches = "never"))


rosterNcaa$freshman_year <- ifelse(is.na(rosterNcaa$freshman_year), rosterNcaa$ncaa_final_season - rosterNcaa$ncaa_year + 1, rosterNcaa$freshman_year)
rosterNcaa <- distinct(rosterNcaa, athlete_id, .keep_all = TRUE) %>%
  select(-ncaa_year)

rm(recruiting, draftNcaa, t)

t <- inner_join(roster, rosterNcaa %>% select(-name, -rookie_year, -draft_pick, -draft_round), keep = TRUE, by = c('espn_id' = 'athlete_id'), na_matches = "never")
t <- bind_rows(t, inner_join(anti_join(roster, t, by = c('espn_id' = 'athlete_id'), na_matches = "never"), rosterNcaa %>% select(-name, -draft_round), by = c('rookie_year', 'draft_pick'), na_matches = "never"))

#rosterNcaa$merge_name <- nflreadr::clean_player_names(rosterNcaa$name, lowercase = TRUE)
#t$merge_name <- nflreadr::clean_player_names(t$name, lowercase = TRUE)
#t <- t %>% bind_rows(inner_join(anti_join(roster, t, by = c('rookie_year', 'draft_pick'), na_matches = "never"), rosterNcaa %>% select(-name, -draft_round), by = c('rookie_year', 'draft_pick'), na_matches = "never"))

t2 <- anti_join(roster, t, by = c('espn_id'), na_matches = "never")
t2 <- anti_join(t2, t, by = c('rookie_year', 'draft_pick'), na_matches = "never")
t <- bind_rows(t, t2)

t2 <- anti_join(rosterNcaa, t, by = c('athlete_id'), na_matches = "never")
t2 <- anti_join(t2, t, by = c('rookie_year', 'draft_pick'), na_matches = "never")
t <- bind_rows(t, t2)

roster <- bind_rows(t, t2)

rm(year, rosterNcaa, t, t2)

arrow::write_parquet(roster, 'Y:/Fantasy Football/Database/roster.parquet')



# Load Relative Athletic Score .csv File, Left Join using name, position, rookie_year
ras <- read.csv('Y:/Fantasy Football/Database/ras.csv') %>%
  select(-Link, -College) %>%
  dplyr::rename(merge_name = Name,
                position = Pos,
                rookie_year = Year,
                ras = RAS,
                all_time_ras = AllTime) %>%
  dplyr::filter(position == 'QB' | position == 'RB' | position == 'WR' | position == 'TE')
ras$merge_name <- nflreadr::clean_player_names(ras$merge_name, lowercase = TRUE)
roster$merge_name <- nflreadr::clean_player_names(roster$name, lowercase = TRUE)
ras <- distinct(ras, .keep_all = TRUE)

roster <- left_join(roster, ras, na_matches = "never")
rm(ras)
roster <- roster %>% select(-merge_name)


# Calculate BMI
roster$bmi <- (roster$wt * 0.453592) / ((roster$ht * 0.0254) ^ 2)
roster$combine_bmi <- (roster$combine_wt * 0.453592) / ((roster$combine_ht * 0.0254) ^ 2)


# Calculate Early Declares
roster$early_declare <- ifelse(roster$rookie_year - roster$freshman_year <= 3, 1, NA)



### College Calculated Stats ###
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

rm(t)
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

rm(t)
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

rm(t)
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

rm(t)
###


# Add Following Advanced Stats to Yearly Player stats Table using Left Join by athlete_id
# Completion %, Interceptions per Pass Attempt, YPA, YPC, & YPR
t <- playerStatsYearly %>%
  dplyr::filter(league == "NCAA" & is.na(athlete_id) == FALSE) %>%
  group_by(athlete_id, season) %>%
  mutate(comp_perc = sum(completions) / sum(attempts),
         int_ratio = sum(interceptions) / sum(attempts),
         ypa = sum(passing_yards) / sum(attempts),
         ypc = sum(rushing_yards) / sum(carries),
         ypr = sum(receiving_yards) / sum(receptions),
         ypt = sum(rushing_yards + receiving_yards) / sum(carries + receptions))
t$comp_perc <- gsub(Inf, NA, t$ncaa_comp_perc)
t$int_ratio <- gsub(Inf, NA, t$ncaa_int_ratio)
t$ypa <- gsub(Inf, NA, t$ncaa_ypa)
t$ypc <- gsub(Inf, NA, t$ncaa_ypc)
t$ypr <- gsub(Inf, NA, t$ncaa_ypr)
t$ypt <- gsub(Inf, NA, t$ncaa_ypt)
t <- distinct(t, athlete_id, .keep_all = TRUE)
playerStatsYearly <- left_join(playerStatsYearly, t, na_matches = "never")

# Add Receiving Yards per Team Pass Attempt & Average Yards per Team Play
teamStatsTemp <- teamStats %>%
  select(-games)
t <- left_join(playerStats, teamStatsTemp) %>%
  dplyr::filter(league == "NCAA" & is.na(athlete_id) == FALSE) %>%
  group_by(athlete_id) %>%
  mutate(ncaa_ryptpa = sum(receiving_yards) / sum(attempts_team),
         ncaa_ayptp = sum(rushing_yards + receiving_yards) / sum(carries_team + attempts_team)) %>%
  select(athlete_id, ncaa_ryptpa, ncaa_ayptp)
t <- distinct(t, athlete_id, .keep_all = TRUE)
recruiting <- left_join(recruiting, t, na_matches = "never")



# QBR
t <- nflreadr::load_espn_qbr(league = 'college', seasons = TRUE) %>%
  dplyr::select(-week, -week_text, -name_display, -player_uid, -player_guid, -name_first, -name_last, -name_short, -age, -team_name, -team_short_name, -exp_sack, -penalty, -qbr_raw, -sack, -slug, -team_id, -team_uid, -headshot_href) %>%
  rename(qbr = qbr_total,
         espn_id = player_id)
t <- distinct(t, espn_id, .keep_all = TRUE)

t <- roster %>%
  dplyr::filter(is.na(espn_id) == FALSE & is.na(athlete_id) == FALSE & position == 'QB')

# College Dominator Rating
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










# Add Early College (Freshman/Sophomore) Best College Season Total Yards to Recruiting Table using Left Join by athlete_id
t <- playerStats %>%
  dplyr::filter(league == "NCAA" & year_in_league <= 2 & is.na(athlete_id) == FALSE) %>%
  select(athlete_id, year_in_league, rushing_yards, receiving_yards)
t[is.na(t)] <- 0
t <- t%>%
  group_by(athlete_id, year_in_league) %>%
  mutate(ncaa_ec_bcs_yards = rushing_yards + receiving_yards) %>%
  select(athlete_id, year_in_league, ncaa_ec_bcs_yards)
t <- t %>%
  group_by(athlete_id) %>%
  mutate(ncaa_ec_bcs_yards = max(ncaa_ec_bcs_yards)) %>%
  select(-year_in_league)
t <- distinct(t)
recruiting <- left_join(recruiting, t, na_matches = "never")
playerInfo <- left_join(playerInfo, t, na_matches = "never")


### Changes Roster - Requires Roster ###
# Speed Score
roster$speed_score <- (roster$combine_wt * 200) / ((roster$forty) ^ 4)
roster$burst_score <- (roster$vertical + ((max(roster$broad_jump, na.rm = TRUE) - max(roster$vertical, na.rm = TRUE)) / 2)) + (roster$broad_jump - ((max(roster$broad_jump, na.rm = TRUE) - max(roster$vertical, na.rm = TRUE)) / 2))
roster$burst_score1 <- (roster$vertical + ((max(roster$broad_jump, na.rm = TRUE) - max(roster$vertical, na.rm = TRUE)) / 2))
roster$burst_score2 <- (roster$broad_jump - ((max(roster$broad_jump, na.rm = TRUE) - max(roster$vertical, na.rm = TRUE)) / 2))
roster$agility_score <- roster$cone + roster$shuttle
t <- roster %>% select(name, combine_wt, forty, vertical, broad_jump, cone, shuttle, speed_score, burst_score1, burst_score2, burst_score, agility_score)


arrow::write_parquet(roster, 'Y:/Fantasy Football/Database/roster.parquet')
