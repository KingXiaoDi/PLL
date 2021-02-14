library(tidyverse)
library(lubridate)
library(ggimage)

load_goals <- function() {
  return (read_csv('data/PLL Goals.csv') %>% 
            filter(!is.na(time)) %>%
            group_by(gameID) %>% 
            mutate(time_q = as.POSIXct(time, format = '%M:%S'),
                   time_r = as.POSIXct(time_q + (4-q)*12*60, format ='%M:%S.%f'),
                   elapsed = 48-(minute(time_r) + second(time_r)/60)) %>% 
            arrange(gameID, q, desc(time_r)) %>% 
            mutate(between = difftime(lag(time_r), time_r, units='secs'),
                   test = case_when(is.na(between)~ difftime(as.POSIXct('48:00.00', format='%M:%S'), time_r, units='secs'),
                                    TRUE ~ 0),
                   between = case_when(!is.na(between)~between,
                                       !is.na(test)~test)) %>% 
            group_by(date, off) %>% 
            mutate(points = twoPT + 1,
                   score = cumsum(points),
                   numA = case_when(numA == 'N/A'~NA_real_,
                                    TRUE~as.numeric(numA)),
                   assist = case_when(assist == 'N/A'~NA_character_,
                                      TRUE~assist)) %>% 
            ungroup())
}

add_score_lead_trail <- function(game) {
  
  score <- game %>% 
    pivot_wider(id_cols = c('gameID','goalID'), names_from='off', values_from='score') %>% 
    fill(3:4)
  
  teamA <- colnames(score)[3]
  teamB <- colnames(score)[4]
  
  score %>% 
    mutate(!!as.symbol(teamA) := case_when(is.na(!!as.symbol(teamA))~0,
                                           TRUE~!!as.symbol(teamA)),
           !!as.symbol(teamB) := case_when(is.na(!!as.symbol(teamB))~0,
                                           TRUE~!!as.symbol(teamB)),
           leader = case_when(!!as.symbol(teamA)>=!!as.symbol(teamB)~teamA,
                              !!as.symbol(teamA)<!!as.symbol(teamB)~teamB,
                              TRUE ~ NA_character_),
           leadScore = case_when(!!as.symbol(teamA)>=!!as.symbol(teamB)~!!as.symbol(teamA),
                                 !!as.symbol(teamA)<!!as.symbol(teamB)~!!as.symbol(teamB),
                                 TRUE ~ NA_real_),
           trailer = case_when(!!as.symbol(teamA)>=!!as.symbol(teamB)~teamB,
                               !!as.symbol(teamA)<!!as.symbol(teamB)~teamA,
                               TRUE ~ NA_character_),
           trailScore = case_when(!!as.symbol(teamA)>=!!as.symbol(teamB)~!!as.symbol(teamB),
                                  !!as.symbol(teamA)<!!as.symbol(teamB)~!!as.symbol(teamA),
                                  TRUE ~ NA_real_))
}

get_final_scores <- function(games, goals) {
  
  gameIDs <- games %>% pull(gameID)
  
  results <- tibble()
  
  for (game in gameIDs) {
    
    gameGoals <- goals %>% filter(gameID == game)
    gameScore <- add_score_lead_trail(gameGoals)
    finalScore <- gameScore %>% filter(goalID == max(goalID)) %>% 
      select(c(gameID, 5:8))
    results <- bind_rows(results, finalScore)
  }
  
  return (results)
}

add_score_to_goals <- function(games, goals) {
  
  gameIDs <- games %>% pull(gameID)
  
  goals_with_scores <- tibble()
  
  for (game in gameIDs) {
    
    gameGoals <- goals %>% filter(gameID == game)
    gameScore <- add_score_lead_trail(gameGoals)
    
    game_with_scores <- inner_join(gameGoals, gameScore, by=c('gameID', 'goalID')) %>% 
      mutate(off_score = if_else(off == leader, leadScore, trailScore)-(points),
             def_score = if_else(def == leader, leadScore, trailScore)) %>% 
      select(-c(37:42))
    goals_with_scores <- bind_rows(goals_with_scores, game_with_scores)
  }
  
  return (goals_with_scores)
}

get_scores_for_team <- function(goals, team) {
  teamGoals <- goals %>% 
    filter(off == team|def == team) %>%
    group_by(gameID) %>% 
    mutate(team = team,
           goalTeamA = case_when(off == team ~ 1,
                                 TRUE ~ 0),
           goalTeamB = case_when(def == team ~ 1,
                                 TRUE ~ 0),
           scoreTeamA = cumsum(goalTeamA*points),
           scoreTeamB = cumsum(goalTeamB*points),
           situation_n = case_when(scoreTeamA > scoreTeamB ~ 'leading',
                                 scoreTeamA < scoreTeamB ~ 'trailing',
                                 scoreTeamA == scoreTeamB ~ 'tied',
                                 TRUE ~ NA_character_),
           situation_p = lag(situation_n),
           situation_p = case_when(is.na(situation_p) ~ 'tied',
                                   TRUE ~ situation_p),
           change = case_when(situation_p != situation_n ~ 1,
                                       TRUE ~ 0))
  return (teamGoals)
}

get_total_time_played <- function(data) {
  return (data %>%
            group_by(team) %>% 
            summarize(time = sum(between)))
}

get_time_played_by_score <- function(data) {
  return (data %>%
    group_by(team,situation_p) %>% 
    summarize(time = sum(between)))
}

determine_Adjustments <- function(data) {
  data %>% 
    group_by(team, gameID) %>% 
    summarize(OT = case_when(last(q) == 5 ~ 1,
                             TRUE ~ 0),
              finalTime = last(time_r),
              finalType = last(situation_n)) %>% 
    mutate(adjustment = 60*minute(finalTime)+second(finalTime))
}

prepare_Game_Goal_Data_For_Plot <- function(game) {
  return (game %>% 
            select(gameID, elapsed, score, goalTeamA, goalTeamB, scoreTeamA, scoreTeamB) %>%
            add_row(gameID = 8, elapsed = 0, score = 0, goalTeamA = 1, goalTeamB = 1, scoreTeamA = 0, scoreTeamB = 0) %>% 
            add_row(gameID = 8, elapsed = 48, score = 0, goalTeamA = 1, goalTeamB = 1, scoreTeamA = 0, scoreTeamB = 0) %>% 
            mutate(scoreTeamA = case_when(elapsed == 48 ~max(scoreTeamA),
                                          TRUE ~ scoreTeamA),
                   scoreTeamB = case_when(elapsed == 48 ~max(scoreTeamB),
                                          TRUE ~ scoreTeamB)) %>%
            arrange(elapsed))
}

make_Game_Flow_Plot <- function(game, teamAname, teamBname) {
  colors <- read_csv('data/teamColors.csv')
  plotData <- prepare_Game_Goal_Data_For_Plot(game)
  
  teamA <- plotData %>% filter(goalTeamA == 1)
  teamB <- plotData %>% filter(goalTeamB == 1)
  teamAcolor <- colors %>% filter(Team == teamAname) %>% pull(Primary)
  teamBcolor <- colors %>% filter(Team == teamBname) %>% pull(Primary)
  print (teamAcolor)
  print (teamAcolor[[1]])
  print (teamBcolor)
  goalsMax <- plotData %>% pull(score) %>% max()
  
  return (ggplot() +
            geom_step(aes(elapsed, scoreTeamA), teamA, color = '#0000ff') +
            geom_point(aes(elapsed, scoreTeamA), teamA, color = teamAcolor, size=2) +
            geom_step(aes(elapsed, scoreTeamB), teamB, color = teamBcolor[1]) +
            geom_point(aes(elapsed, scoreTeamB), teamB, color = teamBcolor[1], size=2) +
            #geom_image(aes(elapsed, scoreTeamA, image='./logos/whips.png'), teamA, size=.05) +
            #geom_image(aes(elapsed, scoreTeamB, image='./logos/atlas.png'), teamB, size=.05) +
            theme_bw(base_size=22) +
            scale_x_continuous(breaks=seq(0,48,12)) +
            scale_y_continuous(limits=c(0, goalsMax)) +
            labs(title = sprintf('Game Flow: %s vs %s', teamAname, teamBname)) +
            ylab('Goals') +
            xlab('Time Elapsed'))
}
make_Game_Flow_Plot(whips %>% filter(game == 8) %>% ungroup(), 'Whipsnakes', 'Atlas')

add_Scoring_Summary_String <- function(row) {
  return (sprintf("(%s, %s) %s scored on %s. Assisted by %s. %s-point goal", 
                  q, time, scorer, goalie, assist, 1+twoPT))
}

add_unique_IDs_to_goals <- function(goals) {
  
  roster <- readr::read_csv('data/rosters/combined/2020 combined roster with UUIDs.csv') %>% 
    separate(name, 
             c('first', 'last'), 
             remove=F, 
             extra = 'merge',
             fill = 'right')
  
  goals_with_ID <- inner_join(goals,
                              roster,
                              by=c('off'='team',
                                   'scorer'='last',
                                   'numS'='number')) %>%
    mutate(scorer = name) %>% 
    rename(scorerID = ID,
           scorerPos = position) %>% 
    select(c(season, gameID, q, time, off, def, shot_clock, 
             scorerID, scorer, scorerPos, 
             numA, assist, numG, goalie, twoPT, hand, advantage, fast_break, 
             release, bounce, crease, jump, rebound, points))
  
  goals_plus_assists <- left_join(goals_with_ID,
                                  roster,
                                  by = c('off'='team',
                                         'assist'='last',
                                         'numA'='number')) %>% 
    mutate(assist = name) %>% 
    rename(season = season.x,
           assistID = ID,
           assistPos = position) %>% 
    select(c(season, gameID, q, time, def, shot_clock, 
             scorerID, scorer, scorerPos, assistID, assist, assistPos,
             numG, goalie, twoPT, hand, advantage, fast_break, 
             release, bounce, crease, jump, rebound, points))
  
  all_players_ID <- inner_join(goals_plus_assists,
                               roster,
                               by = c('def'='team',
                                      'goalie'='last',
                                      'numG'='number')) %>% 
    mutate(goalie = name) %>% 
    rename(season = season.x,
           goalieID = ID) %>% 
    select(c(season, gameID, q, time, shot_clock, 
             scorerID, scorer, scorerPos, assistID, assist, assistPos,
             goalieID, goalie, twoPT, hand, advantage, fast_break, 
             release, bounce, crease, jump, rebound, points))
  
  return (all_players_ID)
}

goals <- load_goals()
games <- readr::read_csv('data/games.csv')

add_score_to_goals(games, goals) %>% 
  readr::write_rds('data/goal data for app.rds')

View(goals)


old <- {
  score <- goals %>% 
    select(gameID, date, off, def, q, time_q, points, score, time_r, between, elapsed)
  
  whips <- get_scores_for_team(score, 'Whipsnakes')
  redwoods <- get_scores_for_team(score, 'Redwoods')
  chaos <- get_scores_for_team(score, 'Chaos')
  chrome <- get_scores_for_team(score, 'Chrome')
  archers <- get_scores_for_team(score, 'Archers')
  atlas <- get_scores_for_team(score, 'Atlas')
  dogs <- get_scores_for_team(score, 'Waterdogs')
  
  teamList <- list(archers, atlas, chaos, chrome, redwoods, dogs, whips)
  lapply(teamList, get_total_time_played)
  lapply(teamList, get_time_played_by_score)
  lapply(teamList, determine_Adjustments)
}