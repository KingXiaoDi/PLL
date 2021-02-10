library(tidyverse)
library(lubridate)
library(ggimage)

load_goals <- function() {
  return (read_csv('data/PLL Goals.csv') %>% 
            filter(!is.na(time)) %>%
            group_by(game) %>% 
            mutate(time_q = as.POSIXct(time, format = '%M:%S'),
                   time_r = as.POSIXct(time_q + (4-q)*12*60, format ='%M:%S.%f'),
                   elapsed = 48-(minute(time_r) + second(time_r)/60)) %>% 
            arrange(game, q, desc(time_r)) %>% 
            mutate(between = difftime(lag(time_r), time_r, units='secs'),
                   test = case_when(is.na(between)~ difftime(as.POSIXct('48:00.00', format='%M:%S'), time_r, units='secs'),
                                    TRUE ~ 0),
                   between = case_when(!is.na(between)~between,
                                       !is.na(test)~test)) %>% 
            group_by(date, off) %>% 
            mutate(points = twoPT + 1,
                   score = cumsum(points)) %>% 
            ungroup())
}

get_scores_for_team <- function(goals, team) {
  teamGoals <- goals %>% 
    filter(off == team|def == team) %>%
    group_by(game) %>% 
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
    group_by(team, game) %>% 
    summarize(OT = case_when(last(q) == 5 ~ 1,
                             TRUE ~ 0),
              finalTime = last(time_r),
              finalType = last(situation_n)) %>% 
    mutate(adjustment = 60*minute(finalTime)+second(finalTime))
}

prepare_Game_Goal_Data_For_Plot <- function(game) {
  return (game %>% 
            select(game, elapsed, score, goalTeamA, goalTeamB, scoreTeamA, scoreTeamB) %>%
            add_row(game = 8, elapsed = 0, score = 0, goalTeamA = 1, goalTeamB = 1, scoreTeamA = 0, scoreTeamB = 0) %>% 
            add_row(game = 8, elapsed = 48, score = 0, goalTeamA = 1, goalTeamB = 1, scoreTeamA = 0, scoreTeamB = 0) %>% 
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
  
goals <- load_goals()  
score <- goals %>% 
  select(game, date, off, def, q, time_q, points, score, time_r, between, elapsed)

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



goals %>% 
  mutate(text = sprintf("(%s, %s) %s (%s) scored on %s (%s). Assisted by %s. %s-point goal", 
                        q, time, scorer, off, goalie, def, assist, 1+twoPT)) %>% pull(text)


goals %>%
  group_by(game, off) %>%
  summarize(goals = n()) %>% 
  arrange(game, -goals)

goals %>%
  group_by(game, off) %>%
  summarize(goals = n()) %>% 
  arrange(game, -goals) %>%
  group_by(game) %>% 
  summarize(goals = max(goals))


goals %>% 
  filter(twoPT == 1) %>%
  select(-c(advantage, fast_break))