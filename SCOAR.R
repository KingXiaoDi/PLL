add_Scoring_Summary_String <- function(row) {
  return (sprintf("(%s, %s) %s scored on %s. Assisted by %s. %s-point goal", 
                 q, time, scorer, goalie, assist, 1+twoPT))
}

goals %>% 
  mutate(text = sprintf("(%s, %s) %s (%s) scored on %s (%s). Assisted by %s. %s-point goal", 
                        q, time, scorer, off, goalie, def, assist, 1+twoPT)) %>% pull(text)

roster <- readr::read_csv('data/rosters/combined/2020 combined roster with UUIDs.csv')

rosterJoin <- roster %>% 
  separate(name, c('first', 'last'), remove=F, extra = 'merge',fill = 'right')

joined <- inner_join(goals,rosterJoin,by=c('off'='team','scorer'='last','numS'='number'))

joined %>% filter(is.na(hand))

joined %>% 
  filter(is.na(goalie)|is.na(scorer)|is.na(q)|is.na(time)|is.na(numS)|is.na(twoPT))

joined %>% 
  mutate(Assister = case_when(is.na(assist)~'Unassisted',assist == 'N/A'~'Unassisted',
                              TRUE ~ sprintf('Assisted by %s-%s', numA, assist))) %>% select(Assister)


#{Team} goal scored by {scorer} ({goals}) on {goalie}. {Assist}. 
#{Leading Team} {Leading Team Score}, {Trailing Team} {Trailing Team Score}
champGame <- joined %>% filter(date == '2020-08-09')

score <- champGame %>% pivot_wider(id_cols = c('q', 'time'), names_from='off', values_from='score') %>% fill(3:4)

teamA <- colnames(score)[3]
teamB <- colnames(score)[4]

inner_join(champGame, score, by=c('q', 'time')) %>% replace_na(list(teamA=0, teamB=0)) %>% select(41:42)

inner_join(champGame, score, by=c('q', 'time')) %>% 
  select(q, time, off, numS, ID, name, position, numA, assist, numG, goalie, 
         twoPT, hand, advantage, fast_break, points, 40:41) %>% 
  mutate(!!as.symbol(teamA) := case_when(is.na(!!as.symbol(teamA))~0,
                                          TRUE~!!as.symbol(teamA)),
         !!as.symbol(teamB) := case_when(is.na(!!as.symbol(teamB))~0,
                                         TRUE~!!as.symbol(teamB)),
         leadScore = case_when(!!as.symbol(teamA)>=!!as.symbol(teamB)~!!as.symbol(teamA),
                               !!as.symbol(teamA)<!!as.symbol(teamB)~!!as.symbol(teamB),
                               TRUE ~ NA_real_),
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
                               TRUE ~ NA_real_),
         assister = case_when(is.na(assist)~'Unassisted',
                              assist == 'N/A'~'Unassisted',
                              TRUE ~ sprintf('Assisted by %s-%s', numA, assist)),
         
         goalString = sprintf('%s goal scored by %s-%s (%s) on %s-%s.
         %s.
         %s-point goal.
         %s %s, %s %s', 
                              off, numS, name, position, numG, goalie, assister, 
                              points, leader, leadScore, trailer, trailScore)) %>% 
  select(-c(off, numS, ID, name, numA, assist, numG, goalie, assister)) %>% 
  select(15) %>% pull(goalString)

score %>% 
  select(!!as.symbol(teamA), !!as.symbol(teamB))
