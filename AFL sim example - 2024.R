library(tidyverse)
library(fitzRoy)
library(gtools)
library(tidymodels)
library(ranger)
### a
afl2024 <- fitzRoy::fetch_player_stats_afltables(season = 2024)
str(afl2024)

afl2024r <- afl2024 %>% 
  filter(Round %in% c(1:23)) %>% 
  select(Round, 
         Date, 
         Home.team, 
         Away.team, 
         Home.score, 
         Away.score, 
         Playing.for, 
         Kicks,
         Marks,
         Inside.50s,
         Tackles,
         Frees.For)

df_2024_Home <- afl2024r %>% 
  group_by(Round, Playing.for) %>% 
  mutate(Home.Kicks = ifelse(Home.team == Playing.for, sum(Kicks), NA),
         Home.Marks = ifelse(Home.team == Playing.for, sum(Marks), NA),
         Home.Inside.50s = ifelse(Home.team == Playing.for, sum(Inside.50s), NA),
         Home.Tackles = ifelse(Home.team == Playing.for, sum(Tackles), NA),
         Home.Frees.For = ifelse(Home.team == Playing.for, sum(Frees.For), NA)) %>% 
  filter(row_number() == 1) %>% 
  filter(!is.na(Home.Kicks)) %>% 
  ungroup() %>% 
  select(Round, Home.team, Home.score, Home.Kicks, Home.Marks,Home.Inside.50s, Home.Tackles, Home.Frees.For)

df_2024_Away <- afl2024r %>% 
  group_by(Round, Playing.for) %>% 
  mutate(Away.Kicks = ifelse(Away.team == Playing.for, sum(Kicks), NA),
         Away.Marks = ifelse(Away.team == Playing.for, sum(Marks), NA),
         Away.Inside.50s = ifelse(Away.team == Playing.for, sum(Inside.50s), NA),
         Away.Tackles = ifelse(Away.team == Playing.for, sum(Tackles), NA),
         Away.Frees.For = ifelse(Away.team == Playing.for, sum(Frees.For), NA)) %>% 
  filter(row_number() == 1) %>% 
  filter(!is.na(Away.Kicks)) %>% 
  ungroup() %>% 
  select(Round, Away.team, Away.score, Away.Kicks, Away.Marks,Away.Inside.50s, Away.Tackles, Away.Frees.For)

df_2024_final <- 
  df_2024_Home %>% 
  cbind(df_2024_Away) %>% 
  select(c(1,2,10,3,11,4:8,12:16)) %>% 
  mutate(Score = factor(ifelse(Home.score > Away.score, 1, 0)),
         Margin = Home.score - Away.score)

Home <- 
  df_2024_final %>% 
  group_by(Home.team) %>% 
  summarise(Home.Kicks = mean(Home.Kicks),
            Home.Marks = mean(Home.Marks),
            Home.Inside.50s = mean(Home.Inside.50s),
            Home.Tackles = mean(Home.Tackles),
            Home.Frees.For = mean(Home.Frees.For)) %>% 
  rename(team = Home.team)%>% 
  filter(team %in% c("Sydney",
                     "Greater Western Sydney",
                     "Brisbane Lions",
                     "Carlton",
                     "Western Bulldogs",
                     "Hawthorn",
                     "Port Adelaide",
                     "Geelong")) %>% 
  mutate(team = case_when(
    team == 'Sydney' ~ 'SYD',
    team == 'Greater Western Sydney' ~ 'GWS',
    team == 'Brisbane Lions' ~ 'BRI',
    team == 'Carlton' ~ 'CAR',
    team == 'Western Bulldogs' ~ 'WES',
    team == 'Hawthorn' ~ 'HAW',
    team == 'Port Adelaide' ~ 'POR',
    team == 'Geelong' ~ 'GEE'
  ))

Away <- 
  df_2024_final %>% 
  group_by(Away.team) %>% 
  summarise(Away.Kicks = mean(Away.Kicks),
            Away.Marks = mean(Away.Marks),
            Away.Inside.50s = mean(Away.Inside.50s),
            Away.Tackles = mean(Away.Tackles),
            Away.Frees.For = mean(Away.Frees.For)) %>% 
  rename(team = Away.team)%>% 
  filter(team %in% c("Sydney",
                     "Greater Western Sydney",
                     "Brisbane Lions",
                     "Carlton",
                     "Western Bulldogs",
                     "Hawthorn",
                     "Port Adelaide",
                     "Geelong")) %>% 
  mutate(team = case_when(
    team == 'Sydney' ~ 'SYD',
    team == 'Greater Western Sydney' ~ 'GWS',
    team == 'Brisbane Lions' ~ 'BRI',
    team == 'Carlton' ~ 'CAR',
    team == 'Western Bulldogs' ~ 'WES',
    team == 'Hawthorn' ~ 'HAW',
    team == 'Port Adelaide' ~ 'POR',
    team == 'Geelong' ~ 'GEE'
  ))%>% 
  rename(opp = team)

# Make grid
grid <- 
  gtools::permutations(n = 8, r = 2, c('SYD',
                               'GWS',
                               'BRI',
                               'CAR',
                               'WES',
                               'HAW',
                               'POR',
                               'GEE'), 
               repeats.allowed = FALSE) %>% 
  as.data.frame()
grid

grid <- 
  grid %>% 
  rename(team = V1,
         opp = V2) %>% 
  left_join(Home) %>% 
  left_join(Away) %>% 
  mutate_if(is.numeric, round, 0)

# create a random forest spec
rf_spec <- rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

rf_fit <- 
  rf_spec %>% 
  fit(Score ~ Home.Kicks + Away.Kicks +
        Home.Marks + Away.Marks +
        Home.Inside.50s + Away.Inside.50s +
        Home.Frees.For + Away.Frees.For, data = df_2024_final)

grid <- 
  augment(rf_fit, new_data = grid) %>% 
  select(team, opp, .pred_1, .pred_0) %>% 
  rename(teamProb = 3,
         oppProb = 4)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
simulate_afl_finals <- function(grid_prob, n_sims = 1000) {
  
  results <- list()
  
  for (i in 1:n_sims) {
    
    # --- Week 1 ---
    week1 <- data.frame(
      finalID = c('QF1', 'QF2', 'EF1', 'EF2'),
      team = c('SYD', 'POR', 'WES', 'BRI'),
      opp = c('GWS', 'GEE', 'HAW', 'CAR')
    ) |> 
      left_join(grid_prob, by = c("team", "opp"))
    
    QF1 <- week1[1, ]; QF2 <- week1[2, ]; EF1 <- week1[3, ]; EF2 <- week1[4, ]
    
    QF1_res <- sample(c('team', 'opp'), 1, prob = c(QF1$teamProb, QF1$oppProb))
    QF2_res <- sample(c('team', 'opp'), 1, prob = c(QF2$teamProb, QF2$oppProb))
    EF1_res <- sample(c('team', 'opp'), 1, prob = c(EF1$teamProb, EF1$oppProb))
    EF2_res <- sample(c('team', 'opp'), 1, prob = c(EF2$teamProb, EF2$oppProb))
    
    QF1 <- QF1 |> mutate(winner = ifelse(QF1_res == 'team', team, opp),
                         loser = ifelse(QF1_res == 'team', opp, team))
    QF2 <- QF2 |> mutate(winner = ifelse(QF2_res == 'team', team, opp),
                         loser = ifelse(QF2_res == 'team', opp, team))
    EF1 <- EF1 |> mutate(winner = ifelse(EF1_res == 'team', team, opp),
                         loser = ifelse(EF1_res == 'team', opp, team))
    EF2 <- EF2 |> mutate(winner = ifelse(EF2_res == 'team', team, opp),
                         loser = ifelse(EF2_res == 'team', opp, team))
    
    week1_output <- bind_rows(QF1, QF2, EF1, EF2)
    
    Q1W <- week1_output$winner[1]; Q1L <- week1_output$loser[1]
    Q2W <- week1_output$winner[2]; Q2L <- week1_output$loser[2]
    E1W <- week1_output$winner[3]; E1L <- week1_output$loser[3]
    E2W <- week1_output$winner[4]; E2L <- week1_output$loser[4]
    
    # --- Week 2 ---
    week2 <- data.frame(
      finalID = c('SF1', 'SF2'),
      team = c(Q1L, Q2L),
      opp = c(E1W, E2W)
    ) |> 
      left_join(grid_prob, by = c("team", "opp"))
    
    SF1 <- week2[1, ]; SF2 <- week2[2, ]
    
    SF1_res <- sample(c('team', 'opp'), 1, prob = c(SF1$teamProb, SF1$oppProb))
    SF2_res <- sample(c('team', 'opp'), 1, prob = c(SF2$teamProb, SF2$oppProb))
    
    SF1 <- SF1 |> mutate(winner = ifelse(SF1_res == 'team', team, opp),
                         loser = ifelse(SF1_res == 'team', opp, team))
    SF2 <- SF2 |> mutate(winner = ifelse(SF2_res == 'team', team, opp),
                         loser = ifelse(SF2_res == 'team', opp, team))
    
    week2_output <- bind_rows(SF1, SF2)
    
    SF1W <- week2_output$winner[1]; SF1L <- week2_output$loser[1]
    SF2W <- week2_output$winner[2]; SF2L <- week2_output$loser[2]
    
    # --- Week 3 ---
    week3 <- data.frame(
      finalID = c('PF1', 'PF2'),
      team = c(Q1W, Q2W),
      opp = c(SF2W, SF1W)
    ) |> 
      left_join(grid_prob, by = c("team", "opp"))
    
    PF1 <- week3[1, ]; PF2 <- week3[2, ]
    
    PF1_res <- sample(c('team', 'opp'), 1, prob = c(PF1$teamProb, PF1$oppProb))
    PF2_res <- sample(c('team', 'opp'), 1, prob = c(PF2$teamProb, PF2$oppProb))
    
    PF1 <- PF1 |> mutate(winner = ifelse(PF1_res == 'team', team, opp),
                         loser = ifelse(PF1_res == 'team', opp, team))
    PF2 <- PF2 |> mutate(winner = ifelse(PF2_res == 'team', team, opp),
                         loser = ifelse(PF2_res == 'team', opp, team))
    
    week3_output <- bind_rows(PF1, PF2)
    
    PF1W <- week3_output$winner[1]; PF1L <- week3_output$loser[1]
    PF2W <- week3_output$winner[2]; PF2L <- week3_output$loser[2]
    
    # --- Week 4 (Grand Final) ---
    week4 <- data.frame(
      finalID = c('GF'),
      team = c(PF1W),
      opp = c(PF2W)
    ) |> 
      left_join(grid_prob, by = c("team", "opp"))
    
    GF <- week4[1, ]
    GF_res <- sample(c('team', 'opp'), 1, prob = c(GF$teamProb, GF$oppProb))
    
    GF <- GF |> mutate(winner = ifelse(GF_res == 'team', team, opp),
                       loser = ifelse(GF_res == 'team', opp, team))
    
    week4_output <- GF
    
    GFW <- week4_output$winner
    GFL <- week4_output$loser
    
    # --- Standings ---
    standings <- data.frame(
      simulation_id = i,
      Result = c('Eliminated EF', 'Eliminated EF', 'Eliminated SF', 'Eliminated SF', 
                 'Eliminated PF', 'Eliminated PF', 'RunnerUp', 'Winner'),
      Team = c(E1L, E2L, SF1L, SF2L, PF1L, PF2L, GFL, GFW)
    )
    
    results[[i]] <- standings
  }
  
  # Combine all simulation results
  all_results <- bind_rows(results)
  
  return(all_results)
}

finals_simulation_results <- 
  simulate_afl_finals(grid_prob = grid, n_sims = 1000)

# Summarise:
a3 <- finals_simulation_results %>%
  group_by(Team, Result) %>%
  summarise(n = n()) %>%
  tidyr::pivot_wider(names_from = Result, values_from = n, values_fill = 0) %>%
  mutate(TotalSims = rowSums(across(where(is.numeric))),
         WinPct = Winner / TotalSims * 100,
         RunnerUpPct = RunnerUp / TotalSims * 100) |>
  select(1:6) |>
  select(1, 6, 3, 2, 4, 5) |>
  arrange(Winner)
# a2 <- finals_simulation_results %>%
#   group_by(Team, Result) %>%
#   summarise(n = n()) %>%
#   tidyr::pivot_wider(names_from = Result, values_from = n, values_fill = 0) %>%
#   mutate(TotalSims = rowSums(across(where(is.numeric))),
#          WinPct = Winner / TotalSims * 100,
#          RunnerUpPct = RunnerUp / TotalSims * 100) |>
#   select(1:6) |>
#   select(1, 6, 3, 2, 4, 5) |>
#   arrange(Winner)
a3



