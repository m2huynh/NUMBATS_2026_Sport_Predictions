# Load libraries
library(tidyverse)
library(fitzRoy)
library(elo)
library(gtools)

# Fetch data
afl2024 <- fitzRoy::fetch_results_afltables(season = 2024)

# Create training set (regular rounds only)
afl2024_rr <- afl2024 |> 
  filter(Round.Type == "Regular") |> 
  mutate(
    Score = case_when(
      Margin > 0  ~ 1,
      Margin == 0 ~ 0.5,
      Margin < 0  ~ 0
    )
  ) |> 
  select(Round, Home.Team, Away.Team, Score)

# Fit an Elo model
elo_fit <- elo::elo.run(
  Score ~ adjust(Home.Team, 50) + Away.Team,
  data = afl2024_rr,
  initial.elos = 1500,
  k = 37,
  history = TRUE
)

# Extract the final ratings
final_ratings <- final.elos(elo_fit) |> 
  enframe(name = "team", value = "elo")  |> 
  arrange(desc(elo))

# Keep the top 8 teams (interesting about Carlton)
top_8 <- final_ratings |> 
  filter(team %in% c("Sydney", "Brisbane Lions", "Footscray", "Port Adelaide", 
                 "GWS", "Carlton", "Hawthorn", "Geelong"))
top_8
# Build top-8 matchup grid
grid_elo <- gtools::permutations(
  n = 8,
  r = 2,
  v = top_8$team,,
  repeats.allowed = FALSE
) |> 
  as_tibble() |>
  rename(team = V1, opp = V2) |> 
  
  # Attach team ELO
  left_join(final_ratings, by = c("team" = "team")) |> 
  rename(team_elo = elo) |>
  
  # Attach opp ELO
  left_join(final_ratings, by = c("opp" = "team")) |> 
  rename(opp_elo = elo)

# Calculate win probabilities
grid_elo <- 
  grid_elo |> 
  mutate(
    
    # teamProb
    teamProb = elo::elo.prob(team_elo, opp_elo),
    
    # oppProb
    oppProb = 1 - teamProb)


simulate_afl_finals <- function(grid_prob, n_sims = 1000) {
  
  results <- list()
  
  for (i in 1:n_sims) {
    
    # --- Week 1 ---
    week1 <- data.frame(
      finalID = c("QF1", "QF2", "EF1", "EF2"),
      team    = c("Sydney", "Port Adelaide", "GWS", "Footscray"),
      opp     = c("Brisbane Lions", "Geelong", "Carlton", "Hawthorn")
    ) |>
      left_join(grid_prob, by = c("team", "opp"))
    
    QF1 <- week1[1, ]; QF2 <- week1[2, ]; EF1 <- week1[3, ]; EF2 <- week1[4, ]
    
    QF1_res <- sample(c("team", "opp"), 1, prob = c(QF1$teamProb, QF1$oppProb))
    QF2_res <- sample(c("team", "opp"), 1, prob = c(QF2$teamProb, QF2$oppProb))
    EF1_res <- sample(c("team", "opp"), 1, prob = c(EF1$teamProb, EF1$oppProb))
    EF2_res <- sample(c("team", "opp"), 1, prob = c(EF2$teamProb, EF2$oppProb))
    
    QF1 <- QF1 |>
      mutate(
        winner = ifelse(QF1_res == "team", team, opp),
        loser  = ifelse(QF1_res == "team", opp, team)
      )
    QF2 <- QF2 |>
      mutate(
        winner = ifelse(QF2_res == "team", team, opp),
        loser  = ifelse(QF2_res == "team", opp, team)
      )
    EF1 <- EF1 |>
      mutate(
        winner = ifelse(EF1_res == "team", team, opp),
        loser  = ifelse(EF1_res == "team", opp, team)
      )
    EF2 <- EF2 |>
      mutate(
        winner = ifelse(EF2_res == "team", team, opp),
        loser  = ifelse(EF2_res == "team", opp, team)
      )
    
    week1_output <- bind_rows(QF1, QF2, EF1, EF2)
    
    Q1W <- week1_output$winner[1]; Q1L <- week1_output$loser[1]
    Q2W <- week1_output$winner[2]; Q2L <- week1_output$loser[2]
    E1W <- week1_output$winner[3]; E1L <- week1_output$loser[3]
    E2W <- week1_output$winner[4]; E2L <- week1_output$loser[4]
    
    # --- Week 2 ---
    week2 <- data.frame(
      finalID = c("SF1", "SF2"),
      team    = c(Q1L, Q2L),
      opp     = c(E1W, E2W)
    ) |>
      left_join(grid_prob, by = c("team", "opp"))
    
    SF1 <- week2[1, ]; SF2 <- week2[2, ]
    
    SF1_res <- sample(c("team", "opp"), 1, prob = c(SF1$teamProb, SF1$oppProb))
    SF2_res <- sample(c("team", "opp"), 1, prob = c(SF2$teamProb, SF2$oppProb))
    
    SF1 <- SF1 |>
      mutate(
        winner = ifelse(SF1_res == "team", team, opp),
        loser  = ifelse(SF1_res == "team", opp, team)
      )
    SF2 <- SF2 |>
      mutate(
        winner = ifelse(SF2_res == "team", team, opp),
        loser  = ifelse(SF2_res == "team", opp, team)
      )
    
    week2_output <- bind_rows(SF1, SF2)
    
    SF1W <- week2_output$winner[1]; SF1L <- week2_output$loser[1]
    SF2W <- week2_output$winner[2]; SF2L <- week2_output$loser[2]
    
    # --- Week 3 ---
    week3 <- data.frame(
      finalID = c("PF1", "PF2"),
      team    = c(Q1W, Q2W),
      opp     = c(SF2W, SF1W)
    ) |>
      left_join(grid_prob, by = c("team", "opp"))
    
    PF1 <- week3[1, ]; PF2 <- week3[2, ]
    
    PF1_res <- sample(c("team", "opp"), 1, prob = c(PF1$teamProb, PF1$oppProb))
    PF2_res <- sample(c("team", "opp"), 1, prob = c(PF2$teamProb, PF2$oppProb))
    
    PF1 <- PF1 |>
      mutate(
        winner = ifelse(PF1_res == "team", team, opp),
        loser  = ifelse(PF1_res == "team", opp, team)
      )
    PF2 <- PF2 |>
      mutate(
        winner = ifelse(PF2_res == "team", team, opp),
        loser  = ifelse(PF2_res == "team", opp, team)
      )
    
    week3_output <- bind_rows(PF1, PF2)
    
    PF1W <- week3_output$winner[1]; PF1L <- week3_output$loser[1]
    PF2W <- week3_output$winner[2]; PF2L <- week3_output$loser[2]
    
    # --- Week 4 (Grand Final) ---
    week4 <- data.frame(
      finalID = "GF",
      team    = PF1W,
      opp     = PF2W
    ) |>
      left_join(grid_prob, by = c("team", "opp"))
    
    GF <- week4[1, ]
    GF_res <- sample(c("team", "opp"), 1, prob = c(GF$teamProb, GF$oppProb))
    
    GF <- GF |>
      mutate(
        winner = ifelse(GF_res == "team", team, opp),
        loser  = ifelse(GF_res == "team", opp, team)
      )
    
    standings <- data.frame(
      simulation_id = i,
      Result = c(
        "Eliminated EF", "Eliminated EF",
        "Eliminated SF", "Eliminated SF",
        "Eliminated PF", "Eliminated PF",
        "RunnerUp", "Winner"
      ),
      Team = c(E1L, E2L, SF1L, SF2L, PF1L, PF2L, GF$loser, GF$winner)
    )
    
    results[[i]] <- standings
  }
  
  bind_rows(results)
}

# Run simulations
finals_simulation_results <- 
  simulate_afl_finals(grid_prob = grid_elo, n_sims = 1000)

# Print results
finals_simulation_results %>%
  group_by(Team, Result) %>%
  summarise(n = n()) %>%
  tidyr::pivot_wider(names_from = Result, values_from = n, values_fill = 0) %>%
  mutate(TotalSims = rowSums(across(where(is.numeric))),
         WinPct = Winner / TotalSims * 100,
         RunnerUpPct = RunnerUp / TotalSims * 100) |>
  select(1, 6, 2:5) |>
  arrange(Winner)

