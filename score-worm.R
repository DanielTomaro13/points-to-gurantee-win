#####################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2) 
library(fitzRoy) 
library(purrr)
#####################################################
seasons <- 2012:2025

results_list <- map(seasons, ~{
  tryCatch({
    fetch_results_afl(season = .x, comp = "AFLM")
  }, error = function(e) {
    message("Failed to fetch season: ", .x)
    NULL
  })
})

results <- bind_rows(compact(results_list))
colnames(results)

score_worm <- results %>% select(
  match.date, round.year, round.roundId, round.roundNumber, match.matchId, 
  match.homeTeam.teamId, match.awayTeam.teamId, 
  match.homeTeam.name, match.awayTeam.name,
  `homeTeamScore.periodScore`, `homeTeamScore.rushedBehinds`, `homeTeamScore.minutesInFront`,
  `homeTeamScore.matchScore.totalScore`, `homeTeamScore.matchScore.goals`, `homeTeamScore.matchScore.behinds`,
  `awayTeamScore.periodScore`, `awayTeamScore.rushedBehinds`,
  `awayTeamScore.minutesInFront`, `awayTeamScore.matchScore.totalScore`, `awayTeamScore.matchScore.goals`,
  `awayTeamScore.matchScore.behinds`,
  `homeTeamScoreChart.goals`, `awayTeamScoreChart.goals`


) 

str(score_worm)

home_periods <- score_worm %>%
  select(match.matchId, match.date, home_team = match.homeTeam.name, home_total = `homeTeamScore.matchScore.totalScore`,
         home_period = `homeTeamScore.periodScore`) %>%
  unnest(home_period) %>%
  arrange(match.matchId, periodNumber) %>%
  group_by(match.matchId) %>%
  mutate(home_cumulative = cumsum(`score.totalScore`)) %>%
  ungroup()

away_periods <- score_worm %>%
  select(match.matchId, away_team = match.awayTeam.name, away_total = `awayTeamScore.matchScore.totalScore`,
         away_period = `awayTeamScore.periodScore`) %>%
  unnest(away_period) %>%
  arrange(match.matchId, periodNumber) %>%
  group_by(match.matchId) %>%
  mutate(away_cumulative = cumsum(`score.totalScore`)) %>%
  ungroup()

match_summary <- home_periods %>%
  select(match.matchId, periodNumber, home_cumulative) %>%
  left_join(
    away_periods %>% select(match.matchId, periodNumber, away_cumulative),
    by = c("match.matchId", "periodNumber")
  ) %>%
  group_by(match.matchId) %>%
  summarise(
    home_3qt = sum(home_cumulative[periodNumber <= 3]),
    away_3qt = sum(away_cumulative[periodNumber <= 3]),
    home_first_80 = ifelse(any(home_cumulative >= 80), min(periodNumber[home_cumulative >= 80]), NA),
    away_first_80 = ifelse(any(away_cumulative >= 80), min(periodNumber[away_cumulative >= 80]), NA),
    home_first_100 = ifelse(any(home_cumulative >= 100), min(periodNumber[home_cumulative >= 100]), NA),
    away_first_100 = ifelse(any(away_cumulative >= 100), min(periodNumber[away_cumulative >= 100]), NA)
  ) %>%
  left_join(
    score_worm %>%
      select(match.matchId, home_total = `homeTeamScore.matchScore.totalScore`, 
             away_total = `awayTeamScore.matchScore.totalScore`) %>%
      mutate(winner = case_when(
        home_total > away_total ~ "home",
        away_total > home_total ~ "away",
        TRUE ~ "draw"
      )),
    by = "match.matchId"
  ) %>%
  mutate(
    lead_3qt = case_when(
      home_3qt > away_3qt ~ "home",
      away_3qt > home_3qt ~ "away",
      TRUE ~ "draw"
    ),
    first_80 = case_when(
      !is.na(home_first_80) & (is.na(away_first_80) | home_first_80 < away_first_80) ~ "home",
      !is.na(away_first_80) & (is.na(home_first_80) | away_first_80 < home_first_80) ~ "away",
      home_first_80 == away_first_80 ~ "tie",
      TRUE ~ NA_character_
    ),
    first_100 = case_when(
      !is.na(home_first_100) & (is.na(away_first_100) | home_first_100 < away_first_100) ~ "home",
      !is.na(away_first_100) & (is.na(home_first_100) | away_first_100 < home_first_100) ~ "away",
      home_first_100 == away_first_100 ~ "tie",
      TRUE ~ NA_character_
    )
  )

# Leading at 3Q time
three_qtr_stats <- match_summary %>%
  filter(lead_3qt %in% c("home", "away")) %>%
  summarise(
    total = n(),
    win = sum(lead_3qt == winner),
    pct = round(mean(lead_3qt == winner) * 100, 1)
  )

# First to 80
first_80_stats <- match_summary %>%
  filter(first_80 %in% c("home", "away")) %>%
  summarise(
    total = n(),
    win = sum(first_80 == winner),
    pct = round(mean(first_80 == winner) * 100, 1)
  )

# First to 100
first_100_stats <- match_summary %>%
  filter(first_100 %in% c("home", "away")) %>%
  summarise(
    total = n(),
    win = sum(first_100 == winner),
    pct = round(mean(first_100 == winner) * 100, 1)
  )

# Combine for easy reading
results_summary <- tibble(
  Metric = c("Leading at 3Q Time", "First to 80 Points", "First to 100 Points"),
  Games = c(three_qtr_stats$total, first_80_stats$total, first_100_stats$total),
  Wins = c(three_qtr_stats$win, first_80_stats$win, first_100_stats$win),
  Win_Percent = c(three_qtr_stats$pct, first_80_stats$pct, first_100_stats$pct)
)


test_3qt <- binom.test(x = 2260, n = 2728, p = 0.5, alternative = "greater")
test_80 <- binom.test(x = 1858, n = 1903, p = 0.5, alternative = "greater")
test_100 <- binom.test(x = 1285, n = 1288, p = 0.5, alternative = "greater")

list(
  Leading_3QT = broom::tidy(test_3qt),
  First_to_80 = broom::tidy(test_80),
  First_to_100 = broom::tidy(test_100)
)

thresholds <- seq(20, 120, by = 10)

threshold_stats <- map_dfr(thresholds, function(thresh) {
  match_summary_thresh <- home_periods %>%
    select(match.matchId, periodNumber, home_cumulative) %>%
    left_join(away_periods %>% select(match.matchId, periodNumber, away_cumulative),
              by = c("match.matchId", "periodNumber")) %>%
    group_by(match.matchId) %>%
    summarise(
      home_first = ifelse(any(home_cumulative >= thresh), min(periodNumber[home_cumulative >= thresh]), NA),
      away_first = ifelse(any(away_cumulative >= thresh), min(periodNumber[away_cumulative >= thresh]), NA)
    ) %>%
    left_join(score_worm %>%
                select(match.matchId, home_total = `homeTeamScore.matchScore.totalScore`, 
                       away_total = `awayTeamScore.matchScore.totalScore`) %>%
                mutate(winner = case_when(
                  home_total > away_total ~ "home",
                  away_total > home_total ~ "away",
                  TRUE ~ "draw"
                )),
              by = "match.matchId") %>%
    mutate(
      first_team = case_when(
        !is.na(home_first) & (is.na(away_first) | home_first < away_first) ~ "home",
        !is.na(away_first) & (is.na(home_first) | away_first < home_first) ~ "away",
        home_first == away_first ~ "tie",
        TRUE ~ NA_character_
      ),
      win = first_team == winner
    ) %>%
    filter(first_team %in% c("home", "away")) %>%
    summarise(
      Threshold = thresh,
      Games = n(),
      Wins = sum(win, na.rm = TRUE),
      Win_Percent = mean(win, na.rm = TRUE) * 100,
      conf.low = qbinom(0.025, n(), mean(win, na.rm = TRUE)) / n() * 100,
      conf.high = qbinom(0.975, n(), mean(win, na.rm = TRUE)) / n() * 100
    )
  
  return(match_summary_thresh)
})

ggplot(threshold_stats, aes(x = Threshold, y = Win_Percent)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(title = "Win % by First to Score Threshold (with 95% CI)",
       x = "First to Score Threshold",
       y = "Win Percentage") +
  theme_minimal()

#####################################################
# Bookie pay out analysis
quarter_scores <- home_periods %>%
  select(match.matchId, periodNumber, home_cumulative) %>%
  left_join(
    away_periods %>% select(match.matchId, periodNumber, away_cumulative),
    by = c("match.matchId", "periodNumber")
  ) %>%
  filter(periodNumber %in% 1:3) %>%
  pivot_wider(
    names_from = periodNumber,
    values_from = c(home_cumulative, away_cumulative),
    names_prefix = "Q"
  )

quarter_margins <- quarter_scores %>%
  left_join(
    score_worm %>%
      select(match.matchId, home_total = `homeTeamScore.matchScore.totalScore`,
             away_total = `awayTeamScore.matchScore.totalScore`) %>%
      mutate(winner = case_when(
        home_total > away_total ~ "home",
        away_total > home_total ~ "away",
        TRUE ~ "draw"
      )),
    by = "match.matchId"
  ) %>%
  mutate(
    # Margins at each break
    Q1_margin = home_cumulative_Q1 - away_cumulative_Q1,
    Q2_margin = home_cumulative_Q2 - away_cumulative_Q2,
    Q3_margin = home_cumulative_Q3 - away_cumulative_Q3,
    
    # Who was leading by 12+ at each break
    Q1_lead12_team = case_when(Q1_margin >= 12 ~ "home", Q1_margin <= -12 ~ "away", TRUE ~ NA_character_),
    Q2_lead12_team = case_when(Q2_margin >= 12 ~ "home", Q2_margin <= -12 ~ "away", TRUE ~ NA_character_),
    Q3_lead12_team = case_when(Q3_margin >= 12 ~ "home", Q3_margin <= -12 ~ "away", TRUE ~ NA_character_),
    
    # Did the leading team win?
    Q1_lead12_result = Q1_lead12_team == winner,
    Q2_lead12_result = Q2_lead12_team == winner,
    Q3_lead12_result = Q3_lead12_team == winner
  )

summary_lead12 <- function(lead_col, result_col) {
  quarter_margins %>%
    filter(!is.na(.data[[lead_col]])) %>%
    summarise(
      Total_Leads = n(),
      Wins = sum(.data[[result_col]], na.rm = TRUE),
      Losses = Total_Leads - Wins,
      Win_Percent = round(100 * Wins / Total_Leads, 1)
    )
}

results_lead12 <- bind_rows(
  Q1 = summary_lead12("Q1_lead12_team", "Q1_lead12_result"),
  Q2 = summary_lead12("Q2_lead12_team", "Q2_lead12_result"),
  Q3 = summary_lead12("Q3_lead12_team", "Q3_lead12_result"),
  .id = "Quarter"
)

results_lead12
