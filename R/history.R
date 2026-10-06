# F1C all-time history (2022-present). "Best Ball Contracts" (2021 and earlier)
# was a different league under the same MFL ID and is excluded.
#
# Owner-of-record = whoever started that season, confirmed with Andrew. Franchise
# slots 11/12 went dark for the 2024 (10-team) season; the owners who took those
# slots back in 2025 are treated as fresh entrants with no linked pre-2024 history.
#
# NOTE on 2024: Andrew's recollection (confirmed against that season's published
# final report) is that the weekly F1-points scale that year gave extra points at
# the top vs. the standard 15/10/7/4/2/1/0 table used every other season. We don't
# have the exact 2024 table, so `f1` below uses the STANDARD scale for every
# season including 2024 -- fine for rank-based stats (wins/podiums/top6/finish),
# but season-total F1 points/earnings for 2024 should be treated as approximate.

suppressPackageStartupMessages({
  library(ffscrapr); library(googlesheets4); library(dplyr); library(tidyr); library(purrr); library(stringr)
})
gs4_deauth()

LEAGUE_ID <- 43580
LEAGUE_SS <- "1-hbPnZU2dLalXbCRjsDe79vX1eJr6gzon4qnrWYuLro"
F1_WEEK   <- c(15, 10, 7, 4, 2, 1, rep(0, 6))   # standard weekly rank -> F1 pts (ranks 7+ score 0)

# ---- owner-of-record map: franchise_id + season -> owner ----------------------
OWNER_MAP <- tribble(
  ~franchise_id, ~year, ~owner,
  "0001", 2022, "Stukas",   "0001", 2023, "Stukas",   "0001", 2024, "Stukas",   "0001", 2025, "Stukas",   "0001", 2026, "Stukas",
  "0002", 2022, "Doucette", "0002", 2023, "Doucette", "0002", 2024, "KashQ",    "0002", 2025, "KashQ",    "0002", 2026, "KashQ",
  "0003", 2022, "Fagan",    "0003", 2023, "Fagan",    "0003", 2024, "AZ",       "0003", 2025, "AZ",       "0003", 2026, "Dom",
  "0004", 2022, "Keenan",   "0004", 2023, "Chafetz",  "0004", 2024, "Chafetz",  "0004", 2025, "Chafetz",  "0004", 2026, "Chafetz",
  "0005", 2022, "Cole",     "0005", 2023, "Cole",     "0005", 2024, "Cole",     "0005", 2025, "Cole",     "0005", 2026, "Cole",
  "0006", 2022, "Bagge",    "0006", 2023, "Bagge",    "0006", 2024, "Bagge",    "0006", 2025, "Bagge",    "0006", 2026, "Bagge",
  "0007", 2022, "Wisnewski","0007", 2023, "Wisnewski","0007", 2024, "Wisnewski","0007", 2025, "Wisnewski","0007", 2026, "Wisnewski",
  "0008", 2022, "Jim",      "0008", 2023, "Jim",      "0008", 2024, "Orlando",  "0008", 2025, "Orlando",  "0008", 2026, "Orlando",
  "0009", 2022, "Murray",   "0009", 2023, "Murray",   "0009", 2024, "Murray",   "0009", 2025, "Murray",   "0009", 2026, "Murray",
  "0010", 2022, "Labelle",  "0010", 2023, "Labelle",  "0010", 2024, "Labelle",  "0010", 2025, "Labelle",  "0010", 2026, "Labelle",
  "0011", 2022, "Dubuc",    "0011", 2023, "Dubuc",                                        "0011", 2025, "Miller",   "0011", 2026, "Miller",
  "0012", 2022, "AZ",       "0012", 2023, "AZ",                                           "0012", 2025, "Dodge",    "0012", 2026, "Dodge",
) %>% mutate(franchise_id = as.character(franchise_id))

# ---- weekly team scores per season --------------------------------------------
weekly_from_sheet <- function(year) {
  tab <- sprintf("%dStandings", year)
  d <- read_sheet(LEAGUE_SS, sheet = tab)
  names(d)[1] <- "franchise_name"
  d <- d %>% filter(franchise_name != "Average")
  wk_cols <- intersect(as.character(1:18), names(d))
  d %>% select(franchise_name, all_of(wk_cols)) %>%
    pivot_longer(-franchise_name, names_to = "week", values_to = "score") %>%
    mutate(week = as.integer(week), year = year, score = as.numeric(score)) %>%
    filter(!is.na(score), score > 0)
}

weekly_from_mfl <- function(year, max_week = 18) {
  conn <- mfl_connect(season = year, league_id = LEAGUE_ID)
  fr <- ff_franchises(conn) %>% select(franchise_id, franchise_name)
  get1 <- function(x, k) { v <- x[[k]]; if (is.null(v) || !length(v)) NA_character_ else v }
  map_dfr(1:max_week, function(w) {
    res <- tryCatch(mfl_getendpoint(conn, "weeklyResults", W = w)$content$weeklyResults$franchise,
                     error = function(e) NULL)
    if (is.null(res) || !length(res)) return(tibble())
    tibble(franchise_id = map_chr(res, get1, "id"),
           score = suppressWarnings(as.numeric(map_chr(res, get1, "score"))), week = w)
  }) %>% filter(!is.na(score), score > 0) %>% left_join(fr, by = "franchise_id") %>% mutate(year = year)
}

franchise_ids_by_year <- function(year) {
  conn <- mfl_connect(season = year, league_id = LEAGUE_ID)
  ff_franchises(conn) %>% select(franchise_id, franchise_name) %>% mutate(year = year)
}

build_history <- function(years_sheet = 2022:2025, year_live = 2026, max_week_live = 18) {
  ids <- map_dfr(c(years_sheet, year_live), franchise_ids_by_year)

  sheet_weekly <- map_dfr(years_sheet, weekly_from_sheet) %>%
    left_join(ids, by = c("year", "franchise_name"))

  live_weekly <- weekly_from_mfl(year_live, max_week_live) %>% select(franchise_id, score, week, year)

  weekly <- bind_rows(sheet_weekly %>% select(franchise_id, score, week, year), live_weekly) %>%
    filter(!is.na(franchise_id)) %>%
    group_by(year, week) %>% mutate(rank = rank(-score, ties.method = "min"), n_teams = n()) %>% ungroup() %>%
    mutate(f1 = F1_WEEK[pmin(rank, 12)]) %>%
    left_join(OWNER_MAP, by = c("franchise_id", "year"))

  weekly
}

# ---- season-level standings: one row per owner per year -----------------------
season_standings <- function(weekly) {
  weekly %>%
    group_by(year, owner) %>%
    summarise(weeks = n(), points_for = sum(score), avg_score = mean(score),
              wins = sum(rank == 1), podiums = sum(rank <= 3), top6 = sum(rank <= 6),
              avg_rank = mean(rank), f1_pts = sum(f1),
              best_week = max(score), worst_week = min(score), .groups = "drop") %>%
    group_by(year) %>% mutate(season_finish = rank(-f1_pts, ties.method = "min")) %>% ungroup() %>%
    arrange(year, season_finish)
}

# ---- career (all-time) standings: one row per owner, summed across their seasons
# `current_year` is excluded from championship/finish counts since it's still in progress.
career_standings <- function(season_st, current_year = NULL) {
  complete <- if (is.null(current_year)) season_st else filter(season_st, year != current_year)
  finishes <- complete %>% group_by(owner) %>%
    summarise(championships = sum(season_finish == 1), runner_up = sum(season_finish == 2),
              avg_finish = mean(season_finish), best_finish = min(season_finish),
              worst_finish = max(season_finish), .groups = "drop")

  season_st %>%
    group_by(owner) %>%
    summarise(seasons = n_distinct(year), weeks = sum(weeks), points_for = sum(points_for),
              wins = sum(wins), podiums = sum(podiums), top6 = sum(top6),
              f1_pts = sum(f1_pts), .groups = "drop") %>%
    left_join(finishes, by = "owner") %>%
    arrange(desc(f1_pts))
}

# ---- record book ---------------------------------------------------------------
record_book <- function(weekly) {
  by_week <- weekly %>% group_by(year, week) %>%
    summarise(top = max(score), bottom = min(score), gap = top - bottom,
              second = sort(score, decreasing = TRUE)[2], .groups = "drop") %>%
    mutate(margin = top - second)

  best_week    <- weekly %>% arrange(desc(score)) %>% slice(1)
  worst_week   <- weekly %>% arrange(score) %>% slice(1)
  biggest_blow <- by_week %>% arrange(desc(gap)) %>% slice(1)
  closest_race <- by_week %>% arrange(margin) %>% slice(1)

  streaks <- weekly %>% arrange(owner, year, week) %>% group_by(owner, year) %>%
    mutate(in_top3 = rank <= 3,
           grp = cumsum(c(1, diff(in_top3) != 0))) %>%
    filter(in_top3) %>% count(owner, year, grp, name = "len") %>%
    group_by(owner) %>% summarise(longest_top3_streak = max(len), .groups = "drop") %>%
    arrange(desc(longest_top3_streak))

  list(best_week = best_week, worst_week = worst_week, biggest_blowout = biggest_blow,
       closest_race = closest_race, streaks = streaks)
}
