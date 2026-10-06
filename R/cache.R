# Local cache of MFL / league-sheet pulls, so finished weeks and past seasons are fetched once.
#   data/cache/<season>-week-NN.rds   one week's rostered players + scores (get_week() output), final weeks only
#   data/cache/history-<year>.rds     a past season: list(weekly = team scores, players = player weeks, names = player map)
# Past seasons are final: read forever, refetch only with force = TRUE. In the current season a week is
# written once it is final; the week being reported is always refetched so MNF updates land.

CACHE_DIR <- "data/cache"
cache_file <- function(...) file.path(CACHE_DIR, paste0(...))

get_week_cached <- function(conn, season, wk, force = FALSE) {
  f <- cache_file(season, sprintf("-week-%02d.rds", wk))
  if (!force && file.exists(f)) return(readRDS(f))
  d <- get_week(conn, wk)
  if (nrow(d) && all(d$yet_to_play == 0)) { dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE); saveRDS(d, f) }
  d
}

# One past season. Team scores come from the league sheet (<year>Standings tab, as history.R always did);
# player weeks come from MFL with a pause between calls to stay under its rate limit.
# Returns NULL players if MFL refuses; rerun later and only the missing part is fetched.
history_season <- function(year, force = FALSE, players = TRUE, pause = 4) {
  f <- cache_file("history-", year, ".rds")
  h <- if (!force && file.exists(f)) readRDS(f) else list()
  changed <- FALSE
  if (is.null(h$weekly)) {
    if (!exists("weekly_from_sheet")) source("R/history.R")
    ids <- franchise_ids_by_year(year)
    h$weekly <- weekly_from_sheet(year) %>% left_join(ids, by = c("year", "franchise_name")) %>%
      left_join(OWNER_MAP, by = c("franchise_id", "year"))
    changed <- TRUE
  }
  if (players && is.null(h$players)) {
    conn <- mfl_connect(season = year, league_id = LEAGUE_ID, rate_limit_number = 1, rate_limit_seconds = 3)
    wks <- sort(unique(h$weekly$week))
    got <- lapply(wks, function(wk) { Sys.sleep(pause)
      tryCatch(get_week(conn, wk), error = function(e) { message(year, " W", wk, ": ", conditionMessage(e)); NULL }) })
    if (all(!vapply(got, is.null, TRUE))) {
      h$players <- bind_rows(got)
      h$names   <- mfl_players(conn) %>% transmute(player_id = as.character(player_id), player_name, pos, nfl = norm_team(team))
      changed <- TRUE
    }
  }
  if (changed) { dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE); saveRDS(h, f) }
  h
}

# Weekly team scores for all cached past seasons, in history.R's shape (franchise_id, score, week, year,
# rank, n_teams, f1, owner)
history_weekly <- function(years = 2022:2025, players = FALSE) {
  bind_rows(lapply(years, function(y) history_season(y, players = players)$weekly)) %>%
    filter(!is.na(franchise_id)) %>%
    group_by(year, week) %>% mutate(rank = rank(-score, ties.method = "min"), n_teams = n()) %>% ungroup() %>%
    mutate(f1 = c(15, 10, 7, 4, 2, 1, rep(0, 6))[pmin(rank, 12)])
}
