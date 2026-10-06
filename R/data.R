# F1C weekly review — data layer. Everything comes from MFL + the league sheet; no manual score entry.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(ffscrapr); library(googlesheets4)
})

SEASON    <- 2026
LEAGUE_ID <- 43580
LEAGUE_SS <- "1-hbPnZU2dLalXbCRjsDe79vX1eJr6gzon4qnrWYuLro"
MY_TEAM   <- "0001"

gs4_deauth()
source("R/cache.R")   # finished weeks and past seasons are read from data/cache

# Owner names drift between sheets; everything is keyed to the Team tab's Name
OWNER_ALIAS <- c(Zibuda = "Dom", Snooze = "Wisnewski", Kash = "KashQ", YMC = "Chafetz")

fix_owner <- function(x) {
  x <- str_trim(x)
  if_else(x %in% names(OWNER_ALIAS), unname(OWNER_ALIAS[x]), x)
}

# MFL uses its own NFL team codes
norm_team <- function(t) {
  recode(toupper(t), SFO = "SF", LAR = "LA", NEP = "NE", GBP = "GB", TBB = "TB", LVR = "LV",
         KCC = "KC", NOS = "NO", JAC = "JAX", .default = toupper(t))
}

load_league <- function() {
  conn  <- mfl_connect(season = SEASON, league_id = LEAGUE_ID)
  teams <- read_sheet(LEAGUE_SS, sheet = "Team") %>%
    select(franchise_id, Name) %>%
    left_join(ff_franchises(conn) %>% select(franchise_id, Franchise = franchise_name), by = "franchise_id")
  list(
    conn     = conn,
    teams    = teams,
    weekly   = read_sheet(LEAGUE_SS, sheet = "WeeklyLookup"),
    seasonal = read_sheet(LEAGUE_SS, sheet = "SeasonalLookup"),
    players  = mfl_players(conn) %>% transmute(player_id = as.character(player_id), player_name, pos, nfl = norm_team(team))
  )
}

# One row per player per franchise per week, with MFL's best-ball starter flag.
# liveScoring leaves bench scores blank, so player scores come from playerScores.
get_week <- function(conn, wk) {
  fr <- mfl_getendpoint(conn, "liveScoring", W = wk, DETAILS = 1)$content$liveScoring$franchise
  ps <- mfl_getendpoint(conn, "playerScores", W = wk)$content$playerScores$playerScore
  ps <- tibble(player_id = map_chr(ps, "id"), score = suppressWarnings(as.numeric(map_chr(ps, "score"))))
  map_dfr(fr, function(f) {
    pl <- f$players$player
    tibble(
      week         = wk,
      franchise_id = f$id,
      team_score   = as.numeric(f$score),
      yet_to_play  = as.numeric(f$playersYetToPlay %||% 0) + as.numeric(f$playersCurrentlyPlaying %||% 0),
      player_id    = map_chr(pl, "id"),
      starter      = map_chr(pl, "status") == "starter"
    )
  }) %>%
    left_join(ps, by = "player_id") %>%
    mutate(score = replace_na(score, 0))
}

# Rank -> points for any tie size: tied teams split the points of the places they span
award <- function(rank, lookup, col) {
  whole <- lookup %>% filter(Rank == floor(Rank)) %>% arrange(Rank)
  vals  <- whole[[col]]
  vals  <- c(vals, rep(0, 12))
  map_dbl(seq_along(rank), function(i) {
    n_tied <- sum(rank == rank[i])
    first  <- rank[i] - (n_tied - 1) / 2
    mean(vals[first:(first + n_tied - 1)])
  })
}

load_scores <- function(lg, through_week) {
  # Earlier weeks come from the cache once final; the week being reported is always refetched
  pl <- map_dfr(seq_len(through_week), ~ get_week_cached(lg$conn, SEASON, .x, force = .x == through_week)) %>%
    left_join(lg$players, by = "player_id")

  results <- pl %>%
    group_by(week, franchise_id) %>%
    summarise(points = first(team_score),
              pending = first(yet_to_play),
              bench  = sum(score[!starter], na.rm = TRUE), .groups = "drop") %>%
    group_by(week) %>%
    mutate(rank     = rank(-points, ties.method = "average"),
           F1Pts    = award(rank, lg$weekly, "F1Pts"),
           Earnings = award(rank, lg$weekly, "Earnings")) %>%
    ungroup() %>%
    left_join(lg$teams, by = "franchise_id")

  list(players = pl, results = results)
}

standings_through <- function(results, wk) {
  results %>%
    filter(week <= wk) %>%
    group_by(franchise_id, Name, Franchise) %>%
    arrange(week, .by_group = TRUE) %>%
    summarise(
      F1Pts    = sum(F1Pts),
      Wins     = sum(rank == 1),
      Podiums  = sum(rank <= 3),
      PtsFin   = sum(rank <= 6),
      Earnings = sum(Earnings),
      PF       = sum(points),
      Bench    = sum(bench),
      AvgRank  = mean(rank),
      ThisWeek = sum(F1Pts[week == wk]),
      Last3    = paste(rev(tail(F1Pts, 3)), collapse = "-"),
      .groups  = "drop"
    ) %>%
    # Tiebreak: average weekly finish, then points scored (last year's convention)
    arrange(desc(F1Pts), AvgRank, desc(PF)) %>%
    mutate(Pos = row_number())
}

build_standings <- function(results, wk) {
  now  <- standings_through(results, wk)
  prev <- if (wk > 1) standings_through(results, wk - 1) %>% select(franchise_id, PrevPos = Pos) else NULL
  if (is.null(prev)) return(now %>% mutate(Move = NA_integer_))
  now %>% left_join(prev, by = "franchise_id") %>% mutate(Move = PrevPos - Pos) %>% select(-PrevPos)
}

# Traded revenue ledger: the league sheet `traded` tab (Sender, Receiver, Amount, Year)
load_traded <- function() {
  read_sheet(LEAGUE_SS, sheet = "traded") %>%
    filter(!is.na(Year)) %>%
    select(Sender, Receiver, Amount, Year) %>%
    mutate(Sender = fix_owner(Sender), Receiver = fix_owner(Receiver))
}

traded_by_owner <- function(traded, names, year) {
  t <- traded %>% filter(Year == year)
  unknown <- setdiff(c(t$Sender, t$Receiver), names)
  if (length(unknown)) warning("Unmatched owner names in traded ledger: ", paste(unknown, collapse = ", "))
  bind_rows(t %>% transmute(Name = Receiver, amt = Amount),
            t %>% transmute(Name = Sender,   amt = -Amount)) %>%
    group_by(Name) %>% summarise(Traded = sum(amt), .groups = "drop")
}

# Best possible best-ball lineup from every rostered player that week (QB1 RB2 WR3 TE1 FLEX2)
team_of_week <- function(players_wk) {
  p <- players_wk %>% filter(pos %in% c("QB", "RB", "WR", "TE")) %>% arrange(desc(score))
  take <- function(pool, pos_ok, n, slot) pool %>% filter(pos %in% pos_ok) %>% head(n) %>% mutate(slot = slot)
  qb <- take(p, "QB", 1, "QB"); p <- anti_join(p, qb, by = "player_id")
  rb <- take(p, "RB", 2, "RB"); p <- anti_join(p, rb, by = "player_id")
  wr <- take(p, "WR", 3, "WR"); p <- anti_join(p, wr, by = "player_id")
  te <- take(p, "TE", 1, "TE"); p <- anti_join(p, te, by = "player_id")
  fx <- take(p, c("RB", "WR", "TE"), 2, "FLEX")
  bind_rows(qb, rb, wr, te, fx)
}

# 2027 cap position so far. Revenue still to come: seasonal F1 (by final rank) and socialism.
CAP_BASE <- 230
cap_table <- function(lg, st) {
  ros <- ff_rosters(lg$conn) %>%
    group_by(franchise_id) %>%
    summarise(Salary = sum(salary[coalesce(contract_years, 0) > 0], na.rm = TRUE),
              Signed = sum(coalesce(contract_years, 0) > 0), .groups = "drop")
  adj <- mfl_getendpoint(lg$conn, endpoint = "salaryAdjustments")$content$salaryAdjustments$salaryAdjustment
  hits <- tibble(franchise_id = map_chr(adj, "franchise_id"), amt = as.numeric(map_chr(adj, "amount"))) %>%
    group_by(franchise_id) %>% summarise(CapHit = sum(amt), .groups = "drop")
  traded <- traded_by_owner(load_traded(), lg$teams$Name, SEASON + 1)
  lg$teams %>%
    left_join(ros, by = "franchise_id") %>%
    left_join(hits, by = "franchise_id") %>%
    left_join(traded, by = "Name") %>%
    left_join(st %>% select(franchise_id, F1Pts), by = "franchise_id") %>%
    mutate(across(c(Salary, Signed, CapHit, Traded, F1Pts), ~ replace_na(.x, 0)),
           Space = CAP_BASE + Traded + F1Pts - Salary - CapHit) %>%
    arrange(desc(Space))
}
