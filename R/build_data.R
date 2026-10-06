# Everything the weekly report needs, pulled once per run and saved to data/cache/report-wNN.rds.
# Each section page (Quarto) reads that file, so MFL and the league sheet are hit once per build.

build_report_data <- function(WEEK) {
  lg  <- load_league()
  sc  <- load_scores(lg, WEEK)
  res <- sc$results
  ros <- load_rosters(lg)
  lu  <- lineups(sc, lg, ros)
  st  <- build_standings(res, WEEK)
  prev_st <- if (WEEK > 1) standings_through(res, WEEK - 1) else NULL

  # history (finished seasons from data/cache; 2024 left out of comparisons, kept for all-time F1)
  hp     <- hist_positions(load_history())
  hall   <- history_weekly(2022:2025)
  hplay  <- lapply(HIST_YEARS, function(y) { h <- history_season(y, players = FALSE); if (is.null(h$players)) NULL else
              h$players %>% distinct(week, franchise_id, player_id, .keep_all = TRUE) %>% left_join(h$names, by = "player_id") %>% mutate(year = y) })
  hplay  <- bind_rows(hplay)

  # sims
  ohist <- ros_history(lu, res, lg, WEEK)
  wsim  <- { f <- sprintf("cards/week-%02d-sim.csv", WEEK); if (file.exists(f)) read.csv(f, stringsAsFactors = FALSE, colClasses = c(franchise_id = "character")) else NULL }

  # money and moves
  cc   <- cap_check(lg, st)
  tags <- franchise_tags(ros)
  fas  <- pending_fas(ros, lu, lg)
  ytd  <- tryCatch({
    ps <- mfl_getendpoint(lg$conn, "playerScores", W = "YTD")$content$playerScores$playerScore
    tibble(player_id = map_chr(ps, "id"), ytd = suppressWarnings(as.numeric(map_chr(ps, "score"))))
  }, error = function(e) NULL)
  trd  <- traded_summary(lg, WEEK)
  tx   <- ff_transactions(lg$conn) %>% mutate(player_id = as.character(player_id))
  faab <- tryCatch({
    fr <- mfl_getendpoint(lg$conn, "league")$content$league$franchises$franchise
    tibble(franchise_id = map_chr(fr, "id"), faab = as.numeric(map_chr(fr, ~ .x$bbidAvailableBalance %||% NA_character_)))
  }, error = function(e) NULL)
  moves <- roster_moves(lu, lg)

  list(WEEK = WEEK, built = Sys.time(), teams = lg$teams, weekly_lookup = lg$weekly, seasonal_lookup = lg$seasonal,
       res = res, lu = lu, st = st, prev_st = prev_st, ros = ros,
       hp = hp, hall = hall, hplay = hplay, ohist = ohist, wsim = wsim,
       cc = cc, tags = tags, fas = fas, ytd = ytd, trd = trd, tx = tx, faab = faab, moves = moves)
}
