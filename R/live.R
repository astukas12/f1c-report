# Live page data from the Sim Center's own run (19 Nov 2026, Andrew: "they should always match. its just 1 single
# sim project"). make_preview.R calls write_live() with the board and sim it just made, so the live page ships a
# subsample of the same draws (first N_LIVE of the 50,000), the same projection file, GTS sims and rosters, plus the
# Sim Center's own table (pregame), which the page shows as-is whenever no game is underway.
#
# Writes live/week-NN.json (+ .bin draws, -tm.bin GTS team totals) here and in the f1c-live repo (F1C_LIVE_DIR).
# make_live.R is now only for replaying a finished past week.

suppressPackageStartupMessages(library(jsonlite))
N_LIVE   <- 5000   # draws per player shipped to the browser (2 bytes each)
LIVE_DIR <- Sys.getenv("F1C_LIVE_DIR", file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Documents", "GitHub", "f1c-live"))

# Commit + push the f1c-live repo. Warns (never stops) on failure, so the Sim Center still publishes.
push_live <- function(msg) {
  old <- Sys.getenv("HOME"); Sys.setenv(HOME = Sys.getenv("USERPROFILE")); on.exit(Sys.setenv(HOME = old))
  g <- function(...) suppressWarnings(system2("git", c("-C", shQuote(LIVE_DIR), ...), stdout = TRUE, stderr = TRUE))
  g("add", "-A")
  if (!length(g("status", "--porcelain"))) { message("Live page: nothing changed"); return(invisible(TRUE)) }
  g("commit", "-q", "-m", shQuote(msg))
  ps <- g("push", "-q", "origin", "main")
  if (!is.null(attr(ps, "status")) && attr(ps, "status") != 0) {
    message("WARNING: live page commit made but push FAILED: ", paste(ps, collapse = " ")); return(invisible(FALSE)) }
  message("Live page pushed ", g("rev-parse", "--short", "HEAD"))
  invisible(TRUE)
}

write_live <- function(week, board, sim, PROJ, GTS, lg, run, psrc) {
  DR <- attr(sim, "draws"); pick <- attr(sim, "pick")
  if (is.null(DR) || ncol(DR) != N_LIVE) stop("write_live needs sim_week(..., keep = N_LIVE)")
  board <- board %>% mutate(how = case_when(gts_team & !is.na(gkey) ~ "gts", gts_team ~ "none", med > 0 ~ "etr", TRUE ~ "none"))
  D <- pmin(pmax(round(DR * 10), 0), 32767)
  keep <- which(rowSums(D) > 0)
  board$row <- NA_integer_; board$row[keep] <- seq_along(keep) - 1L

  # Each GTS team's simulated fantasy total for the same picked sims (the page's game-script proxy)
  TM <- NULL
  if (!is.null(GTS) && length(pick)) {
    sims <- sort(unique(GTS$SimID))
    tt <- GTS %>% group_by(team, SimID) %>% summarise(pts = sum(pts), .groups = "drop")
    TM <- do.call(rbind, lapply(split(tt, tt$team), function(d) { v <- numeric(max(sims)); v[d$SimID] <- d$pts; v[pick] }))
    TM <- pmin(pmax(round(TM * 10), 0), 32767)
  }

  sch <- tryCatch(fromJSON(sprintf("https://api.myfantasyleague.com/%d/export?TYPE=nflSchedule&W=%d&JSON=1", SEASON, week),
                           simplifyVector = FALSE)$nflSchedule$matchup,
                  error = function(e) { warning("No NFL schedule: ", conditionMessage(e)); NULL })
  games <- bind_rows(lapply(sch, function(m) {
    t <- bind_rows(lapply(m$team, function(x) tibble(id = x$id, isHome = x$isHome)))
    tibble(kickoff = as.numeric(m$kickoff), away = norm_team(t$id[t$isHome == "0"]), home = norm_team(t$id[t$isHome == "1"]))
  }))
  ids <- tryCatch(dp_playerids() %>% transmute(player_id = as.character(mfl_id), espn = as.character(espn_id)) %>%
                    filter(!is.na(player_id), !is.na(espn)) %>% distinct(player_id, .keep_all = TRUE),
                  error = function(e) { warning("No ESPN ids: ", conditionMessage(e)); tibble(player_id = character(), espn = character()) })
  board <- board %>% left_join(ids, by = "player_id")

  rf <- sprintf("data/cache/report-w%02d.rds", week - 1)
  st <- tryCatch({ R <- readRDS(if (file.exists(rf)) rf else "data/cache/report-latest.rds")
    R$st %>% transmute(id = franchise_id, f1 = F1Pts) }, error = function(e) NULL)
  teams <- lg$teams %>% transmute(id = franchise_id, name = Name, franchise = Franchise)
  if (!is.null(st)) teams <- teams %>% left_join(st, by = "id")

  srcs <- if (any(board$how == "gts")) paste0("GTS sims", if (any(board$how == "etr")) paste0(" + ", psrc, " for the rest") else "") else
    paste(psrc, "projections")
  out <- list(
    season = SEASON, league = LEAGUE_ID, week = week, nsim = N_LIVE,
    built = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    run = run,                                         # same stamp as the Sim Center's dek line
    sources = srcs, f1 = F1_WEEK, teams = teams, games = games,
    final_teams = I(sort(unique(board$nfl[board$done]))), # the Sim Center table below is exact for this state
    pregame = sim %>% transmute(id = franchise_id, mean, win, top3, top6, ef1 = expF1),
    players = board %>% transmute(id = player_id, espn, name = Player, pos, nfl, fid = franchise_id,
                                  proj = round(if_else(how == "none", 0, if_else(how == "gts", coalesce(gmean, 0), med)), 1), row,
                                  src = how),
    tm = if (!is.null(TM)) tibble(team = rownames(TM), row = seq_len(nrow(TM)) - 1L) else list()
  )
  for (d in c("live", LIVE_DIR)) {
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    write_json(out, sprintf("%s/week-%02d.json", d, week), auto_unbox = TRUE, digits = NA, dataframe = "rows")
    con <- file(sprintf("%s/week-%02d.bin", d, week), "wb")
    writeBin(as.integer(t(D[keep, , drop = FALSE])), con, size = 2, endian = "little"); close(con)
    tf <- sprintf("%s/week-%02d-tm.bin", d, week)
    if (!is.null(TM)) { con <- file(tf, "wb"); writeBin(as.integer(t(TM)), con, size = 2, endian = "little"); close(con) }
    else if (file.exists(tf)) file.remove(tf)
  }
  cur <- tryCatch(read_json("live/current.json")$week, error = function(e) 0)
  if (week >= cur) for (d in c("live", LIVE_DIR)) write_json(list(week = week), file.path(d, "current.json"), auto_unbox = TRUE)
  invisible(file.copy("live/index.html", file.path(LIVE_DIR, "index.html"), overwrite = TRUE))
  message(sprintf("Live data: week %d, %d players with draws (%s of the same %s sims), %d games -> live/ and %s",
                  week, length(keep), format(N_LIVE, big.mark = ","), format(SIM_N, big.mark = ","), nrow(games), LIVE_DIR))
}
