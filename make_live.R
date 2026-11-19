# PAST-WEEK REPLAYS ONLY (19 Nov 2026). The current week's live data comes from make_preview.R (R/live.R), the same
# sim as the Sim Center; running this for the current week would re-sim and drift from it, so it refuses.
# Live page data:  Rscript make_live.R 10            (current week, before kickoff or any time during it)
#                  Rscript make_live.R 9            (past week: open live/?week=9&replay=1 to replay it)
#
# Live scores come from ESPN's public game feeds in the viewer's browser (MFL blocks browser reads), scored with the
# league rules (full PPR, 4-pt pass TD, -1 INT, -1 fumble lost). MFL stays the official score.
# Writes live/week-NN.json (rosters, teams, NFL games) and live/week-NN.bin (correlated sim draws per player),
# copied to the standalone site folder (F1C_LIVE_DIR). The live page (live/index.html) reads MFL's live scoring in the browser and re-sims
# every minute: final = actual so far + draw x share of the game left. Same sources as make_preview.R:
# GTS draws for games GTS covers (one SimID per simulated week, so stacks stay correlated), ETR lognormal otherwise.

args <- commandArgs(trailingOnly = TRUE)
setwd(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))))

source("R/data.R")
source("R/sim.R")
suppressPackageStartupMessages(library(jsonlite))

WEEK   <- if (length(args)) as.integer(args[1]) else stop("Pass the week, e.g. Rscript make_live.R 10")
if (!file.exists(sprintf("data/cache/%d-week-%02d.rds", 2026, WEEK)) && !"--force" %in% args)
  stop("make_live.R is for finished past weeks only; the current week comes from make_preview.R (one sim for both pages). --force to override.")
N_LIVE <- 5000   # draws per player shipped to the browser (2 bytes each)

# Projections: explicit path, else newest ETR then Rotowire file saved for this week by auto_sims.R
newest <- function(dir, pat) { f <- list.files(dir, pat, full.names = TRUE); if (length(f)) f[which.max(file.mtime(f))] }
pf <- if (length(args) > 1) args[2] else newest("data/etr", sprintf("^ETR_Week%d_.*\\.csv$", WEEK)) %||%
  newest("data/rw", sprintf("^RW_Week%d_.*\\.csv$", WEEK))
PROJ <- if (is.null(pf)) read_etr() else read_etr(pf)
GTS  <- load_gts(WEEK)
message("Projections: ", basename(attr(PROJ, "file")), if (!is.null(GTS)) paste0(" + GTS (", length(attr(GTS, "games")), " games)"))

lg    <- load_league()
board <- sim_board(lg, WEEK, PROJ, GTS) %>%
  mutate(how = case_when(gts_team & !is.na(gkey) ~ "gts", gts_team ~ "none", med > 0 ~ "etr", TRUE ~ "none"))

# Finished past week: that week's rosters (data/cache/2026-week-NN.rds), not today's. Players since dropped
# come back from lg$players with no projection and no draws; players added since are dropped.
wf <- sprintf("data/cache/%d-week-%02d.rds", SEASON, WEEK)
if (file.exists(wf)) {
  wr <- readRDS(wf) %>% mutate(player_id = as.character(player_id))
  if (nrow(wr) && all(wr$yet_to_play == 0)) {
    wr <- wr %>% distinct(player_id, .keep_all = TRUE) %>% select(player_id, wk_fid = franchise_id)
    add <- wr %>% filter(!player_id %in% board$player_id) %>% inner_join(lg$players, by = "player_id") %>%
      filter(pos %in% c("QB", "RB", "WR", "TE")) %>%
      mutate(ln = str_trim(str_extract(player_name, "^[^,]+")), fn = str_trim(str_extract(player_name, "(?<=,).*")),
             Player = if_else(is.na(fn), player_name, paste(fn, ln))) %>%
      transmute(player_id, franchise_id = wk_fid, Player, pos, nfl, med = 0, sigma = SIG_DEF, gkey = NA_character_,
                gmean = NA_real_, gts_team = FALSE, how = "none")
    board <- board %>% inner_join(wr, by = "player_id") %>% mutate(franchise_id = wk_fid) %>% select(-wk_fid) %>%
      bind_rows(add)
    message(sprintf("Week %d rosters from %s: %d players (%d no longer rostered, no draws)", WEEK, basename(wf), nrow(board), nrow(add)))
  }
}

# Draws for every rostered skill player, whether or not his game has started (the browser decides what is locked)
set.seed(42)
D <- matrix(0, nrow(board), N_LIVE)
e <- which(board$how == "etr")
if (length(e)) D[e, ] <- board$med[e] * exp(board$sigma[e] * matrix(rnorm(length(e) * N_LIVE), length(e)) - board$sigma[e]^2 / 2)
g <- which(board$how == "gts")
if (length(g)) {
  sims <- sort(unique(GTS$SimID))
  pick <- sims[sample.int(length(sims), N_LIVE, replace = length(sims) < N_LIVE)]
  gw   <- GTS %>% mutate(gkey = paste(player, team, sep = "|")) %>% filter(gkey %in% board$gkey[g])
  gd   <- lapply(split(gw, gw$gkey), function(d) { v <- numeric(max(sims)); v[d$SimID] <- d$pts; v[pick] })
  for (i in g) D[i, ] <- gd[[board$gkey[i]]]
  # Each GTS team's simulated fantasy total (every skill player in the sim, rostered or not) for the same picked sims:
  # the live page compares it with the real team's points so far to weight the sims that look like the game (no team
  # scores in the GTS output, so this is the game-script proxy)
  tt <- GTS %>% group_by(team, SimID) %>% summarise(pts = sum(pts), .groups = "drop")
  TM <- do.call(rbind, lapply(split(tt, tt$team), function(d) { v <- numeric(max(sims)); v[d$SimID] <- d$pts; v[pick] }))
  TM <- pmin(pmax(round(TM * 10), 0), 32767)
}
D <- pmin(pmax(round(D * 10), 0), 32767)
keep <- which(rowSums(D) > 0)
board$row <- NA_integer_; board$row[keep] <- seq_along(keep) - 1L

# NFL games this week (kickoff + teams) for the game list and the replay clock
# (league-independent export: only answers on api.myfantasyleague.com, not the league host)
sch <- tryCatch(jsonlite::fromJSON(sprintf("https://api.myfantasyleague.com/%d/export?TYPE=nflSchedule&W=%d&JSON=1", SEASON, WEEK),
                                   simplifyVector = FALSE)$nflSchedule$matchup,
                error = function(e) { warning("No NFL schedule: ", conditionMessage(e)); NULL })
games <- bind_rows(lapply(sch, function(m) {
  t <- bind_rows(lapply(m$team, function(x) tibble(id = x$id, isHome = x$isHome)))
  tibble(kickoff = as.numeric(m$kickoff), away = norm_team(t$id[t$isHome == "0"]), home = norm_team(t$id[t$isHome == "1"]))
}))

# ESPN ids for matching ESPN box scores (name + team is the fallback in the page)
ids <- tryCatch(dp_playerids() %>% transmute(player_id = as.character(mfl_id), espn = as.character(espn_id)) %>%
                  filter(!is.na(player_id), !is.na(espn)) %>% distinct(player_id, .keep_all = TRUE),
                error = function(e) { warning("No ESPN ids: ", conditionMessage(e)); tibble(player_id = character(), espn = character()) })
board <- board %>% left_join(ids, by = "player_id")

# Season F1 points before this week, from the previous week's report build (data/cache/report-wNN.rds, else report-latest.rds)
rf <- sprintf("data/cache/report-w%02d.rds", WEEK - 1)
st <- tryCatch({ R <- readRDS(if (file.exists(rf)) rf else "data/cache/report-latest.rds")
  if (R$WEEK != WEEK - 1) warning(sprintf("report-latest.rds is week %d, not week %d", R$WEEK, WEEK - 1))
  R$st %>% transmute(id = franchise_id, f1 = F1Pts) }, error = function(e) NULL)
teams <- lg$teams %>% transmute(id = franchise_id, name = Name, franchise = Franchise)
if (!is.null(st)) teams <- teams %>% left_join(st, by = "id")

PSRC <- Sys.getenv("F1C_PROJ_SRC", if (grepl("^RW_", basename(attr(PROJ, "file") %||% ""))) "Rotowire" else "ETR")
out <- list(
  season = SEASON, league = LEAGUE_ID, week = WEEK, nsim = N_LIVE,
  built = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  sources = if (any(board$how == "gts")) paste0("GTS sims", if (any(board$how == "etr")) paste0(" + ", PSRC, " for the rest") else "") else
    paste(PSRC, "projections"),
  f1 = F1_WEEK,
  teams = teams,
  games = games,
  players = board %>% transmute(id = player_id, espn, name = Player, pos, nfl, fid = franchise_id,
                                proj = round(if_else(how == "none", 0, if_else(how == "gts", coalesce(gmean, 0), med)), 1), row,
                                src = how),
  tm = if (exists("TM")) tibble(team = rownames(TM), row = seq_len(nrow(TM)) - 1L) else list()
)

# Outputs: live/ here (the page's source) and the standalone site repo (F1C_LIVE_DIR, default GitHub/f1c-live)
LIVE_DIR <- Sys.getenv("F1C_LIVE_DIR", file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Documents", "GitHub", "f1c-live"))
OUT_DIRS <- c("live", LIVE_DIR)
for (d in OUT_DIRS) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  write_json(out, sprintf("%s/week-%02d.json", d, WEEK), auto_unbox = TRUE, digits = NA, dataframe = "rows")
  con <- file(sprintf("%s/week-%02d.bin", d, WEEK), "wb")
  writeBin(as.integer(t(D[keep, , drop = FALSE])), con, size = 2, endian = "little")   # player-major: N_LIVE values per player
  close(con)
  tf <- sprintf("%s/week-%02d-tm.bin", d, WEEK)
  if (exists("TM")) { con <- file(tf, "wb"); writeBin(as.integer(t(TM)), con, size = 2, endian = "little"); close(con) }
  else if (file.exists(tf)) file.remove(tf)
}
# The page opens the week listed here (the newest one built)
cur <- tryCatch(read_json("live/current.json")$week, error = function(e) 0)
if (WEEK >= cur) for (d in OUT_DIRS) write_json(list(week = WEEK), file.path(d, "current.json"), auto_unbox = TRUE)
invisible(file.copy("live/index.html", file.path(LIVE_DIR, "index.html"), overwrite = TRUE))
message(sprintf("Live data: week %d, %d players with draws (%s draws each), %d games -> live/ and %s",
                WEEK, length(keep), format(N_LIVE, big.mark = ","), nrow(games), LIVE_DIR))
