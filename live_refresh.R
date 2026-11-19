# Live page refreshes from Task Scheduler (Andrew, 12 Nov 2026). No Claude involved.
#   Rscript live_refresh.R --roster   daily 12:00, an hour after MFL waivers: rebuild this week's rosters
#   Rscript live_refresh.R --gts      before each game window (Thu 7:50 PM, Sun 9:05 AM / 12:35 / 3:55 / 7:55 PM,
#                                     Mon 7:50 PM): sim any GTS slate sheet saved since the last run, then rebuild
#   Rscript live_refresh.R --mfl      from the start of each game window: once a game is final (+10 min), MFL's official
#                                     scores -> week-NN-mfl.json (the page shows them for final games)
#   Friday and Saturday roster runs pull fresh ETR first (--etr forces it); Sunday is GTS.
#   options: --week N (default: Sleeper's current week)  --no-push  --dry-run (--gts: say what it would sim, change nothing)
#            --proj <csv>  build from exactly this projection file (no Friday/Saturday ETR pull)
#                          (used to pin a file; both pages always come from one make_preview.R run).
# Both run make_preview.R: ONE 50k sim (newest ETR/Rotowire projections, GTS draws where simmed) that writes the
# Sim Center files and the live data; it pushes f1c-live, then this re-renders and pushes the Sim Center (19 Nov 2026).
# One line per run in logs/live_refresh.log.
# auto_sims.R and manual make_preview.R runs publish the live page too (same run).

suppressMessages(library(jsonlite))
args <- commandArgs(trailingOnly = TRUE)
setwd(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))))
RSCRIPT  <- file.path(R.home("bin"), "Rscript.exe")
LIVE_DIR <- Sys.getenv("F1C_LIVE_DIR", file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Documents", "GitHub", "f1c-live"))
SLATES   <- "C:/Users/astuk/OneDrive/Documents/GTS/NFL/slates"
STATE    <- "logs/live_refresh_gts_state.json"   # sheet -> modified time last simmed
MODE     <- if ("--gts" %in% args) "gts" else if ("--mfl" %in% args) "mfl" else "roster"
DRY      <- "--dry-run" %in% args
NOPUSH   <- "--no-push" %in% args || DRY
PROJF    <- { k <- match("--proj", args); if (is.na(k)) NULL else args[k + 1] }
dir.create("logs", showWarnings = FALSE)
logl <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M"), "  ", MODE, "  ", paste0(...))
  cat(line, "\n", file = "logs/live_refresh.log", append = TRUE); cat(line, "\n")
}
done <- function(code = 0) quit(save = "no", status = code)

# ---- week ----
wi <- match("--week", args)
WEEK <- if (!is.na(wi)) as.integer(args[wi + 1]) else
  tryCatch(as.integer(fromJSON("https://api.sleeper.app/v1/state/nfl")$week), error = function(e) NA_integer_)
if (is.na(WEEK) || WEEK < 1) WEEK <- tryCatch(as.integer(read_json("live/current.json")$week), error = function(e) NA_integer_)
if (is.na(WEEK)) { logl("FAILED: could not tell the current week"); done(1) }

# ---- MFL: official scores once a player's game is over (browsers can't read MFL, so this publishes them) ----
if (MODE == "mfl") {
  # Watcher (Andrew, 15 Nov 2026: "a single check per game once it goes final"): started at each game window, it
  # checks ESPN's scoreboard every 3 minutes; 10 minutes after a game goes final it pulls MFL once and publishes;
  # one retry 30 minutes later only if MFL still has 0 for a rostered player ESPN shows with stats. Exits when every
  # game that has started is synced and nothing kicks off in the next 20 minutes.
  if (is.na(wi)) WEEK <- tryCatch(as.integer(read_json("live/current.json")$week), error = function(e) WEEK)   # the week the page shows
  suppressPackageStartupMessages(library(ffscrapr))
  ESPN <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/"
  tc   <- function(a) c(WSH = "WAS", LAR = "LA")[a] %||% a
  MST  <- "logs/live_refresh_mfl_state.json"
  fn   <- sprintf("week-%02d-mfl.json", WEEK)
  Sys.setenv(HOME = Sys.getenv("USERPROFILE"))
  g    <- function(...) suppressWarnings(system2("git", c("-C", shQuote(LIVE_DIR), ...), stdout = TRUE, stderr = TRUE))
  roster <- tryCatch(fromJSON(file.path(LIVE_DIR, sprintf("week-%02d.json", WEEK)))$players, error = function(e) NULL)
  mfl_pull <- function() {   # whole league: every player whose game is over; push if anything changed
    ls <- mfl_getendpoint(mfl_connect(season = 2026, league_id = 43580), "liveScoring", W = WEEK, DETAILS = 1)$content$liveScoring
    pl <- list(); tm <- list()
    for (f in ls$franchise) {
      tm[[as.character(f$id)]] <- as.numeric(f$score)
      for (z in f$players$player) if (isTRUE(as.numeric(z$gameSecondsRemaining) == 0)) pl[[as.character(z$id)]] <- as.numeric(z$score)
    }
    out <- list(week = WEEK, players = pl, teams = tm)
    js  <- function(x) as.character(toJSON(x, auto_unbox = TRUE, digits = NA))
    f   <- file.path(LIVE_DIR, fn)
    old <- if (file.exists(f)) tryCatch({ o <- read_json(f); o$updated <- NULL; o }, error = function(e) NULL) else NULL
    if (!is.null(old) && identical(js(old), js(out))) return(list(pl = pl, msg = "MFL unchanged"))
    out$updated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    for (d in c(LIVE_DIR, "live")) write_json(out, file.path(d, fn), auto_unbox = TRUE, digits = NA)
    if (NOPUSH) return(list(pl = pl, msg = "written, not pushed"))
    g("add", fn); g("commit", "-q", "-m", shQuote(sprintf("Week %d MFL scores", WEEK)))
    ps <- g("push", "-q", "origin", "main")
    if (!is.null(attr(ps, "status")) && attr(ps, "status") != 0) return(list(pl = pl, msg = paste("push FAILED:", paste(ps, collapse = " "))))
    list(pl = pl, msg = paste("pushed", g("rev-parse", "--short", "HEAD")))
  }
  lagging <- function(ev, pl) {   # rostered players in this game with ESPN stats but 0 (or nothing) on MFL
    if (is.null(roster)) return(character())
    sm <- tryCatch(fromJSON(paste0(ESPN, "summary?event=", ev$id), simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(sm)) return(character())
    ids <- unique(unlist(lapply(sm$boxscore$players, function(t) lapply(t$statistics, function(c) sapply(c$athletes, function(a) a$athlete$id)))))
    r <- roster[roster$nfl %in% ev$teams & !is.na(roster$espn) & roster$espn %in% ids, ]
    r$name[vapply(r$id, function(i) is.null(pl[[i]]) || pl[[i]] == 0, logical(1))]
  }
  st <- if (file.exists(MST)) tryCatch(read_json(MST), error = function(e) list()) else list()
  wk <- paste0("w", WEEK); if (is.null(st[[wk]])) st[[wk]] <- list()
  t_end <- Sys.time() + 10 * 3600; first <- TRUE
  repeat {
    sb <- tryCatch(fromJSON(sprintf("%sscoreboard?seasontype=2&week=%d&dates=2026", ESPN, WEEK), simplifyVector = FALSE)$events, error = function(e) NULL)
    if (is.null(sb)) { cat("scoreboard unavailable, retrying\n"); Sys.sleep(180); next }
    now <- Sys.time(); evs <- lapply(sb, function(e) list(id = e$id, name = e$shortName, state = e$status$type$state,
      kick = as.POSIXct(e$date, format = "%Y-%m-%dT%H:%MZ", tz = "UTC"),
      teams = unname(sapply(e$competitions[[1]]$competitors, function(c) tc(c$team$abbreviation)))))
    r_now <- NULL   # one MFL pull per pass, shared by every game due in it
    for (ev in evs) {
      s <- st[[wk]][[ev$id]] %||% list()
      if (ev$state != "post" || isTRUE(s$done)) next
      if (is.null(s$final)) s$final <- as.numeric(now) - if (first) 600 else 0   # already final when the watcher started: sync now
      due <- if (is.null(s$synced)) s$final + 600 else s$synced + 1800
      if (as.numeric(now) >= due) {
        if (is.null(r_now)) r_now <- tryCatch(mfl_pull(), error = function(e) list(pl = list(), msg = paste("MFL FAILED:", conditionMessage(e))))
        r <- r_now
        lag <- lagging(ev, r$pl)
        retry <- !is.null(s$synced)
        logl(sprintf("week %d: %s final, MFL %s%s%s", WEEK, ev$name, if (retry) "retry, " else "", r$msg,
                     if (length(lag)) paste0("; MFL still 0 for ", paste(lag, collapse = ", ")) else ""))
        s$synced <- as.numeric(now); s$done <- retry || !length(lag)
      }
      st[[wk]][[ev$id]] <- s
    }
    write_json(st, MST, auto_unbox = TRUE, digits = NA)
    started <- Filter(function(e) e$kick <= now, evs)
    pending <- Filter(function(e) !isTRUE(st[[wk]][[e$id]]$done), started)
    soon <- any(sapply(evs, function(e) e$kick > now && e$kick <= now + 20 * 60))
    if (!length(pending) && !soon) { cat("all started games synced; exiting\n"); done() }
    if (DRY || Sys.time() > t_end) done()
    first <- FALSE; Sys.sleep(180)
  }
}

has <- function(dir, pat) length(list.files(dir, pat)) > 0
if (!has("data/etr", sprintf("^ETR_Week%d_.*\\.csv$", WEEK)) && !has("data/rw", sprintf("^RW_Week%d_.*\\.csv$", WEEK))) {
  logl(sprintf("week %d: no projections yet, skipped", WEEK)); done()
}

# ---- GTS: sheets saved since the last ingest ----
label <- "rosters"
if (MODE == "gts") {
  thu   <- as.Date("2026-09-10") + 7 * (WEEK - 1)
  days  <- format(thu + (-2):4, "%Y-%m-%d")   # Tue .. Mon around this week's Thursday
  pick_sheets <- function() {
    f <- list.files(SLATES, "\\.xlsx$", full.names = TRUE)
    f <- f[substr(basename(f), 1, 10) %in% days & !grepl("\\.bak|~\\$|copy", basename(f), ignore.case = TRUE)]
    st <- if (file.exists(STATE)) tryCatch(read_json(STATE), error = function(e) list()) else list()
    seen <- vapply(basename(f), function(b) as.numeric(st[[b]] %||% -1), 0)
    new <- f[as.numeric(file.mtime(f)) > seen + 1]
    new[order(file.mtime(new))]   # oldest first, so a game in two sheets ends up from the newest one
  }
  sh <- pick_sheets()
  if (!length(sh)) { logl(sprintf("week %d: no new GTS sheet since the last run", WEEK)); done() }
  if (any(difftime(Sys.time(), file.mtime(sh), units = "mins") < 5) && !DRY) {   # still being saved: wait once
    cat("A sheet changed in the last 5 minutes; waiting 5 minutes\n"); Sys.sleep(300); sh <- pick_sheets()
  }
  ids <- sub("\\.xlsx$", "", basename(sh))
  gf  <- sprintf("data/gts/week-%02d-gts.rds", WEEK)
  if (DRY) {
    have <- if (file.exists(gf)) attr(readRDS(gf), "games") else character()
    logl(sprintf("week %d: DRY RUN would sim %s (--add keeps the other games already simmed: %s), then make_preview.R %d (both pages)",
                 WEEK, paste(ids, collapse = ", "), paste(have, collapse = " "), WEEK)); done()
  }
  r <- system2(RSCRIPT, c("make_gts.R", WEEK, ids, "--sims", "50000", if (file.exists(gf)) "--add"))
  if (r != 0) { logl(sprintf("week %d: make_gts.R FAILED on %s", WEEK, paste(ids, collapse = ", "))); done(1) }
  st <- if (file.exists(STATE)) tryCatch(read_json(STATE), error = function(e) list()) else list()
  for (f in sh) st[[basename(f)]] <- as.numeric(file.mtime(f))
  write_json(st, STATE, auto_unbox = TRUE, digits = NA)
  label <- paste("GTS", paste(unique(sub("^\\d{4}-\\d{2}-\\d{2}_", "", ids)), collapse = " + "))
}

# ---- roster runs on Friday and Saturday (or --etr): pull fresh ETR first (saved login; Sunday is GTS) ----
etr_note <- ""
if (MODE == "roster" && is.null(PROJF) && ("--etr" %in% args || format(Sys.Date(), "%u") %in% c("5", "6")) && !DRY) {
  FANTASY <- "C:/Users/astuk/OneDrive/Documents/nascaR/Fantasy"
  r <- tryCatch({
    source(file.path(FANTASY, "F1C_ETR_Pull.R"), local = TRUE)
    tmp <- file.path(tempdir(), "etr"); dir.create(tmp, showWarnings = FALSE)
    f <- etr_week_pull(WEEK, dir = tmp, quiet = TRUE); p <- read.csv(f, check.names = FALSE)
    # still last week's table? its matchups should match the ETR file already saved for this week
    prev <- list.files("data/etr", sprintf("^ETR_Week%d_.*\\.csv$", WEEK), full.names = TRUE)
    if (length(prev)) {
      q <- read.csv(prev[which.max(file.mtime(prev))], check.names = FALSE)
      mk <- function(d) unique(paste(d$Team, sub("^@", "", d$Opp)))
      if (mean(mk(p) %in% mk(q)) < .6) stop("ETR table is not on week ", WEEK, " matchups")
    }
    out <- sprintf("data/etr/ETR_Week%d_Projections_%s.csv", WEEK, format(as.POSIXlt(Sys.time(), tz = "America/New_York"), "%m%d_%H%M"))
    file.copy(f, out, overwrite = TRUE); file.copy(f, file.path(FANTASY, sprintf("ETR_Week%d_Projections.csv", WEEK)), overwrite = TRUE)
    sprintf("fresh ETR %s (%d rows)", basename(out), nrow(p))
  }, error = function(e) paste("ETR pull failed, kept the last file:", conditionMessage(e)))
  etr_note <- paste0(", ", r); label <- "rosters + ETR"
}

# ---- one sim for both pages (19 Nov 2026, Andrew: "they should always match. its just 1 single sim project") ----
# make_preview.R runs the 50k sim once and writes the Sim Center files AND the live page data from that run (it pushes
# f1c-live itself); then the Sim Center site is re-rendered and pushed. Either publish failing is logged, never silent.
if (!is.null(PROJF)) etr_note <- paste0(etr_note, ", projections ", basename(PROJF))
out <- suppressWarnings(system2(RSCRIPT, c("make_preview.R", WEEK, if (!is.null(PROJF)) shQuote(PROJF), if (NOPUSH) "--no-push"),
                                stdout = TRUE, stderr = TRUE))
if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
  logl(sprintf("week %d: make_preview.R FAILED (neither page updated): %s", WEEK, paste(tail(out, 2), collapse = " / "))); done(1)
}
live_msg <- grep("^Live page|^WARNING: live", out, value = TRUE)
built    <- sub("^Live data: ", "", grep("^Live data:", out, value = TRUE)[1] %||% "")
run      <- tryCatch(read.csv(sprintf("cards/week-%02d-sim.csv", WEEK))$run[1], error = function(e) "?")

site <- "not pushed (--no-push)"
if (!NOPUSH) {
  Sys.setenv(HOME = Sys.getenv("USERPROFILE"))   # git identity from ~/.gitconfig
  r <- system2(RSCRIPT, c("make_report.R", WEEK - 1, "--render-only"))
  if (r != 0) site <- "Sim Center render FAILED" else {
    system2("git", c("add", "docs", "img", "data/cache", "weeks", "archive.qmd"))
    system2("git", c("commit", "-q", "-m", shQuote(sprintf("Week %d sims: %s (%s)", WEEK, run, label))))
    site <- if (system2("git", c("push", "-q")) == 0) paste("Sim Center pushed", system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE)) else
      "Sim Center push FAILED"
  }
}
logl(sprintf("week %d: %s, run %s; %s; %s%s (%s)", WEEK, label, run, paste(live_msg, collapse = " "), site, etr_note, built))
done(if (grepl("FAILED", paste(site, live_msg))) 1 else 0)
