# Automated weekly sims on fresh ETR:  Rscript auto_sims.R [--week N] [--gts] [--fallback] [--no-push]
# --fallback: if ETR fails or isn't on this week yet, use Rotowire's PPR projections from Sleeper instead
#   (ceilings from ETR's prior-week ceiling/proj ratio), labelled "Rotowire projections" on the page
# --gts also runs make_gts.R (50k) on this week's TNF sheet (GTS/NFL/slates/<Thu date>_TNF_*.xlsx, newest) first
# 1. Pulls ETR with the saved login (F1C_ETR_Pull.R, no Chrome / download) and checks it is on this week
# 2. make_preview.R <wk> on that file (50k sims), snapshots week-NN-sim-preview.csv before Thursday kickoff
# 3. make_report.R <wk-1> --render-only, then commits + pushes the site
# Exit codes: 0 ok, 2 ETR not on this week yet, 3 ETR pull failed (login expired?), 4 sim/render/push failed

suppressMessages({ library(dplyr); library(jsonlite); library(curl) })
args <- commandArgs(trailingOnly = TRUE)
setwd(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))))
FANTASY <- "C:/Users/astuk/OneDrive/Documents/nascaR/Fantasy"
RSCRIPT <- file.path(R.home("bin"), "Rscript.exe")
now_et  <- as.POSIXlt(Sys.time(), tz = "America/New_York")
TODAY   <- as.Date(format(now_et, "%Y-%m-%d"))
wi      <- match("--week", args)
WEEK    <- if (!is.na(wi)) as.integer(args[wi + 1]) else
             as.integer(floor(as.numeric(TODAY - as.Date("2026-09-08")) / 7)) + 1   # weeks start Tuesday
quit_with <- function(code, ...) { cat("RESULT:", ..., "\n"); quit(save = "no", status = code) }
cat(sprintf("Week %d auto sims, %s ET\n", WEEK, format(now_et, "%a %b %d %I:%M %p")))

# ---- 1. ETR pull (saved login) into a temp dir, so a stale page never overwrites this week's file ----
source(file.path(FANTASY, "F1C_ETR_Pull.R"))
FALLBACK <- "--fallback" %in% args
SRC <- "ETR"
TMAP <- c(SFO="SF", LAR="LA", NEP="NE", GBP="GB", TBB="TB", LVR="LV", KCC="KC", NOS="NO", JAC="JAX", WSH="WAS")
nt <- function(t) { t <- toupper(t); ifelse(t %in% names(TMAP), TMAP[t], t) }
cpn <- function(x) trimws(tolower(gsub("[^A-Za-z ]", "", gsub(" Jr[.]?$| Sr[.]?$| II$| III$| IV$| V$", "", x, ignore.case = TRUE))))
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
sl <- tryCatch(fromJSON(rawToChar(curl_fetch_memory(sprintf(
  "https://api.sleeper.com/projections/nfl/2026/%d?season_type=regular&position[]=QB&position[]=RB&position[]=WR&position[]=TE", WEEK),
  handle = new_handle(useragent = "Mozilla/5.0", timeout = 40))$content), simplifyVector = FALSE), error = function(e) NULL)

# ETR pull into a temp dir, so a stale page never overwrites this week's file; returns NULL + reason on failure
tmp <- file.path(tempdir(), "etr"); dir.create(tmp, showWarnings = FALSE)
etr_try <- function() {
  f <- tryCatch(etr_week_pull(WEEK, dir = tmp, quiet = TRUE), error = function(e) e)
  if (inherits(f, "error")) return(list(code = 3, why = paste("ETR pull failed:", conditionMessage(f))))
  p <- read.csv(f, check.names = FALSE)
  if (length(sl)) {   # ETR still on last week? compare its matchups with Sleeper's for this week
    sm <- bind_rows(lapply(sl, function(x) if (length(x$team) && length(x$opponent) && identical(x$player$position, "QB"))
      tibble(nfl = nt(x$team), o = nt(sub("^@", "", x$opponent))))) %>% distinct()
    em <- p %>% transmute(nfl = nt(Team), o = nt(sub("^@", "", Opp))) %>% distinct()
    chk <- inner_join(sm, em, by = "nfl")
    if (!nrow(chk) || mean(chk$o.x == chk$o.y) < 0.6) return(list(code = 2, why = sprintf("ETR projections are not on week %d matchups yet", WEEK)))
  } else cat("Sleeper check unavailable, skipping the week check\n")
  list(code = 0, f = f, p = p)
}
r <- etr_try()
if (r$code == 0) {
  f <- r$f; p <- r$p
  dir.create("data/etr", recursive = TRUE, showWarnings = FALSE)
  etr <- sprintf("data/etr/ETR_Week%d_Projections_%s.csv", WEEK, format(now_et, "%m%d_%H%M"))
  file.copy(f, etr, overwrite = TRUE)
  file.copy(f, file.path(FANTASY, sprintf("ETR_Week%d_Projections.csv", WEEK)), overwrite = TRUE)
  cat(sprintf("ETR week %d: %d rows, %d teams -> %s\n", WEEK, nrow(p), length(unique(p$Team)), etr))
} else if (!FALLBACK) {
  quit_with(r$code, r$why)
} else {
  # Fallback: Rotowire PPR medians from Sleeper, written in ETR's CSV layout so make_preview reads it unchanged
  cat(r$why, "- falling back to Rotowire (Sleeper)\n")
  if (!length(sl)) quit_with(3, r$why, "and Sleeper/Rotowire projections are unavailable")
  SKILL <- c("QB", "RB", "WR", "TE"); CEIL_DEF <- c(QB = 1.45, RB = 1.75, WR = 1.85, TE = 1.9)
  rw <- bind_rows(lapply(sl, function(x) {
    pp <- x$stats$pts_ppr; if (is.null(pp) || is.null(x$player$position)) return(NULL)
    tibble(Player = paste(x$player$first_name, x$player$last_name), Pos = x$player$position, Team = nt(x$team %||% ""),
           Opp = x$opponent %||% "", med = round(as.numeric(pp), 1))
  })) %>% filter(Pos %in% SKILL, Team != "", med > 0) %>% mutate(k = cpn(Player)) %>% distinct(k, Pos, .keep_all = TRUE)
  prev <- file.path(FANTASY, sprintf("ETR_Week%d_Projections.csv", WEEK - 1))
  cr <- if (file.exists(prev)) read.csv(prev, check.names = FALSE) %>% filter(Pos %in% SKILL, `DK Proj` > 3) %>%
    transmute(k = cpn(Player), Pos, cr = pmin(pmax(`DK Ceiling` / `DK Proj`, 1.2), 3)) %>% distinct(k, Pos, .keep_all = TRUE) else
    tibble(k = character(), Pos = character(), cr = numeric())
  rw <- rw %>% left_join(cr, by = c("k", "Pos")) %>% mutate(ceil = round(med * coalesce(cr, CEIL_DEF[Pos]), 1))
  out <- rw %>% transmute(Player, Pos, Team, Opp, `DK Proj` = med, `Full PPR Proj` = med, `DK Ceiling` = ceil, Slate = "Main")
  dir.create("data/rw", recursive = TRUE, showWarnings = FALSE)
  etr <- sprintf("data/rw/RW_Week%d_Projections_%s.csv", WEEK, format(now_et, "%m%d_%H%M"))
  write.csv(out, etr, row.names = FALSE)
  SRC <- "Rotowire"; Sys.setenv(F1C_PROJ_SRC = "Rotowire")
  cat(sprintf("Rotowire week %d: %d players, %d teams -> %s\n", WEEK, nrow(out), length(unique(out$Team)), etr))
}

# ---- 1b. GTS sim for TNF (Thursday pre-kickoff run) ----
if ("--gts" %in% args) {
  thu <- as.Date("2026-09-10") + 7 * (WEEK - 1)
  tnf <- list.files("C:/Users/astuk/OneDrive/Documents/GTS/NFL/slates", pattern = sprintf("^%s_TNF_.*\\.xlsx$", thu), full.names = TRUE)
  if (!length(tnf)) cat("GTS: no TNF sheet for", format(thu), "- TNF stays on ETR\n") else {
    tnf <- tnf[which.max(file.mtime(tnf))]
    cat("GTS: simming", basename(tnf), "(", format(file.mtime(tnf), "%a %I:%M %p"), ")\n")
    if (system2(RSCRIPT, c("make_gts.R", WEEK, sub("\\.xlsx$", "", basename(tnf)), "--sims", "50000")) != 0) quit_with(4, "make_gts.R failed")
  }
}

# ---- 2. sims ----
if (system2(RSCRIPT, c("make_preview.R", WEEK, shQuote(etr), if ("--no-push" %in% args) "--no-push")) != 0) quit_with(4, "make_preview.R failed")
# make_preview.R is the one sim for both pages (19 Nov 2026): it also writes and pushes the live page from this run.
sim_csv <- sprintf("cards/week-%02d-sim.csv", WEEK)
kick <- as.POSIXct(paste(as.Date("2026-09-10") + 7 * (WEEK - 1), "20:15"), tz = "America/New_York")   # Thursday kickoff
if (Sys.time() < kick) { file.copy(sim_csv, sprintf("cards/week-%02d-sim-preview.csv", WEEK), overwrite = TRUE); cat("Saved pre-kickoff snapshot\n") }

# ---- 3. site ----
if (system2(RSCRIPT, c("make_report.R", WEEK - 1, "--render-only")) != 0) quit_with(4, "make_report.R failed")
if (!"--no-push" %in% args) {
  system2("git", c("add", "docs", "img", "data/cache", "weeks", "archive.qmd"))   # not -A: never sweep stray files into the public repo
  Sys.setenv(HOME = Sys.getenv("USERPROFILE"))   # R sets HOME to Documents on Windows, so git misses ~/.gitconfig (no identity)
  if (system2("git", c("commit", "-q", "-m", shQuote(sprintf("Week %d sims: %s (auto)", WEEK, paste0(SRC, if ("--gts" %in% args) " + GTS TNF" else ""))))) != 0) quit_with(4, "git commit failed")
  if (system2("git", c("push", "-q")) != 0) quit_with(4, "git push failed")
}
s <- read.csv(sim_csv, check.names = FALSE)
print(s %>% select(any_of(c("Name", "proj", "win", "top3", "top6", "expF1"))) %>% head(12), row.names = FALSE)
quit_with(0, sprintf("week %d sims published from %s (%s)", WEEK, basename(etr), SRC))
