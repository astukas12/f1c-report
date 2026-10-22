# Automated weekly sims on fresh ETR:  Rscript auto_sims.R [--week N] [--no-push]
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
tmp <- file.path(tempdir(), "etr"); dir.create(tmp, showWarnings = FALSE)
f <- tryCatch(etr_week_pull(WEEK, dir = tmp, quiet = TRUE), error = function(e) quit_with(3, "ETR pull failed:", conditionMessage(e)))
p <- read.csv(f, check.names = FALSE)

TMAP <- c(SFO="SF", LAR="LA", NEP="NE", GBP="GB", TBB="TB", LVR="LV", KCC="KC", NOS="NO", JAC="JAX", WSH="WAS")
nt <- function(t) { t <- toupper(t); ifelse(t %in% names(TMAP), TMAP[t], t) }
sl <- tryCatch(fromJSON(rawToChar(curl_fetch_memory(sprintf(
  "https://api.sleeper.com/projections/nfl/2026/%d?season_type=regular&position[]=QB", WEEK),
  handle = new_handle(useragent = "Mozilla/5.0", timeout = 40))$content), simplifyVector = FALSE), error = function(e) NULL)
if (length(sl)) {   # ETR still on last week? compare its matchups with Sleeper's for this week
  sm <- bind_rows(lapply(sl, function(x) if (length(x$team) && length(x$opponent)) tibble(nfl = nt(x$team), o = nt(sub("^@", "", x$opponent))))) %>% distinct()
  em <- p %>% transmute(nfl = nt(Team), o = nt(sub("^@", "", Opp))) %>% distinct()
  chk <- inner_join(sm, em, by = "nfl")
  if (!nrow(chk) || mean(chk$o.x == chk$o.y) < 0.6) quit_with(2, sprintf("ETR projections are not on week %d matchups yet", WEEK))
} else cat("Sleeper check unavailable, skipping the week check\n")

dir.create("data/etr", recursive = TRUE, showWarnings = FALSE)
etr <- sprintf("data/etr/ETR_Week%d_Projections_%s.csv", WEEK, format(now_et, "%m%d_%H%M"))
file.copy(f, etr, overwrite = TRUE)
file.copy(f, file.path(FANTASY, sprintf("ETR_Week%d_Projections.csv", WEEK)), overwrite = TRUE)
cat(sprintf("ETR week %d: %d rows, %d teams -> %s\n", WEEK, nrow(p), length(unique(p$Team)), etr))

# ---- 2. sims ----
if (system2(RSCRIPT, c("make_preview.R", WEEK, shQuote(etr))) != 0) quit_with(4, "make_preview.R failed")
sim_csv <- sprintf("cards/week-%02d-sim.csv", WEEK)
kick <- as.POSIXct(paste(as.Date("2026-09-10") + 7 * (WEEK - 1), "20:15"), tz = "America/New_York")   # Thursday kickoff
if (Sys.time() < kick) { file.copy(sim_csv, sprintf("cards/week-%02d-sim-preview.csv", WEEK), overwrite = TRUE); cat("Saved pre-kickoff snapshot\n") }

# ---- 3. site ----
if (system2(RSCRIPT, c("make_report.R", WEEK - 1, "--render-only")) != 0) quit_with(4, "make_report.R failed")
if (!"--no-push" %in% args) {
  system2("git", c("add", "-A"))
  system2("git", c("commit", "-q", "-m", shQuote(sprintf("Week %d sims: ETR (auto)", WEEK))))
  if (system2("git", c("push", "-q")) != 0) quit_with(4, "git push failed")
}
s <- read.csv(sim_csv, check.names = FALSE)
print(s %>% select(any_of(c("Name", "proj", "win", "top3", "top6", "expF1"))) %>% head(12), row.names = FALSE)
quit_with(0, sprintf("week %d sims published from %s", WEEK, basename(etr)))
