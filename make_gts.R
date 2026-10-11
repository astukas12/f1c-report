# Run Andrew's GTS NFL engine on this week's slate sheets and save every player's simulated F1C points.
#   Rscript make_gts.R 5 2026-10-11_W5_SUN 2026-10-11_SUN_PHI_JAX [--sims 10000]
# Writes data/gts/week-05-gts.rds (gitignored: raw sim output never goes in the public repo).
#
# The engine (GitHub/GTS/SimApp/nfl_engine.R) is SOURCED READ-ONLY and the sheets are only read.
# Each game is simmed on its own (as run_nfl_classic_simulation does), so one SimID is one world
# for every player in that game: QB, his receivers and the backs stay correlated.

args <- commandArgs(trailingOnly = TRUE)
setwd(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))))
F1C <- getwd()
WEEK   <- as.integer(args[1])
ni     <- which(args == "--sims")
N_SIMS <- if (length(ni)) as.integer(args[ni + 1]) else 10000L
gi_   <- which(args == "--games")
ONLY   <- if (length(gi_)) strsplit(args[gi_ + 1], ",")[[1]] else NULL   # e.g. --games BAL@ATL
ADD    <- "--add" %in% args   # merge into the existing file, replacing only the games simmed now
SHEETS <- setdiff(args[-1], c("--sims", if (length(ni)) args[ni + 1], "--games", if (length(gi_)) args[gi_ + 1], "--add"))
if (is.na(WEEK) || !length(SHEETS)) stop("Usage: Rscript make_gts.R <week> <slate_id> [<slate_id> ...] [--sims N]")

GTS_SLATES <- "C:/Users/astuk/OneDrive/Documents/GTS/NFL/slates"
SIMAPP     <- "C:/Users/astuk/OneDrive/Documents/GitHub/GTS/SimApp"

suppressPackageStartupMessages(library(data.table))
setwd(SIMAPP)                      # the app's own nfl_data/, exactly what SimApp sims with
source("nfl_engine.R")

# F1C (MFL league 43580) scoring for QB/RB/WR/TE: full PPR, 0.1/rush+rec yd, 0.04/pass yd,
# 6 rush/rec/return TD, 4 pass TD, -1 INT, -1 fumble lost. (2-pt conversions aren't in the engine.)
f1c_points <- function(A) {
  g <- function(nm) if (nm %in% names(A)) nfl_num(A[[nm]]) else 0
  g("rec") + 0.1 * g("ryds") + 6 * g("rtd") + 0.1 * g("cyds") + 6 * g("ctd") +
    0.04 * g("pyds") + 4 * g("ptd") - g("pint") + 6 * g("rettd") - g("fum")
}

out <- list(); games <- character(); info <- list()
for (s in SHEETS) {
  xlsx <- file.path(GTS_SLATES, paste0(s, ".xlsx"))
  if (!file.exists(xlsx)) stop("No sheet: ", xlsx)
  inp <- tryCatch(read_nfl_input(xlsx, slate = "classic_main"), error = function(e) read_nfl_input(xlsx))
  G   <- as.data.table(inp$game)
  if (!"start_order" %in% names(G) || anyNA(G$start_order)) G[, start_order := seq_len(.N)]
  info[[s]] <- list(file = xlsx, mtime = file.mtime(xlsx), games = paste(G$away, G$home, sep = "@"))
  for (i in seq_len(nrow(G))) {
    gi <- G[i]; tms <- c(gi$away, gi$home); key <- paste0(gi$away, "@", gi$home)
    if (!is.null(ONLY) && !key %in% ONLY) next
    t0 <- Sys.time()
    sub <- list(game = gi, team = as.data.table(inp$team)[team %in% tms],
                players = as.data.table(inp$players)[team %in% tms], projections = inp$projections)
    res <- run_nfl_simulation(sub, n_sims = N_SIMS, keep_components = TRUE, .slate_type = tolower(gi$slate_type %||% "classic"),
                              seed = 13L + length(games))
    A <- as.data.table(res$sim_components)
    if ("is_dst" %in% names(A)) A <- A[!(is_dst %in% TRUE)]
    out[[key]] <- A[, .(game = key, SimID = as.integer(SimID), player, team, pts = round(f1c_points(.SD), 2))]
    games <- c(games, key)
    message(sprintf("GTS %s: %d players x %d sims (%.0fs)", key, uniqueN(A$player), N_SIMS,
                    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    rm(res, A); gc(verbose = FALSE)
  }
}

setwd(F1C)
dir.create("data/gts", recursive = TRUE, showWarnings = FALSE)
D <- rbindlist(out)
f <- sprintf("data/gts/week-%02d-gts.rds", WEEK)
if (ADD && file.exists(f)) {   # keep the other games' sims from the earlier run
  old <- readRDS(f); keep <- setdiff(attr(old, "games"), games)
  D <- rbindlist(list(as.data.table(old)[game %in% keep], D), use.names = TRUE)
  info <- c(attr(old, "sheets"), info); games <- c(keep, games)
  message("Kept from the earlier run: ", paste(keep, collapse = " "))
}
attr(D, "n_sims") <- N_SIMS; attr(D, "games") <- games; attr(D, "sheets") <- info; attr(D, "built") <- Sys.time()
saveRDS(D, f)
message("Saved ", f, ": ", length(games), " games, ", N_SIMS, " sims, ", uniqueN(D[, paste(player, team)]), " players")
