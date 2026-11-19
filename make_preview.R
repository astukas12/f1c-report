# Build the week-preview card:  Rscript make_preview.R 3
# Output: cards/week-03-preview.png (1080x1350)
#
# Needs ETR's full-week projections in ~/Downloads — download the CSV from
# establishtherun.com/fantasy-point-projections/ first (the page is paywalled).
# Pass a path as the second argument to use a specific file.

#
# ONE sim for both pages (19 Nov 2026): this run also writes the live page's data (R/live.R) from the same draws,
# projection file, GTS sims and rosters. --no-live skips that (local only). Publishing (git push of both repos) is
# publish_sims.R's job; auto_sims.R and live_refresh.R call it. Default projections: newest data/etr ETR file for the
# week, else newest data/rw Rotowire file, else the newest ETR download in ~/Downloads.

args <- commandArgs(trailingOnly = TRUE)
# --final PHI,JAX locks those NFL teams to their current MFL scores when MFL's game clock lags
fi <- which(args == "--final")
FINAL <- if (length(fi)) strsplit(args[fi + 1], ",")[[1]] else character()
if (length(fi)) args <- args[-c(fi, fi + 1)]
NOLIVE <- "--no-live" %in% args; NOPUSH <- "--no-push" %in% args; args <- setdiff(args, c("--no-live", "--no-push"))
setwd(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))))

source("R/data.R")
source("R/ui.R")
source("R/card.R")
source("R/sim.R")
source("R/live.R")

SHARE_H <- 760   # share image height in CSS px (PNG is 2x)

WEEK <- if (length(args)) as.integer(args[1]) else stop("Pass the week number, e.g. Rscript make_preview.R 3")
newest <- function(dir, pat) { f <- list.files(dir, pat, full.names = TRUE); if (length(f)) f[which.max(file.mtime(f))] }
pf <- if (length(args) > 1) args[2] else newest("data/etr", sprintf("^ETR_Week%d_.*\\.csv$", WEEK)) %||%
  newest("data/rw", sprintf("^RW_Week%d_.*\\.csv$", WEEK))
PROJ <- if (is.null(pf)) read_etr() else read_etr(pf)
RUN  <- paste0(basename(attr(PROJ, "file")), " · run ", format(as.POSIXlt(Sys.time(), tz = "America/New_York"), "%a %b %d %I:%M %p ET"))

message("Projections: ", basename(attr(PROJ, "file")), " (", format(attr(PROJ, "updated"), "%a %b %d %I:%M %p"), ")")

GTS   <- load_gts(WEEK)   # data/gts/week-NN-gts.rds from make_gts.R, if it has been run
if (!is.null(GTS)) message("GTS: ", length(attr(GTS, "games")), " games, ", format(attr(GTS, "n_sims"), big.mark = ","),
                           " sims (", format(attr(GTS, "built"), "%a %I:%M %p"), "): ", paste(attr(GTS, "games"), collapse = " "))

lg    <- load_league()
board <- sim_board(lg, WEEK, PROJ, GTS, final = FINAL)
sim   <- sim_week(lg, WEEK, board, gts = GTS, keep = if (NOLIVE) 0 else N_LIVE)

done    <- sort(unique(board$nfl[board$done]))
missing <- board %>% filter(med == 0, !done) %>% nrow()
note <- paste0(format(attr(PROJ, "updated"), "%a %b %e, %l:%M %p"), " · ",
               if (length(done)) paste0(paste(done, collapse = " "), " final") else "no games played")

dir.create("cards", showWarnings = FALSE)
out <- sprintf("cards/week-%02d-preview.png", WEEK)
shoot(odds_card(sim, WEEK, note), out)

print(as.data.frame(sim %>% select(Pos, Name, proj, mean, win, top3, top6, expF1)), row.names = FALSE)
message("\nCard written: ", out)
if (missing > 0) message(missing, " rostered players have no ETR projection (out / inactive) — they score 0.")
# Sources for the page note: GTS for its games, ETR for the rest (named by slot: TNF / SNF / MNF / London)
etr_left <- board %>% filter(src == "etr", med > 0) %>% distinct(nfl, slate)
slot <- c(Thursday = "TNF", Sunday = "SNF", Monday = "MNF", Other = "London", Main = "Sunday")
etr_lbl <- unique(unname(slot[etr_left$slate]))
PSRC <- Sys.getenv("F1C_PROJ_SRC", if (grepl("^RW_", basename(attr(PROJ, "file")))) "Rotowire" else "ETR")
sim$sources <- if (is.null(GTS) || !any(board$src == "gts")) paste(PSRC, "projections") else
  paste0("GTS sims", if (length(etr_lbl)) paste0(" · ", PSRC, " for ", paste(etr_lbl[order(match(etr_lbl, slot))], collapse = ", ")) else "")
if (!is.null(GTS)) {
  um <- board %>% filter(src == "gts", is.na(gkey)) %>% arrange(desc(med))
  message("GTS: ", sum(board$src == "gts" & !is.na(board$gkey)), " rostered players matched; ", nrow(um), " rostered in GTS games but not in GTS",
          if (nrow(um)) paste0(" (score 0): ", paste0(um$Player, " ", um$nfl, " [ETR ", um$med, "]", collapse = ", ")) else "")
}
sim$final <- if (length(done) > 4) sprintf("%d games", length(done) %/% 2) else paste(done, collapse = " ")   # "10 games final" once the list gets long   # NFL teams already final, shown on the page and share image
sim$run <- RUN   # shown on both pages, so a mismatch is visible
write.csv(sim, sprintf("cards/week-%02d-sim.csv", WEEK), row.names = FALSE)
# Pre-kickoff snapshot (Projected vs Actual in the weekly report): any run before Thursday kickoff refreshes it
kick <- as.POSIXct(paste(as.Date("2026-09-10") + 7 * (WEEK - 1), "20:15"), tz = "America/New_York")
if (Sys.time() < kick) file.copy(sprintf("cards/week-%02d-sim.csv", WEEK), sprintf("cards/week-%02d-sim-preview.csv", WEEK), overwrite = TRUE)
if (!NOLIVE) {
  ok <- tryCatch({ write_live(WEEK, board, sim, PROJ, GTS, lg, RUN, PSRC); TRUE },
                 error = function(e) { message("WARNING: live page data NOT written (", conditionMessage(e), ") - it is now behind the Sim Center"); FALSE })
  if (ok && !NOPUSH) push_live(sprintf("Week %d sims: %s", WEEK, RUN))
}

# Shareable sims image (table + GTS ad), also published with the site at img/week-NN-sims.png
source("R/brand.R")
share <- sprintf("cards/week-%02d-sims-share.png", WEEK)
shoot(sims_share_card(sim, WEEK, file.mtime(sprintf("cards/week-%02d-sim.csv", WEEK)), h = SHARE_H), share, h = SHARE_H)
dir.create("img", showWarnings = FALSE); dir.create("docs/img", showWarnings = FALSE)
for (d in c("img", "docs/img")) file.copy(share, sprintf("%s/week-%02d-sims.png", d, WEEK), overwrite = TRUE)
message("Share image: ", share, " (+ img/, docs/img/)")
