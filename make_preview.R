# Build the week-preview card:  Rscript make_preview.R 3
# Output: cards/week-03-preview.png (1080x1350)
#
# Needs ETR's full-week projections in ~/Downloads — download the CSV from
# establishtherun.com/fantasy-point-projections/ first (the page is paywalled).
# Pass a path as the second argument to use a specific file.

args <- commandArgs(trailingOnly = TRUE)
# --final PHI,JAX locks those NFL teams to their current MFL scores when MFL's game clock lags
fi <- which(args == "--final")
FINAL <- if (length(fi)) strsplit(args[fi + 1], ",")[[1]] else character()
if (length(fi)) args <- args[-c(fi, fi + 1)]
setwd(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))))

source("R/data.R")
source("R/ui.R")
source("R/card.R")
source("R/sim.R")

SHARE_H <- 760   # share image height in CSS px (PNG is 2x)

WEEK <- if (length(args)) as.integer(args[1]) else stop("Pass the week number, e.g. Rscript make_preview.R 3")
PROJ <- if (length(args) > 1) read_etr(args[2]) else read_etr()

message("Projections: ", basename(attr(PROJ, "file")), " (", format(attr(PROJ, "updated"), "%a %b %d %I:%M %p"), ")")

GTS   <- load_gts(WEEK)   # data/gts/week-NN-gts.rds from make_gts.R, if it has been run
if (!is.null(GTS)) message("GTS: ", length(attr(GTS, "games")), " games, ", format(attr(GTS, "n_sims"), big.mark = ","),
                           " sims (", format(attr(GTS, "built"), "%a %I:%M %p"), "): ", paste(attr(GTS, "games"), collapse = " "))

lg    <- load_league()
board <- sim_board(lg, WEEK, PROJ, GTS, final = FINAL)
sim   <- sim_week(lg, WEEK, board, gts = GTS)

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
sim$sources <- if (is.null(GTS) || !any(board$src == "gts")) "ETR projections" else
  paste0("GTS sims", if (length(etr_lbl)) paste0(" · ETR for ", paste(etr_lbl[order(match(etr_lbl, slot))], collapse = ", ")) else "")
if (!is.null(GTS)) {
  um <- board %>% filter(src == "gts", is.na(gkey)) %>% arrange(desc(med))
  message("GTS: ", sum(board$src == "gts" & !is.na(board$gkey)), " rostered players matched; ", nrow(um), " rostered in GTS games but not in GTS",
          if (nrow(um)) paste0(" (score 0): ", paste0(um$Player, " ", um$nfl, " [ETR ", um$med, "]", collapse = ", ")) else "")
}
sim$final <- paste(done, collapse = " ")   # NFL teams already final, shown on the page and share image
write.csv(sim, sprintf("cards/week-%02d-sim.csv", WEEK), row.names = FALSE)

# Shareable sims image (table + GTS ad), also published with the site at img/week-NN-sims.png
source("R/brand.R")
share <- sprintf("cards/week-%02d-sims-share.png", WEEK)
shoot(sims_share_card(sim, WEEK, file.mtime(sprintf("cards/week-%02d-sim.csv", WEEK)), h = SHARE_H), share, h = SHARE_H)
dir.create("img", showWarnings = FALSE); dir.create("docs/img", showWarnings = FALSE)
for (d in c("img", "docs/img")) file.copy(share, sprintf("%s/week-%02d-sims.png", d, WEEK), overwrite = TRUE)
message("Share image: ", share, " (+ img/, docs/img/)")
