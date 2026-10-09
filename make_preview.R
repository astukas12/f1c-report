# Build the week-preview card:  Rscript make_preview.R 3
# Output: cards/week-03-preview.png (1080x1350)
#
# Needs ETR's full-week projections in ~/Downloads — download the CSV from
# establishtherun.com/fantasy-point-projections/ first (the page is paywalled).
# Pass a path as the second argument to use a specific file.

args <- commandArgs(trailingOnly = TRUE)
setwd(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))))

source("R/data.R")
source("R/ui.R")
source("R/card.R")
source("R/sim.R")

SHARE_H <- 760   # share image height in CSS px (PNG is 2x)

WEEK <- if (length(args)) as.integer(args[1]) else stop("Pass the week number, e.g. Rscript make_preview.R 3")
PROJ <- if (length(args) > 1) read_etr(args[2]) else read_etr()

message("Projections: ", basename(attr(PROJ, "file")), " (", format(attr(PROJ, "updated"), "%a %b %d %I:%M %p"), ")")

lg    <- load_league()
board <- sim_board(lg, WEEK, PROJ)
sim   <- sim_week(lg, WEEK, board)

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
sim$final <- paste(done, collapse = " ")   # NFL teams already final, shown on the page and share image
write.csv(sim, sprintf("cards/week-%02d-sim.csv", WEEK), row.names = FALSE)

# Shareable sims image (table + GTS ad), also published with the site at img/week-NN-sims.png
source("R/brand.R")
share <- sprintf("cards/week-%02d-sims-share.png", WEEK)
shoot(sims_share_card(sim, WEEK, file.mtime(sprintf("cards/week-%02d-sim.csv", WEEK)), h = SHARE_H), share, h = SHARE_H)
dir.create("img", showWarnings = FALSE); dir.create("docs/img", showWarnings = FALSE)
for (d in c("img", "docs/img")) file.copy(share, sprintf("%s/week-%02d-sims.png", d, WEEK), overwrite = TRUE)
message("Share image: ", share, " (+ img/, docs/img/)")
