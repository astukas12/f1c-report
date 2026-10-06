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
shoot(odds_card(sim, WEEK, MY_TEAM, note), out)

print(as.data.frame(sim %>% select(Pos, Name, proj, mean, win, top3, top6, expF1)), row.names = FALSE)
message("\nCard written: ", out)
if (missing > 0) message(missing, " rostered players have no ETR projection (out / inactive) — they score 0.")
write.csv(sim, sprintf("cards/week-%02d-sim.csv", WEEK), row.names = FALSE)
