# Build the WhatsApp cards for a week:  Rscript make_cards.R 1
# Output: cards/week-01-standings.png, cards/week-01-team.png (1080x1350)

args <- commandArgs(trailingOnly = TRUE)
setwd(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))))

source("R/data.R")
source("R/ui.R")
source("R/card.R")

WEEK <- if (length(args)) as.integer(args[1]) else stop("Pass the week number, e.g. Rscript make_cards.R 1")

lg <- load_league()
sc <- load_scores(lg, WEEK)
wk <- sc$results %>% filter(week == WEEK)
st <- build_standings(sc$results, WEEK)
pl <- sc$players %>% filter(week == WEEK) %>% left_join(lg$teams, by = "franchise_id")

dir.create("cards", showWarnings = FALSE)
stem <- sprintf("cards/week-%02d", WEEK)
shoot(standings_card(st, wk, WEEK, MY_TEAM), paste0(stem, "-standings.png"))
shoot(lineup_card(team_of_week(pl), WEEK, max(wk$points), sum(wk$pending)), paste0(stem, "-team.png"))
shoot(cap_card(cap_table(lg, st), WEEK, MY_TEAM), paste0(stem, "-cap.png"))
message("Cards written: ", stem, "-standings.png, ", stem, "-team.png, ", stem, "-cap.png")
if (sum(wk$pending) > 0) message("PROVISIONAL: ", sum(wk$pending), " players still to play. Rerun once the week is final.")

# Final cards also go on the site (img/ and docs/img/; cards/ is git-ignored), e.g.
# https://astukas12.github.io/f1c-report/img/week-05-standings.png once pushed
if (sum(wk$pending) == 0) for (k in c("standings", "team", "cap")) for (d in c("img", "docs/img")) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  file.copy(sprintf("%s-%s.png", stem, k), file.path(d, sprintf("week-%02d-%s.png", WEEK, k)), overwrite = TRUE)
}
