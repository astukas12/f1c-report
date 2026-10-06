# Week preview sim — projected finish order and odds off ETR's full-week projections.
#
# ETR's page is paywalled, so the CSV has to be downloaded by hand from
# establishtherun.com/fantasy-point-projections/ ("Download CSV"). We take the
# newest "NFL Weekly Projections*.csv" sitting in ~/Downloads.
#
# Games already played are locked to their actual MFL scores; everything else is
# simulated lognormal with median = ETR projection and 90th pct = ETR ceiling.
# Players are independent, so stacked lineups are slightly understated.

suppressPackageStartupMessages({library(dplyr); library(stringr); library(tidyr); library(ffscrapr)})

SIM_N    <- 20000
SIM_SLOTS <- list(c("QB", 1), c("RB", 2), c("WR", 3), c("TE", 1))
SIM_FLEX <- 2
SKILL    <- c("QB", "RB", "WR", "TE")
Z90      <- 1.2816
SIG_DEF  <- 0.6
F1_WEEK  <- c(15, 10, 7, 4, 2, 1, rep(0, 6))

# MFL nicknames vs ETR's listing: exact name first, then last name + pos + NFL team.
# Without the fallback, Kenny/Kenneth Gainwell and Chig/Chigoziem Okonkwo look unrostered.
sim_key <- function(x) {
  x <- gsub(" Jr[.]?$| Sr[.]?$| II$| III$| IV$| V$", "", x, ignore.case = TRUE)
  trimws(tolower(gsub("[^A-Za-z ]", "", x)))
}

newest_etr <- function() {
  f <- list.files(file.path(Sys.getenv("USERPROFILE"), "Downloads"),
                  "^NFL Weekly Projections.*\\.csv$", full.names = TRUE)
  if (!length(f)) stop("No 'NFL Weekly Projections*.csv' in Downloads — download it from ETR first.")
  f[which.max(file.mtime(f))]
}

read_etr <- function(path = NULL) {
  path <- path %||% newest_etr()
  p <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE) %>%
    filter(Pos %in% SKILL) %>%
    transmute(Player, pos = Pos, nfl = norm_team(Team), Opp, slate = Slate,
              med = as.numeric(`Full PPR Proj`),
              dk  = as.numeric(`DK Proj`),
              dkc = as.numeric(`DK Ceiling`)) %>%
    # ETR publishes only a DK ceiling; rescale it to the league's PPR line
    mutate(ceil = ifelse(dk > 0, round(dkc * med / dk, 1), 0),
           k = sim_key(Player), lk = sim_key(word(Player, -1)))
  attr(p, "file") <- path
  attr(p, "updated") <- file.mtime(path)
  p
}

# Best-ball optimal lineup: returns the slot each player fills, NA for bench.
sim_fill <- function(v, pos) {
  o <- order(-v); used <- rep(FALSE, length(v)); slot <- rep(NA_character_, length(v))
  for (s in SIM_SLOTS) {
    i <- o[pos[o] == s[1] & !used[o]][seq_len(as.integer(s[2]))]
    i <- i[!is.na(i)]; used[i] <- TRUE; slot[i] <- s[1]
  }
  i <- o[pos[o] %in% c("RB", "WR", "TE") & !used[o]][seq_len(SIM_FLEX)]
  i <- i[!is.na(i)]; used[i] <- TRUE; slot[i] <- "FLEX"
  slot
}

# Rosters joined to projections, with actual scores locked in for finished games.
sim_board <- function(lg, week, proj = NULL) {
  proj <- proj %||% read_etr()
  ros <- ff_rosters(lg$conn) %>% filter(pos %in% SKILL) %>%
    mutate(ln = str_trim(str_extract(player_name, "^[^,]+")),
           fn = str_trim(str_extract(player_name, "(?<=,).*")),
           Player = if_else(is.na(fn), player_name, paste(fn, ln)),
           player_id = as.character(player_id),
           k = sim_key(Player), lk = sim_key(word(ln, -1)), nfl = norm_team(team))

  ps <- mfl_getendpoint(lg$conn, "playerScores", W = week)$content$playerScores$playerScore
  if (!is.null(ps) && !is.null(names(ps))) ps <- list(ps)
  act <- tibble(player_id = character(), actual = numeric())
  if (length(ps)) act <- bind_rows(lapply(ps, function(z)
    tibble(player_id = as.character(z$id), actual = as.numeric(z$score))))
  played <- act %>% left_join(lg$players %>% select(player_id, nfl), by = "player_id") %>%
    filter(!is.na(nfl), actual != 0) %>% pull(nfl) %>% unique()

  ex <- ros %>% inner_join(proj %>% select(k, pos, med, ceil, slate, Opp), by = c("k", "pos"))
  fb <- ros %>% filter(!player_id %in% ex$player_id) %>%
    inner_join(proj %>% select(lk, pos, nfl, med, ceil, slate, Opp), by = c("lk", "pos", "nfl")) %>%
    group_by(player_id) %>% filter(n() == 1) %>% ungroup()
  miss <- ros %>% filter(!player_id %in% c(ex$player_id, fb$player_id)) %>%
    mutate(med = 0, ceil = 0, slate = "OUT", Opp = "")

  bind_rows(ex, fb, miss) %>%
    left_join(act, by = "player_id") %>%
    mutate(done   = nfl %in% played,
           actual = ifelse(done, coalesce(actual, 0), NA_real_),
           value  = ifelse(done, actual, med),
           sigma  = ifelse(med > 0 & ceil > med, log(ceil / med) / Z90, SIG_DEF)) %>%
    group_by(franchise_id) %>% mutate(slot = sim_fill(value, pos)) %>% ungroup() %>%
    left_join(lg$teams, by = "franchise_id")
}

sim_week <- function(lg, week, board = NULL, nsim = SIM_N, seed = 42) {
  board <- board %||% sim_board(lg, week)
  set.seed(seed)
  ids <- sort(unique(board$franchise_id))
  scores <- sapply(ids, function(fid) {
    d <- board %>% filter(franchise_id == fid)
    m <- matrix(0, nsim, nrow(d))
    live <- which(!d$done & d$med > 0)
    if (length(live))
      m[, live] <- matrix(d$med[live], nsim, length(live), byrow = TRUE) *
        exp(sweep(matrix(rnorm(nsim * length(live)), nsim), 2, d$sigma[live], "*") -
            matrix(d$sigma[live]^2 / 2, nsim, length(live), byrow = TRUE))
    fixed <- which(d$done)
    if (length(fixed)) m[, fixed] <- matrix(d$actual[fixed], nsim, length(fixed), byrow = TRUE)
    apply(m, 1, function(v) sum(v[!is.na(sim_fill(v, d$pos))]))
  })
  rk <- t(apply(-scores, 1, rank, ties.method = "first"))

  det <- board %>% filter(!is.na(slot)) %>% group_by(franchise_id) %>%
    summarise(proj = sum(value), banked = sum(value[done]), .groups = "drop")

  tibble(franchise_id = ids,
         mean  = round(colMeans(scores), 1),
         win   = round(100 * colMeans(rk == 1), 1),
         top3  = round(100 * colMeans(rk <= 3), 1),
         top6  = round(100 * colMeans(rk <= 6), 1),
         expF1 = round(colMeans(matrix(F1_WEEK[rk], nsim)), 2)) %>%
    left_join(det, by = "franchise_id") %>%
    left_join(lg$teams, by = "franchise_id") %>%
    arrange(desc(expF1)) %>%
    mutate(Pos = row_number()) %>%
    select(Pos, franchise_id, Name, Franchise, proj, banked, mean, win, top3, top6, expF1)
}
