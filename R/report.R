# Weekly report: data helpers, rest-of-season sim, cap market, luck index, roster-move scorecard,
# storylines and the chart theme. Calculations ported from last season's F1C_violin.Rmd onto the
# MFL data layer in data.R. Sourced by _report.qmd after data.R, ui.R and sim.R.

suppressPackageStartupMessages({library(ggplot2); library(htmltools)})

LAST_WEEK <- 18            # final F1C week (MFL endWeek)
SLOTS5    <- c("QB", "RB", "WR", "TE", "FLEX")
PRIOR_K   <- 4             # ETR preseason prior counts as this many weeks of results in the season sim
ROS_N     <- 20000
GOLD      <- "#E8C547"

# ---- data -------------------------------------------------------------------------

load_rosters <- function(lg) ff_rosters(lg$conn) %>% mutate(player_id = as.character(player_id))

# Every rostered skill player per week, with MFL's best-ball starter flag and the slot each starter filled
lineups <- function(sc, lg, ros) {
  sc$players %>%
    filter(pos %in% SKILL) %>%
    group_by(week, franchise_id) %>%
    mutate(slot = { s <- rep(NA_character_, n()); i <- which(starter)
                    if (length(i)) s[i] <- sim_fill(score[i], pos[i]); s }) %>%
    ungroup() %>%
    rename(used = starter) %>%
    left_join(lg$teams, by = "franchise_id") %>%
    left_join(ros %>% select(player_id, franchise_id, salary, contract_years), by = c("player_id", "franchise_id"))
}

weekly_lookup <- function(lg) lg$weekly %>% filter(Rank == floor(Rank)) %>% arrange(Rank)

# Socialism, two passes (F1C_violin.Rmd:1102-1117): every point over 100 is spread over the teams below
socialism <- function(total) {
  excess <- pmax(total - 100, 0)
  ss1    <- if_else(excess > 0, -excess, sum(excess) / sum(excess < 1))
  rev    <- total + ss1
  ex2    <- pmax(rev - 100, 0)
  ss2    <- if_else(ex2 > 0, -ex2, if_else(rev < 100, sum(ex2) / max(sum(rev < 100), 1), 0))
  ss1 + ss2
}

seasonal_award <- function(final_rank, lg, col = "SeasonalF1Pts") {
  s <- lg$seasonal %>% arrange(Rank)
  out <- s[[col]][match(final_rank, s$Rank)]
  out[is.na(out)] <- 0
  out
}

# ---- rest-of-season sim -----------------------------------------------------------
# Each team's true weekly mean starts from the ETR preseason roster rating (league mean + z-score x
# PRIOR_SPREAD) and is updated by its actual best-ball scores (normal-normal), so the prior fades as weeks
# accrue (about 50/50 at week 4). Week-to-week SD is the team's own, shrunk toward the league's pooled SD.
# Each sim draws every team's true mean from its posterior, then plays out the remaining weeks.
PRIOR_SPREAD <- 12   # SD of true weekly means across teams, in points (preseason)

etr_prior <- function(lg) {
  key <- function(x) tolower(gsub("[^A-Za-z]", "", x))
  etr <- read.csv("../RosterRating_ETR_2026.csv", check.names = FALSE, encoding = "UTF-8")
  etr$Name <- lg$teams$Name[match(key(etr$Team), key(lg$teams$Franchise))]
  miss <- setdiff(lg$teams$Name, etr$Name)
  if (length(miss)) warning("ETR prior missing for: ", paste(miss, collapse = ", "))
  etr %>% filter(!is.na(Name)) %>% transmute(Name, z = (Rating - mean(Rating)) / sd(Rating))
}

# Title-odds model version. v2 (6 Oct, Andrew): this season's results only, no ETR prior; each team's true
# weekly mean = its average so far +/- sd/sqrt(n), and it drifts DRIFT_SD points a week over the remaining
# weeks (injuries, trades, waivers). DRIFT_SD = 8 chosen by backtest on 2022, 2023 and 2025 (as of weeks 4, 8, 12):
# with 8, no team the model put under 1% went on to finish top 3; without drift, 4 did.
ROS_MODEL <- "v2"
DRIFT_SD  <- 8

ros_inputs <- function(res, lg, through, model = ROS_MODEL) {
  r <- res %>% filter(week <= through)
  t <- r %>% group_by(Name) %>% summarise(n = n(), m = mean(points), s = sd(points), .groups = "drop")
  pooled <- sqrt(mean(t$s^2, na.rm = TRUE))
  if (!is.finite(pooled)) pooled <- 25
  if (model == "v2") return(t %>% mutate(
    s     = coalesce(s, pooled),
    sd    = sqrt((pmax(n - 1, 0) * s^2 + PRIOR_K * pooled^2) / (pmax(n - 1, 0) + PRIOR_K)),  # weekly SD, steadied toward the league's
    mu    = m, mu_se = sd / sqrt(n), w_data = 1))
  t %>% left_join(etr_prior(lg), by = "Name") %>%
    mutate(z     = coalesce(z, 0),
           prior = mean(r$points) + z * PRIOR_SPREAD,
           s     = coalesce(s, pooled),
           sd    = sqrt((pmax(n - 1, 0) * s^2 + PRIOR_K * pooled^2) / (pmax(n - 1, 0) + PRIOR_K)),
           prec  = 1 / PRIOR_SPREAD^2 + n / sd^2,
           mu    = (prior / PRIOR_SPREAD^2 + n * m / sd^2) / prec,
           mu_se = sqrt(1 / prec),
           w_data = (n / sd^2) / prec)
}

ros_sim <- function(lu, res, lg, through, nsim = ROS_N, seed = 42, model = ROS_MODEL) {
  set.seed(seed)
  inp   <- ros_inputs(res, lg, through, model) %>% arrange(Name)
  drift <- if (model == "v2") DRIFT_SD else 0
  teams <- inp$Name
  wk_mu <- inp$mu; wk_sd <- inp$sd; mu_se <- inp$mu_se

  now <- res %>% filter(week <= through) %>% group_by(Name) %>%
    summarise(F1 = sum(F1Pts), Earn = sum(Earnings), .groups = "drop")
  f1_0 <- now$F1[match(teams, now$Name)]; earn_0 <- now$Earn[match(teams, now$Name)]
  left <- LAST_WEEK - through
  wl   <- weekly_lookup(lg)
  nt   <- length(teams)
  rowm <- function(v) matrix(v, nsim, nt, byrow = TRUE)

  F1 <- rowm(f1_0); ER <- rowm(earn_0)
  if (left > 0) {
    true_mu <- rowm(wk_mu) + matrix(rnorm(nsim * nt), nsim) * rowm(mu_se)
    for (w in seq_len(left)) {
      if (drift > 0) true_mu <- true_mu + matrix(rnorm(nsim * nt, 0, drift), nsim)
      sc <- true_mu + matrix(rnorm(nsim * nt), nsim) * rowm(wk_sd)
      rk <- t(apply(-sc, 1, rank, ties.method = "random"))
      F1 <- F1 + matrix(wl$F1Pts[rk], nsim); ER <- ER + matrix(wl$Earnings[rk], nsim)
    }
  }
  fr   <- t(apply(-F1, 1, rank, ties.method = "random"))
  seas <- matrix(seasonal_award(fr, lg), nsim)
  seE  <- matrix(seasonal_award(fr, lg, "Earnings"), nsim)
  rev  <- t(apply(F1 + seas, 1, function(x) x + socialism(x)))   # 2027 revenue = total F1 + socialism

  tibble(Name = teams, week = through, Current = f1_0, WeeklyMu = round(wk_mu, 1),
         Proj = colMeans(F1), ProjSD = apply(F1, 2, sd), ProjEarn = colMeans(ER + seE),
         Seasonal = colMeans(seas), Revenue = colMeans(rev),
         Title = 100 * colMeans(fr == 1), Top3 = 100 * colMeans(fr <= 3), Top6 = 100 * colMeans(fr <= 6),
         AvgFinish = colMeans(fr)) %>%
    arrange(desc(Title), desc(Proj))
}

# Per-week odds archive (sims/title-odds.csv). Weeks never archived are backfilled from results through that week.
ros_history <- function(lu, res, lg, week, model = ROS_MODEL) {
  f <- if (model == "v2") "sims/title-odds-v2.csv" else "sims/title-odds.csv"; dir.create("sims", showWarnings = FALSE)
  h <- if (file.exists(f)) read.csv(f, stringsAsFactors = FALSE) else NULL
  for (w in seq_len(week)) {
    live <- w == week
    if (live || is.null(h) || !w %in% h$week) {
      r <- ros_sim(lu, res, lg, w, nsim = if (live) ROS_N else 5000, model = model)
      r$source <- if (live) "live" else "backfill"
      h <- bind_rows(if (!is.null(h)) filter(h, week != w), as.data.frame(r))
    }
  }
  h <- h %>% arrange(week, desc(Title))
  write.csv(h, f, row.names = FALSE)
  h
}

# ---- luck index: actual F1 vs F1 expected from the score alone ----------------------
# Every team-week score is dropped into every week's field so far (against the other 11 scores) and
# awarded F1 points; the average is what that score is "worth". Luck = actual minus expected.
luck_table <- function(res, lg, through) {
  wl <- c(weekly_lookup(lg)$F1Pts, rep(0, 12))
  r  <- res %>% filter(week <= through)
  r$xF1 <- vapply(seq_len(nrow(r)), function(i) {
    mean(vapply(unique(r$week), function(w) {
      others <- r$points[r$week == w & r$franchise_id != r$franchise_id[i]]
      above <- sum(others > r$points[i]); tied <- sum(others == r$points[i])
      mean(wl[(above + 1):(above + 1 + tied)])
    }, numeric(1)))
  }, numeric(1))
  r$apW <- vapply(seq_len(nrow(r)), function(i) sum(r$points[r$week == r$week[i]] < r$points[i]), numeric(1))
  r %>% group_by(franchise_id, Name, Franchise) %>%
    summarise(F1 = sum(F1Pts), xF1 = sum(xF1), apW = sum(apW), apG = n() * 11, .groups = "drop") %>%
    mutate(Luck = F1 - xF1) %>% arrange(desc(Luck))
}

# ---- cap market ---------------------------------------------------------------------
cap_outlook <- function(cap, st, lg, ros_now) {
  today_rank <- st$Pos[match(cap$franchise_id, st$franchise_id)]
  tot_today  <- cap$F1Pts + seasonal_award(today_rank, lg)
  cap %>% mutate(
    SeasonalToday = tot_today - F1Pts,
    SocToday      = socialism(tot_today),
    SpaceToday    = CAP_BASE + Traded + tot_today + SocToday - Salary - CapHit,
    RevenueProj   = ros_now$Revenue[match(Name, ros_now$Name)],
    SpaceProj     = CAP_BASE + Traded + RevenueProj - Salary - CapHit) %>%
    arrange(desc(SpaceProj))
}

# Franchise tag estimate (F1C_violin.Rmd:1158): mean of the top 3 (QB/TE) or top 5 (RB/WR) salaries on 1+ year deals
franchise_tags <- function(ros) {
  ros %>% filter(coalesce(contract_years, 0) >= 1, salary > 0, pos %in% SKILL) %>%
    group_by(pos) %>% arrange(desc(salary), .by_group = TRUE) %>%
    filter(row_number() <= if_else(first(pos) %in% c("QB", "TE"), 3L, 5L)) %>%
    summarise(tag = round(mean(salary)), .groups = "drop")
}

# Expiring deals (0 years left) and no-contract pickups on today's rosters, by season points (any team)
pending_fas <- function(ros, lu, lg) {
  pts <- lu %>% group_by(player_id) %>% summarise(Pts = sum(score), Used = sum(score[used]), .groups = "drop")
  ros %>% filter(pos %in% SKILL, is.na(contract_years) | contract_years == 0) %>%
    left_join(lg$teams %>% select(franchise_id, Name), by = "franchise_id") %>%
    left_join(pts, by = "player_id") %>%
    mutate(across(c(Pts, Used), ~ replace_na(.x, 0)),
           Kind = if_else(is.na(contract_years), "pickup", "expiring")) %>%
    arrange(desc(Pts))
}

# ---- roster moves scorecard -----------------------------------------------------------
# Waiver/FA adds and trade acquisitions this season, scored by best-ball starter points for the team that
# acquired them, in the weeks after the move (MFL timestamp -> first week whose Thursday is after it).
roster_moves <- function(lu, lg) {
  tx <- ff_transactions(lg$conn)
  acq <- tx %>%
    filter((type %in% c("BBID_WAIVER", "FREE_AGENT") & type_desc == "added") | (type == "TRADE" & type_desc == "traded_for")) %>%
    transmute(player_id = as.character(player_id), franchise_id, when = as.POSIXct(timestamp),
              how = if_else(type == "TRADE", "Trade", "Waiver"), bid = bbid_spent)
  # The week the move happened in (weeks run Thursday to Wednesday). Weekly rows only exist while the
  # player sat on that roster, so this just guards against counting an earlier stint with the same team.
  wk1 <- as.POSIXct(paste(SEASON_WEEK1_THU, "20:00"), tz = "America/New_York")
  acq <- acq %>% mutate(from_week = pmax(1L, as.integer(floor(as.numeric(difftime(when, wk1, units = "days")) / 7)) + 1L))
  # A player moved twice to the same team keeps the latest move
  acq <- acq %>% group_by(player_id, franchise_id) %>% slice_max(when, n = 1, with_ties = FALSE) %>% ungroup()
  lu %>%
    inner_join(acq, by = c("player_id", "franchise_id")) %>%
    filter(week >= from_week) %>%
    group_by(player_id, player_name, pos, nfl, franchise_id, Name, how, when, bid, from_week) %>%
    summarise(Weeks = n(), Starts = sum(used), Used = sum(score[used]), Total = sum(score), .groups = "drop") %>%
    arrange(desc(Used), desc(Total))
}
SEASON_WEEK1_THU <- "2026-09-10"

# ---- storylines -----------------------------------------------------------------------
pick <- function(x, seed) { set.seed(seed); sample(x, 1) }

storylines <- function(st, wk, pl, WEEK, prev_st = NULL) {
  out <- character()
  lead <- st$F1Pts[1] - st$F1Pts[2]
  ldr <- st$Name[1]; sec <- st$Name[2]
  p <- if (!is.null(prev_st)) prev_st %>% arrange(Pos) else NULL
  prev_lead <- if (!is.null(p) && p$Name[1] == ldr) p$F1Pts[1] - p$F1Pts[2] else NA
  prev_ldr  <- if (!is.null(p)) p$Name[1] else NA
  out <- c(out, if (lead == 0) sprintf("<b>%s and %s are level</b> at the top on %s F1 points. Tiebreak on average finish keeps %s in front.", ldr, sec, fmtpt(st$F1Pts[1]), ldr)
           else if (!is.na(prev_lead) && lead > prev_lead) sprintf("<b>%s stretches the lead to %s</b> over %s (it was %s). %s", ldr, fmtpt(lead), sec, fmtpt(prev_lead),
                                                                pick(c("Somebody check the scoring settings.", "The rest of the league is playing for second.", "Comfortable is starting to look like a word for it."), WEEK))
           else if (!is.na(prev_lead)) sprintf("<b>%s still leads, but by %s</b> now, down from %s. %s is closing.", ldr, fmtpt(lead), fmtpt(prev_lead), sec)
           else if (!is.na(prev_ldr)) sprintf("<b>%s takes first from %s</b> and leads by %s. %s", ldr, prev_ldr, fmtpt(lead),
                                              pick(c("Enjoy the view.", "A one-week lead is a rumour until it lasts.", "Fourteen weeks is a long time to defend it."), WEEK))
           else sprintf("<b>%s leads after week %d</b>, %s clear of %s.", ldr, WEEK, fmtpt(lead), sec))

  wkr <- wk %>% arrange(rank)
  w1 <- wkr[1, ]
  out <- c(out, sprintf("<b>%s wins week %d</b> with %s, %s clear of %s. +15 F1, %s.", w1$Name, WEEK, fmt1(w1$points),
                        fmt1(w1$points - wkr$points[2]), wkr$Name[2], money(w1$Earnings)))

  mv <- st %>% filter(!is.na(Move), Move > 0, Name != w1$Name, Pos > 1)
  if (nrow(mv)) {
    m <- mv %>% slice_max(Move, n = 1, with_ties = FALSE)
    out <- c(out, sprintf("<b>Climber: %s</b>, up %d to %s.", m$Name, m$Move, ordinal(m$Pos)))
  }
  fall <- st %>% filter(!is.na(Move), Move < 0) %>% slice_min(Move, n = 1, with_ties = FALSE)
  if (nrow(fall) && fall$Move <= -2)
    out <- c(out, sprintf("<b>Faller: %s</b>, down %d to %s.", fall$Name, abs(fall$Move), ordinal(fall$Pos)))

  bad <- wkr %>% filter(F1Pts == 0) %>% slice_max(points, n = 1, with_ties = FALSE)
  if (nrow(bad)) {
    would <- sum(wk$points > bad$points)  # how many beat it this week
    out <- c(out, sprintf("<b>Bad beat: %s</b> put up %s and got nothing for it. %s", bad$Name, fmt1(bad$points),
                          pick(c("Best ball giveth, best ball taketh away.", "That score wins some weeks. Not this one.",
                                 "Nothing for seventh. Not even a participation trophy."), WEEK + 1)))
  }
  lucky <- wkr %>% filter(F1Pts > 0) %>% slice_min(points, n = 1, with_ties = FALSE)
  if (nrow(lucky))
    out <- c(out, sprintf("<b>Lucky: %s</b> banked +%s F1 with just %s. %s", lucky$Name, fmtpt(lucky$F1Pts), fmt1(lucky$points),
                          pick(c("A point is a point.", "Sometimes the cut line comes to you.", "Not pretty. Still counts."), WEEK + 2)))

  top <- pl %>% filter(pos %in% SKILL) %>% slice_max(score, n = 1, with_ties = FALSE)
  out <- c(out, sprintf("<b>Player of the week: %s</b> (%s, %s) with %s%s.", short_name(top$player_name), top$pos, top$Name,
                        fmt1(top$score), if (!top$starter) ", and he sat on the bench" else ""))
  out
}

ordinal <- function(n) paste0(n, if (n %% 100 %in% 11:13) "th" else c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])

# ---- HTML helpers ------------------------------------------------------------------

# Section shell: numbered kicker, takeaway headline, body
section <- function(id, n, kicker, headline, ..., tag = NULL) {
  tags$section(class = "rs", id = id,
               div(class = "rs-kick", span(class = "rs-num", sprintf("%02d", n)), kicker, if (!is.null(tag)) span(class = "rs-tag", tag)),
               h2(class = "rs-head", HTML(headline)),
               ...)
}

# Narrow table. cols = named list label -> column; first column left aligned, the rest right/tabular.
mtable <- function(df, cols, me = NULL, cls = NULL, widths = NULL, left = 1) {
  colgroup <- if (!is.null(widths)) tags$colgroup(lapply(widths, function(w) tags$col(style = if (!is.na(w)) paste0("width:", w))))
  head <- tags$tr(lapply(seq_along(cols), function(j) tags$th(HTML(names(cols)[j]), class = if (j > left) "r")))
  body <- lapply(seq_len(nrow(df)), function(i) {
    tags$tr(class = if (!is.null(me) && isTRUE(me[i])) "me",
            lapply(seq_along(cols), function(j) tags$td(class = if (j > left) "r", HTML(as.character(df[[cols[[j]]]][i])))))
  })
  tags$table(class = paste("mt", cls), `data-quarto-disable-processing` = "true", colgroup, tags$thead(head), tags$tbody(body))
}

team_cell <- function(name, sub = NULL) paste0('<span class="tn">', htmlEscape(name), '</span>',
                                               if (!is.null(sub)) paste0('<span class="ts">', sub, '</span>') else "")

pct_txt <- function(x) ifelse(x >= 99.5, ">99%", ifelse(x > 0 & x < 0.5, "<1%", paste0(round(x), "%")))
sgn <- function(x, d = 1) ifelse(x > 0, paste0("+", formatC(x, format = "f", digits = d)),
                          ifelse(x < 0, paste0("−", formatC(abs(x), format = "f", digits = d)), "0"))
updown <- function(x, txt = sgn(x)) paste0('<span class="', ifelse(x > 0, "up", ifelse(x < 0, "down", "flat")), '">', txt, '</span>')
money_s <- function(x) paste0(ifelse(x < 0, "−", ""), "$", format(round(abs(x)), big.mark = ",", trim = TRUE))
move_txt <- function(m) ifelse(is.na(m) | m == 0, "", ifelse(m > 0, paste0('<span class="up">▲', m, '</span>'), paste0('<span class="down">▼', abs(m), '</span>')))

tile <- function(label, value, sub = NULL) div(class = "tile", div(class = "tile-lbl", label), div(class = "tile-val", HTML(value)), if (!is.null(sub)) div(class = "tile-sub", HTML(sub)))

# ---- charts: one shared theme, direct labels, inline SVG ---------------------------------

theme_gts <- function() {
  theme_minimal(base_family = "Inter", base_size = 11) +
    theme(plot.background = element_rect(fill = "#0a0a0a", colour = NA),
          panel.background = element_rect(fill = "#0a0a0a", colour = NA),
          panel.grid.major = element_line(colour = "#1c1c1c", linewidth = .35),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "#7a7a7a", size = 9),
          axis.title = element_text(colour = "#5c5c5c", size = 8.5),
          legend.position = "none",
          plot.margin = margin(6, 6, 4, 2))
}

svg_chart <- function(p, w = 6.4, h = 4.4) {
  s <- svglite::svgstring(width = w, height = h, bg = "#0a0a0a", standalone = FALSE, web_fonts = NULL)
  print(p); dev.off()
  x <- as.character(s())
  x <- sub("<svg ", '<svg preserveAspectRatio="xMidYMid meet" style="width:100%;height:auto;display:block" ', x)
  x <- sub(" width='[^']*pt'", "", x); x <- sub(" height='[^']*pt'", "", x)
  div(class = "chart", HTML(x))
}

# ---- more data pieces ---------------------------------------------------------------

# Team x week finish grid (weekly rank), standings order
finish_grid <- function(res, st, through) {
  res %>% filter(week <= through) %>% select(Name, week, rank, F1Pts, points) %>%
    mutate(Name = factor(Name, levels = st$Name)) %>% arrange(Name, week)
}

# Cumulative weekly F1 by team and week (the race chart)
race_data <- function(res, through) {
  res %>% filter(week <= through) %>% arrange(week) %>% group_by(Name) %>%
    mutate(cumF1 = cumsum(F1Pts)) %>% ungroup() %>% select(Name, week, cumF1)
}

# Winning, podium (3rd) and points (6th) scores by week
cut_trend <- function(res, through) {
  res %>% filter(week <= through) %>% group_by(week) %>% arrange(desc(points), .by_group = TRUE) %>%
    summarise(Win = points[1], Podium = points[3], Points = points[6], Seventh = points[7], .groups = "drop")
}

# Best-ball slot output vs league per team (for "weakest slot"), and bench waste
slot_profile <- function(lu, through) {
  s <- lu %>% filter(used, week <= through) %>% group_by(Name, slot) %>% summarise(pts = sum(score) / n_distinct(week), .groups = "drop")
  s %>% group_by(slot) %>% mutate(lg = mean(pts), vs = pts - lg, rk = rank(-pts, ties.method = "min")) %>% ungroup()
}

# ---- page shell ---------------------------------------------------------------------

masthead <- function(week, pending = 0) {
  div(class = "mast",
      div(div(class = "mast-kick", sprintf("Week %d \u00b7 %d season", week, SEASON)),
          div(class = "mast-title", "Fantasy1 Championship Weekly"),
          div(class = "mast-sub", HTML("Presented by <b>Golden Ticket Sims</b>")),
          if (pending > 0) div(class = "prov-flag", sprintf("Provisional \u00b7 %d player%s still to play", pending, if (pending == 1) "" else "s"))),
      tags$img(src = logo_small(), alt = "Golden Ticket Sims"))
}

# items = named vector id -> label, in order
toc_bar <- function(items) {
  tags$nav(class = "toc-bar", lapply(seq_along(items), function(i)
    tags$a(href = paste0("#", names(items)[i]), span(sprintf("%02d", i)), items[[i]])))
}

gts_href <- function() if (gts_link_ok()) GTS_URL else "#"

ad_card <- function() {
  div(class = "ad",
      tags$img(src = logo_small(), alt = ""),
      div(div(class = "ad-kick", "From Golden Ticket Sims"),
          div(class = "ad-title", "The sims behind this report, for your DFS lineups"),
          div(class = "ad-body", paste0("Slate sims and optimal lineups across ", GTS_SPORTS, "."))),
      div(class = "ad-cta",
          tags$a(class = "ad-btn", href = gts_href(), target = "_blank", rel = "noopener", GTS_CTA),
          if (nzchar(GTS_PROMO)) span(class = "ad-promo", GTS_PROMO)))
}

gts_footer <- function() {
  div(class = "gts-foot",
      tags$img(src = logo_small(), alt = "Golden Ticket Sims"),
      div(span("Presented by"), tags$b("Golden Ticket Sims"),
          if (gts_link_ok()) tags$a(href = GTS_URL, target = "_blank", rel = "noopener", sub("^https?://", "", GTS_URL))))
}

# ---- charts --------------------------------------------------------------------------

# Lines with a label at each line's end instead of a legend. hi = names drawn in colour, the rest grey.
label_lines <- function(d, x, y, group, hi, me = NULL, ylab = NULL, yrev = FALSE, ysuffix = "", h = 4.2) {
  cols <- c("#E8C547", "#5b9cf5", "#4cc38a", "#e5484d", "#c084fc", "#f59e0b")
  hi <- head(hi, length(cols))
  pal <- setNames(rep("#3a3a3a", length(unique(d[[group]]))), unique(d[[group]]))
  pal[hi] <- cols[seq_along(hi)]
  d$g <- d[[group]]; d$x <- d[[x]]; d$y <- d[[y]]
  d$hl <- d$g %in% c(hi, me)
  ends <- d %>% group_by(g) %>% filter(x == max(x)) %>% ungroup() %>% filter(hl)
  ends <- ends %>% arrange(if (yrev) y else desc(y))
  # nudge labels apart
  span <- diff(range(d$y)); gap <- span * 0.055
  yy <- ends$y; if (length(yy) > 1) for (i in 2:length(yy)) {
    if (!yrev && yy[i] > yy[i - 1] - gap) yy[i] <- yy[i - 1] - gap
    if (yrev && yy[i] < yy[i - 1] + gap) yy[i] <- yy[i - 1] + gap }
  ends$ly <- yy
  p <- ggplot(d, aes(x, y, group = g, colour = g)) +
    geom_line(data = d %>% filter(!hl), linewidth = .7) +
    geom_line(data = d %>% filter(hl), linewidth = 1.3) +
    geom_point(data = d %>% filter(hl), size = 1.8) +
    geom_text(data = ends, aes(y = ly, label = paste0(g, "  ", y, ysuffix)), hjust = 0, nudge_x = .12, size = 3.1,
              family = "Inter", fontface = "bold") +
    scale_colour_manual(values = pal) +
    scale_x_continuous(breaks = unique(d$x), labels = paste0("W", unique(d$x)), expand = expansion(mult = c(.03, .38))) +
    labs(x = NULL, y = ylab) + theme_gts()
  if (yrev) p <- p + scale_y_reverse()
  svg_chart(p, h = h)
}
