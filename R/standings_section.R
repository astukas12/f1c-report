# Standings section of the weekly report: official table, the race, week by week, historical context,
# standings vs final finish. History compares with HIST_YEARS only (2024 was a 10-team league with a
# different F1 scale) and uses the official tiebreak everywhere: F1 points (tied weeks split the points),
# then average weekly finish, then points for.

HIST_YEARS <- c(2022, 2023, 2025)
F1_STD     <- c(15, 10, 7, 4, 2, 1, rep(0, 7))
RACE_COLS  <- c("#E8C547", "#5b9cf5", "#4cc38a", "#e5484d", "#c084fc", "#f59e0b")
GHOST      <- "#a8954a"

# Weekly scores of past seasons (R/history.R), cached in sims/history-weekly.rds
load_history <- function() {
  f <- "sims/history-weekly.rds"
  if (!file.exists(f)) { source("R/history.R", local = TRUE); saveRDS(build_history(max_week_live = 1), f) }
  readRDS(f) %>% filter(year %in% HIST_YEARS)
}

split_pts <- function(rk) vapply(rk, function(r) { n <- sum(rk == r); lo <- r - (n - 1) / 2; mean(F1_STD[lo:(lo + n - 1)]) }, numeric(1))

# Position after every week of every past season, plus the final position
hist_positions <- function(h) {
  h %>% group_by(year, week) %>% mutate(rk = rank(-score, ties.method = "average"), f1 = split_pts(rk)) %>% ungroup() %>%
    arrange(year, owner, week) %>% group_by(year, owner) %>%
    mutate(F1c = cumsum(f1), avgR = cummean(rk), PF = cumsum(score)) %>% ungroup() %>%
    group_by(year, week) %>% arrange(desc(F1c), avgR, desc(PF), .by_group = TRUE) %>% mutate(pos = row_number()) %>% ungroup() %>%
    left_join((.) %>% group_by(year) %>% filter(week == max(week)) %>% ungroup() %>% select(year, owner, final = pos), by = c("year", "owner"))
}

yr2 <- function(y) paste0("’", substr(y, 3, 4))
ord_list <- function(x) { x <- sapply(x, ordinal); if (length(x) < 2) x else paste(paste(head(x, -1), collapse = ", "), "and", tail(x, 1)) }
and_list <- function(x) if (length(x) < 2) x else paste(paste(head(x, -1), collapse = ", "), "and", tail(x, 1))

# "the highest" / "the lowest" / "the 2nd-highest" of x against past values
superl <- function(x, past, hi = "highest", lo = "lowest") {
  if (x > max(past)) return(paste("the", hi))
  if (x < min(past)) return(paste("the", lo))
  r <- sum(past > x) + 1
  if (r <= length(past) / 2 + 1) paste0("the ", ordinal(r), "-", hi) else paste0("the ", ordinal(sum(past < x) + 1), "-", lo)
}

# ---- 1. official standings ------------------------------------------------------------
standings_table <- function(st, wk) {
  d <- st %>% left_join(wk %>% select(franchise_id, wkRank = rank, wkF1 = F1Pts), by = "franchise_id") %>%
    mutate(posc = paste0('<span class="pos">', Pos, '</span>', move_txt(Move)),
           team = pmap_chr(list(Name, Franchise), function(n, f) team_cell(n, htmlEscape(f))),
           fin  = paste0('<span class="fin', ifelse(wkRank <= 3, " top", ""), '">', vapply(wkRank, finish_lbl, ""), '</span>'),
           w = Wins, pod = Podiums, pts = PtsFin,
           gain = ifelse(wkF1 > 0, paste0('<span class="gain">+', vapply(wkF1, fmtpt, ""), '</span>'), '<span class="nil">–</span>'),
           won  = ifelse(Earnings > 0, money_s(Earnings), '<span class="nil">–</span>'),
           f1   = paste0('<span class="big">', vapply(F1Pts, fmtpt, ""), '</span>'))
  mtable(d, list("#" = "posc", Team = "team", Wk = "fin", W = "w", Pod = "pod", Pts = "pts", "+F1" = "gain", "$" = "won", F1 = "f1"),
         left = 2, cls = "st", widths = c("2.55rem", NA, "1.85rem", "1.2rem", "1.65rem", "1.6rem", "1.9rem", "2.3rem", "2.1rem"))
}

# ---- 2. the race ----------------------------------------------------------------------------
race_chart <- function(res, st, hp, week) {
  cur <- res %>% filter(week <= !!week) %>% arrange(week) %>% group_by(Name) %>% mutate(y = cumsum(F1Pts)) %>% ungroup()
  top <- st$Name[1:6]
  pal <- setNames(rep("#3a3a3a", nrow(st)), st$Name); pal[top] <- RACE_COLS
  champs <- hp %>% filter(final == 1, week <= !!week) %>% transmute(lab = paste(yr2(year), "champ", owner), week, y = F1c)
  ymax <- max(c(cur$y, champs$y))
  lab <- bind_rows(cur %>% filter(week == !!week, Name %in% top) %>% transmute(txt = paste(fmtpt(y), Name), y, col = pal[Name], face = "bold"),
                   champs %>% filter(week == !!week) %>% transmute(txt = paste(fmtpt(y), lab), y, col = GHOST, face = "plain")) %>%
    arrange(desc(y))
  gap <- ymax * 0.062; yy <- lab$y
  for (i in seq_along(yy)[-1]) if (yy[i] > yy[i - 1] - gap) yy[i] <- yy[i - 1] - gap
  lab$ly <- yy
  p <- ggplot() +
    geom_line(data = champs, aes(week, y, group = lab), colour = GHOST, linewidth = .8, linetype = "22") +
    geom_line(data = cur %>% filter(!Name %in% top), aes(week, y, group = Name), colour = "#3a3a3a", linewidth = .7) +
    geom_line(data = cur %>% filter(Name %in% top), aes(week, y, group = Name, colour = Name), linewidth = 1.4) +
    geom_point(data = cur %>% filter(Name %in% top, week == !!week), aes(week, y, colour = Name), size = 2.2) +
    geom_segment(data = lab, aes(x = week + .04, xend = week + .22, y = y, yend = ly), colour = "#333333", linewidth = .3) +
    geom_text(data = lab, aes(week + .26, ly, label = txt), colour = lab$col, fontface = lab$face, hjust = 0, size = 3, family = "Inter") +
    scale_colour_manual(values = pal) +
    scale_x_continuous(breaks = seq_len(week), labels = paste0("W", seq_len(week)), limits = c(1, week + 1.9), expand = c(.02, 0)) +
    scale_y_continuous(limits = c(0, ceiling((ymax + 2) / 5) * 5), expand = c(0, 0)) +
    labs(x = NULL, y = "F1 points") + theme_gts() + theme(panel.grid.major.x = element_blank())
  tagList(div(class = "key", HTML('<span><i class="dash"></i>Past champions at this point</span><span>Coloured: current top 6</span>')),
          svg_chart(p, h = 4.4))
}

# ---- 3. week by week ------------------------------------------------------------------------
finish_grid_chart <- function(res, st, week) {
  ord <- st$Name
  d <- res %>% filter(week <= !!week) %>%
    mutate(Name = factor(Name, levels = rev(ord)),
           band = case_when(rank == 1 ~ "win", rank <= 3 ~ "pod", rank <= 6 ~ "pts", TRUE ~ "out"),
           lbl = ifelse(rank == floor(rank), as.character(rank), paste0("T", floor(rank))))
  tot <- st %>% transmute(Name = factor(Name, levels = rev(ord)), F1Pts)
  p <- ggplot(d, aes(week, Name)) +
    geom_tile(aes(fill = band), colour = "#0a0a0a", linewidth = 1.6) +
    geom_text(aes(label = lbl, colour = band), size = 3.3, family = "Inter", fontface = "bold") +
    geom_text(data = tot, aes(x = week + .9, y = Name, label = vapply(F1Pts, fmtpt, "")), colour = "#ffffff", size = 3.5,
              family = "Inter", fontface = "bold", hjust = 1, inherit.aes = FALSE) +
    scale_fill_manual(values = c(win = "#E8C547", pod = "#7a6a25", pts = "#2c2c2c", out = "#141414")) +
    scale_colour_manual(values = c(win = "#0a0a0a", pod = "#ffffff", pts = "#d6d6d6", out = "#555555")) +
    scale_x_continuous(breaks = c(seq_len(week), week + .75), labels = c(paste0("W", seq_len(week)), "F1"),
                       position = "top", expand = expansion(add = c(.5, .3))) +
    labs(x = NULL, y = NULL) + theme_gts() +
    theme(panel.grid = element_blank(), axis.text.y = element_text(colour = "#e8e8e8", size = 10, face = "bold"))
  tagList(svg_chart(p, h = 1.2 + .36 * length(ord)),
          div(class = "key", HTML('<span><i class="sw" style="background:#E8C547"></i>Won the week</span><span><i class="sw" style="background:#7a6a25"></i>Podium</span><span><i class="sw" style="background:#2c2c2c"></i>Points finish</span><span><i class="sw" style="background:#141414;border:1px solid #333"></i>No points</span>')))
}

# ---- 4. historical context --------------------------------------------------------------------
fact_row <- function(num, head, sub) div(class = "fact", div(class = "fact-num", num), div(div(class = "fact-head", HTML(head)), div(class = "fact-sub", HTML(sub))))

context_rows <- function(st, res, hp, week) {
  pw <- hp %>% filter(week == !!week)
  rows <- list()
  # leader's start
  lF <- st$F1Pts[1]; ldr <- st$Name[1]
  higher <- pw %>% filter(F1c > lF) %>% arrange(desc(F1c)); tied <- pw %>% filter(F1c == lF)
  r <- nrow(higher) + 1
  rows[[length(rows) + 1]] <- fact_row(fmtpt(lF),
    sprintf("%s: %s start through %d weeks", ldr, if (r == 1 && !nrow(tied)) "best" else paste0(if (nrow(tied)) "tied for " else "", ordinal(r), "-best"), week),
    paste0(if (nrow(higher)) paste0("Higher: ", paste0(higher$owner, " ", higher$year, " (", fmtpt_v(higher$F1c), ", finished ", sapply(higher$final, ordinal), ")", collapse = "; ")) else "No team has started better.",
           if (nrow(tied)) paste0(". Also ", fmtpt(lF), ": ", paste0(tied$owner, " ", tied$year, " (finished ", sapply(tied$final, ordinal), ")", collapse = "; ")) else "", "."))
  # zero-point teams
  z <- st %>% filter(F1Pts == 0)
  if (nrow(z)) {
    pz <- pw %>% filter(F1c == 0) %>% arrange(final)
    rows[[length(rows) + 1]] <- fact_row("0",
      sprintf("%s: no F1 points after %d weeks", and_list(z$Name), week),
      if (nrow(pz)) sprintf("%d previous team%s had 0 at this point. They finished %s.", nrow(pz), if (nrow(pz) == 1) "" else "s", ord_list(pz$final))
      else "No previous team was still on 0 at this point.")
  }
  # 6th place
  six <- st$F1Pts[6]; p6 <- pw %>% filter(pos == 6) %>% arrange(year)
  rows[[length(rows) + 1]] <- fact_row(fmtpt(six),
    sprintf("6th place on %s F1, %s after %d weeks", fmtpt(six), superl(six, p6$F1c), week),
    if (n_distinct(p6$F1c) == 1) sprintf("%s in each previous season.", fmtpt(p6$F1c[1]))
    else paste0("Previous seasons: ", paste0(fmtpt_v(p6$F1c), " (", p6$year, ")", collapse = ", "), "."))
  # scoring level
  hs <- hp %>% filter(week <= !!week) %>% group_by(year) %>%
    summarise(avg = mean(score), win = mean(score[rk == min(rk)]), gap = mean(tapply(score, week, function(s) max(s) - min(s))), .groups = "drop")
  cr <- res %>% filter(week <= !!week)
  cavg <- mean(cr$points); cwin <- mean(tapply(cr$points, cr$week, max)); cgap <- mean(tapply(cr$points, cr$week, function(s) max(s) - min(s)))
  top_of <- function(v, col) { i <- which.max(hs[[col]]); sprintf("%s in %d", fmt1(hs[[col]][i]), hs$year[i]) }
  rows[[length(rows) + 1]] <- fact_row(fmt1(cavg),
    sprintf("Average team score %s, %s through %d weeks", fmt1(cavg), superl(cavg, hs$avg), week),
    sprintf("Previous high %s. Average winning score %s, %s.", top_of(hs$avg, "avg"), fmt1(cwin),
            if (cwin > max(hs$win) && cavg > max(hs$avg)) "also the highest" else superl(cwin, hs$win)))
  rows[[length(rows) + 1]] <- fact_row(fmt1(cgap),
    sprintf("Average gap from 1st to 12th each week %s, %s", fmt1(cgap), superl(cgap, hs$gap, "widest", "narrowest")),
    sprintf("Previous high %s.", top_of(hs$gap, "gap")))
  div(class = "facts", rows)
}
fmtpt_v <- function(x) vapply(x, fmtpt, "")

# ---- 5. standings vs final finish -------------------------------------------------------------
settle_chart <- function(hp, week) {
  rho <- hp %>% group_by(week) %>% summarise(v = 100 * cor(pos, final, method = "spearman"), .groups = "drop") %>% filter(week < max(week))
  now <- rho %>% filter(week == !!week)
  p <- ggplot(rho, aes(week, v)) +
    geom_line(colour = "#6a6a6a", linewidth = 1) + geom_point(colour = "#6a6a6a", size = 1.4) +
    geom_point(data = now, colour = GOLD, size = 3.6) +
    geom_text(data = now, aes(label = paste0("Week ", week, ": ", round(v))), colour = GOLD, size = 3.2, family = "Inter",
              fontface = "bold", hjust = 0, nudge_x = .5, nudge_y = -7) +
    scale_x_continuous(breaks = c(1, 4, 8, 12, 17), labels = function(x) paste0("W", x)) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
    labs(x = NULL, y = NULL) + theme_gts()
  svg_chart(p, h = 2.8)
}

settle_rows <- function(hp, week) {
  pw <- hp %>% filter(week == !!week) %>% mutate(chg = pos - final)
  nt <- pw %>% group_by(year) %>% summarise(n = n(), .groups = "drop")
  t6 <- pw %>% filter(pos <= 6); t3 <- pw %>% filter(pos <= 3)
  b3 <- pw %>% left_join(nt, by = "year") %>% filter(pos > n - 3)
  climb <- pw %>% arrange(desc(chg), final) %>% slice(1)
  fall  <- pw %>% arrange(chg, pos) %>% slice(1)
  champ <- pw %>% filter(final == 1) %>% arrange(year)
  bbest <- b3 %>% arrange(final) %>% slice(1)
  div(class = "facts",
      fact_row(sprintf("%d/%d", sum(t6$final <= 6), nrow(t6)), sprintf("Top 6 after week %d who finished top 6", week),
               sprintf("Biggest climb from week %d: %s %d, %s to %s.", week, climb$owner, climb$year, ordinal(climb$pos), ordinal(climb$final))),
      fact_row(sprintf("%d/%d", sum(t3$final <= 3), nrow(t3)), sprintf("Top 3 after week %d who finished top 3", week),
               paste0("Champions at this point: ", paste0(champ$owner, " ", ordinal_v(champ$pos), " (", champ$year, ")", collapse = ", "),
                      sprintf(". Biggest fall: %s %d, %s to %s.", fall$owner, fall$year, ordinal(fall$pos), ordinal(fall$final)))),
      fact_row(sprintf("%d/%d", sum(b3$final <= 6), nrow(b3)), sprintf("Bottom 3 after week %d who finished top 6", week),
               sprintf("Best finish from the bottom 3: %s (%s %d).", ordinal(bbest$final), bbest$owner, bbest$year)))
}
ordinal_v <- function(x) vapply(x, ordinal, "")
