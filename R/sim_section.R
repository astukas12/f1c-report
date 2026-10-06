# The GTS Sim Center section: title odds table, title odds by week, projected final F1, next week's sims.
# Odds come from ros_history() in R/report.R (this season's results only, ROS_MODEL "v2").

sim_odds_table <- function(odds, prev, st) {
  o <- odds %>% left_join(st %>% select(Name, Franchise), by = "Name") %>%
    left_join(prev %>% select(Name, PrevT = Title), by = "Name") %>% arrange(desc(Title), desc(Proj)) %>%
    mutate(team = pmap_chr(list(Name, Franchise), function(n, f) team_cell(n, htmlEscape(f))),
           f1 = vapply(Current, fmtpt, ""), ttl = paste0('<span class="big">', pct_txt(Title), '</span>'),
           chg = ifelse(is.na(PrevT) | abs(Title - PrevT) < 0.5, '<span class="nil">–</span>',
                        updown(Title - PrevT, paste0(ifelse(Title > PrevT, "+", "−"), round(abs(Title - PrevT))))),
           t3 = pct_txt(Top3), t6 = pct_txt(Top6),
           pj = paste0(round(Proj), '<span class="ts">', round(P10), "–", round(P90), '</span>'))
  mtable(o, list(Team = "team", F1 = "f1", Title = "ttl", Wk = "chg", "Top 3" = "t3", "Top 6" = "t6", "Proj F1" = "pj"),
         left = 1, widths = c(NA, "2rem", "3rem", "2.2rem", "2.7rem", "2.7rem", "3.6rem"))
}

odds_by_week_chart <- function(hist, odds, week) {
  top <- (odds %>% arrange(desc(Title)))$Name[1:5]
  pal <- setNames(rep("#3a3a3a", n_distinct(hist$Name)), unique(hist$Name)); pal[top] <- RACE_COLS[1:5]
  d <- hist %>% filter(week <= !!week) %>% mutate(T = round(Title))
  lab <- d %>% filter(week == !!week, Name %in% top) %>% arrange(desc(T)); yy <- lab$T
  for (i in seq_along(yy)[-1]) if (yy[i] > yy[i - 1] - 3.2) yy[i] <- yy[i - 1] - 3.2
  lab$ly <- yy
  p <- ggplot() +
    geom_line(data = d %>% filter(!Name %in% top), aes(week, T, group = Name), colour = "#3a3a3a", linewidth = .7) +
    geom_line(data = d %>% filter(Name %in% top), aes(week, T, colour = Name, group = Name), linewidth = 1.4) +
    geom_point(data = d %>% filter(Name %in% top), aes(week, T, colour = Name), size = 1.9) +
    geom_text(data = lab, aes(week + .2, ly, label = paste0(T, "% ", Name), colour = Name), hjust = 0, size = 3.1, family = "Inter", fontface = "bold") +
    scale_colour_manual(values = pal) +
    scale_x_continuous(breaks = seq_len(week), labels = paste0("W", seq_len(week)), limits = c(1, week + 1.4)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, max(d$T) + 3)) +
    labs(x = NULL, y = NULL) + theme_gts() + theme(panel.grid.major.x = element_blank())
  svg_chart(p, h = 3.8)
}

proj_range_chart <- function(odds) {
  r <- odds %>% arrange(desc(Title), desc(Proj))
  r <- r %>% mutate(Name = factor(Name, levels = rev(r$Name)))
  p <- ggplot(r) +
    geom_segment(aes(x = P10, xend = P90, y = Name, yend = Name), colour = "#3a3a3a", linewidth = 3, lineend = "round") +
    geom_point(aes(Current, Name), colour = "#8a8a8a", size = 2) +
    geom_point(aes(Proj, Name), colour = GOLD, size = 3) +
    geom_text(aes(P90 + 3, Name, label = round(Proj)), colour = "#e8e8e8", size = 3, hjust = 0, family = "Inter", fontface = "bold") +
    scale_x_continuous(limits = c(0, max(r$P90) + 15)) +
    labs(x = "F1 points", y = NULL) + theme_gts() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = "#e8e8e8", size = 10, face = "bold"))
  svg_chart(p, h = 4.8)
}

next_week_block <- function(week) {
  div(class = "soon", div(class = "soon-k", "GTS Sim Center"),
      div(class = "soon-t", "Coming Thursday, with updates after every game window."),
      div(class = "soon-s", sprintf("Projected finish, win, podium and top-6 odds for every team, %s sims.", format(SIM_N, big.mark = ","))))
}
