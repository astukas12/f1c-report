# Section bodies for the weekly report (REPORT_PLAN.md + approved mockups, locked 6 Oct 2026). Each takes D, the
# list from build_report_data(), and returns HTML. Pages: index (Standings), review, sims, money, office, teams, alltime.
# Rules: never highlight any one team; plain titles; history comparisons use 2022, 2023, 2025 (2024 only in all-time F1).

PAGES <- c(index = "Standings", review = "Week in Review", sims = "Sim Center", money = "Money & Cap",
           office = "Front Office", teams = "Team Pages", alltime = "All-Time")
POS_C <- "#82E0AA"; NEG_C <- "#F1948A"

page_nav <- function(current, href = function(p) paste0(p, ".html")) {
  tags$nav(class = "toc-bar", lapply(names(PAGES), function(p) tags$a(class = if (p == current) "on", href = href(p), PAGES[[p]])))
}

page_head <- function(D, section, title, sub = NULL) {
  div(class = "mast",
      div(div(class = "mast-kick", paste0("Fantasy1 Championship · ", section)),
          div(class = "mast-title", title),
          div(class = "mast-sub", HTML(sub %||% sprintf("Week %d · %d season · Presented by <b>Golden Ticket Sims</b>", D$WEEK, SEASON)))),
      tags$img(src = logo_small(), alt = "Golden Ticket Sims"))
}

page_foot <- function(D) div(class = "pg-foot", span(sprintf("Week %d · %d season", D$WEEK, SEASON)), span(HTML("Presented by <b>Golden Ticket Sims</b>")))

sec <- function(id, title, ..., kicker = NULL, tag = NULL) {
  tags$section(class = "rs", id = id,
               if (!is.null(kicker) || !is.null(tag)) div(class = "rs-kick", kicker, if (!is.null(tag)) span(class = "rs-tag", HTML(tag))),
               h2(class = "rs-head", title), ...)
}

signed_c <- function(x, d = 0) ifelse(x == 0, '<span class="nil">–</span>',
  sprintf('<span class="%s">%s$%s</span>', ifelse(x > 0, "pos-v", "neg-v"), ifelse(x > 0, "+", "−"), formatC(abs(x), format = "f", digits = d)))
nil <- '<span class="nil">–</span>'

# ---- 1. Standings ----------------------------------------------------------------------------
section_standings <- function(D) {
  W <- D$WEEK; wk <- D$res %>% filter(week == W)
  tagList(
    sec("standings", sprintf("Standings after week %d", W),
        standings_table(D$st, wk),
        div(class = "note", "Ranked by F1 points, then average weekly finish, then points scored. Wk = this week's finish. W = wins, Pod = podiums (top 3), Pts = points finishes (top 6). +F1 = F1 points this week. $ = prize money won this season.")),
    sec("race", "The Race", p(class = "dek", "F1 points by week."), race_chart(D$res, D$st, D$hp, W)),
    sec("weekly", "Week by Week", p(class = "dek", "Each team's finish every week, in standings order."), finish_grid_chart(D$res, D$st, W)),
    sec("context", "Historical Context", context_rows(D$st, D$res, D$hp, W),
        div(class = "note", sprintf("Compared with %s. 2024 is left out: it was a 10-team league.", and_list(HIST_YEARS)))),
    sec("final", "Standings vs. Final Finish",
        p(class = "dek", "How closely the standings after each week matched the final standings (0 = no relation, 100 = identical)."),
        settle_chart(D$hp, W), settle_rows(D$hp, W),
        div(class = "note", sprintf("Seasons %s, official tiebreak.", and_list(HIST_YEARS)))))
}

# ---- 2. Week in Review ----------------------------------------------------------------------
section_review <- function(D) {
  W <- D$WEEK; wk <- D$res %>% filter(week == W) %>% arrange(rank)
  r <- wk %>% mutate(fin = paste0('<span class="fin', ifelse(rank <= 3, " top", ""), '">', vapply(rank, finish_lbl, ""), '</span>'),
                     team = pmap_chr(list(Name, Franchise), function(n, f) team_cell(n, htmlEscape(f))),
                     sc = paste0('<span class="big">', fmt1(points), '</span>'),
                     f1 = ifelse(F1Pts > 0, paste0("+", vapply(F1Pts, fmtpt, "")), nil),
                     won = ifelse(Earnings > 0, money_s(Earnings), nil))
  tbl <- mtable(r, list(Finish = "fin", Team = "team", Score = "sc", F1 = "f1", "$" = "won"), left = 2, cls = "res",
                widths = c("3rem", NA, "3.6rem", "2.4rem", "2.6rem"))
  wins_hist <- D$hp %>% group_by(year, week) %>% summarise(top = max(score), .groups = "drop")
  past_win <- c(wins_hist$top, (D$res %>% filter(week < W) %>% group_by(week) %>% summarise(t = max(points)))$t)
  rec <- wins_hist %>% slice_max(top, n = 1, with_ties = FALSE)
  win_line <- sprintf("<b>%s</b> is %s of %d winning weeks since 2022 (2024 left out). Record: %s (%d Wk %d).",
                      fmt1(wk$points[1]), ordinal(sum(past_win > wk$points[1]) + 1), length(past_win) + 1, fmt1(rec$top), rec$year, rec$week)

  # scores by finish
  place <- function(d, s) d %>% group_by(week) %>% arrange(desc(.data[[s]]), .by_group = TRUE) %>%
    summarise(`1st` = .data[[s]][1], `3rd` = .data[[s]][3], `6th` = .data[[s]][6], .groups = "drop")
  cur  <- place(D$res %>% filter(week <= W), "points")
  past <- place(D$hp %>% mutate(week = paste(year, week)), "score") %>% summarise(across(-week, mean))
  past_w <- place(D$hp %>% filter(week <= W) %>% mutate(week = paste(year, week)), "score") %>% summarise(across(-week, mean))
  curl <- cur %>% pivot_longer(-week, names_to = "pl", values_to = "s")
  cols <- c(`1st` = GOLD, `3rd` = "#C9A646", `6th` = "#F2F2F2")
  lab <- curl %>% filter(week == W) %>% arrange(desc(s))
  p1 <- ggplot(curl, aes(week, s, colour = pl)) +
    geom_hline(yintercept = past$`1st`, colour = GOLD, linetype = "22", linewidth = .5, alpha = .6) +
    geom_hline(yintercept = past$`6th`, colour = "#F2F2F2", linetype = "22", linewidth = .5, alpha = .4) +
    geom_line(linewidth = 1.3) + geom_point(size = 2) +
    geom_text(aes(label = fmt1(s)), vjust = -1, size = 2.5, family = "Inter", show.legend = FALSE) +
    geom_text(data = lab, aes(label = pl), hjust = 0, nudge_x = .2, size = 3.1, family = "Inter", fontface = "bold") +
    scale_colour_manual(values = cols) +
    scale_x_continuous(breaks = seq_len(W), labels = paste0("W", seq_len(W)), limits = c(.8, W + .7)) +
    labs(x = NULL, y = NULL) + theme_gts() + theme(panel.grid.major.x = element_blank())
  avg <- tibble(pl = c("1st", "3rd", "6th"), now = unlist(cur %>% summarise(across(-week, mean))), was = unlist(past_w)) %>%
    mutate(d = now - was, plc = paste0('<span class="fin top">', pl, '</span>'), nowc = paste0('<span class="big">', fmt1(now), '</span>'), wasc = fmt1(was),
           dc = sprintf('<span class="%s">%s</span>', ifelse(d >= 0, "pos-v", "neg-v"), sgn(d)))
  avgt <- mtable(avg, setNames(list("plc", "nowc", "wasc", "dc"), c(sprintf("Average, weeks 1–%d", W), "2026", "Past 3 yrs", "+/−")),
                 left = 1, widths = c(NA, "3.6rem", "4.2rem", "3rem"))

  # projected vs actual
  pva <- NULL
  if (!is.null(D$wsim)) {
    pv <- D$wsim %>% select(franchise_id, proj = mean, win, ppos = Pos) %>% left_join(wk %>% select(franchise_id, Name, points, rank), by = "franchise_id") %>%
      mutate(diff = points - proj) %>% arrange(diff) %>%
      mutate(lbl = sprintf("%s  ·  %s proj · %s actual", Name, fmt1(proj), fmt1(points)), lbl = factor(lbl, levels = lbl))
    b <- pv %>% slice_max(diff, n = 1, with_ties = FALSE)
    outcome <- if (b$rank == 1) sprintf("won the week by %s", fmt1(b$points - sort(wk$points, TRUE)[2])) else sprintf("finished %s with %s", finish_lbl(b$rank), fmt1(b$points))
    p2 <- ggplot(pv, aes(diff, lbl, fill = diff > 0)) + geom_col(width = .62) +
      geom_text(aes(x = diff + ifelse(diff > 0, 1.2, -1.2), label = sgn(diff), hjust = ifelse(diff > 0, 0, 1)), colour = "#F2F2F2", size = 2.9, family = "Inter", fontface = "bold") +
      geom_vline(xintercept = 0, colour = "#555") +
      scale_fill_manual(values = c(`TRUE` = POS_C, `FALSE` = NEG_C)) +
      scale_x_continuous(expand = expansion(mult = .25)) + labs(x = NULL, y = NULL) + theme_gts() +
      theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = "#d6d6d6", size = 8.5))
    pva <- sec("pva", "Projected vs. Actual", tag = "Sims by <b>Golden Ticket Sims</b>",
               svg_chart(p2, h = 4.6),
               div(class = "fact-line", HTML(sprintf("<b>%s was projected %s with a %s chance to win, and %s.</b>", b$Name, ordinal(b$ppos),
                                                     paste0(formatC(b$win, format = "f", digits = 1), "%"), outcome))),
               div(class = "note", "Projection: average score from the GTS preview sim (ETR projections). Sims by Golden Ticket Sims."))
  }

  # team of the week
  pl <- D$lu %>% filter(week == W) %>% rename(starter = used) %>% select(-slot)
  tow <- team_of_week(pl)
  tw <- tow %>% mutate(slotc = paste0('<span class="slot-g">', slot, '</span>'),
                       pc = pmap_chr(list(player_name, nfl, Name, starter), function(n, t, o, s) team_cell(short_name(n), paste0(t, " · ", o, if (!s) " · bench" else ""))),
                       s = paste0('<span class="big">', fmt1(score), '</span>'))
  top <- tow %>% slice_max(score, n = 1, with_ties = FALSE)
  allp <- c(D$hplay$score[D$hplay$pos %in% SKILL], D$lu$score[D$lu$week < W])
  recp <- D$hplay %>% filter(pos %in% SKILL) %>% slice_max(score, n = 1, with_ties = FALSE)
  pline <- if (length(allp)) sprintf("<b>%s's %s</b> is the %s-best player week since 2022 (2024 left out). Record: %s %s.",
                                     sub("^.* ", "", short_name(top$player_name)), fmt1(top$score), ordinal(sum(allp > top$score) + 1),
                                     sub("^.* ", "", short_name(recp$player_name)), fmt1(recp$score)) else ""
  tagList(
    sec("results", sprintf("Week %d Results", W), tbl,
        div(class = "note", "Dashed line: points cut (top 6 score F1 points)."), div(class = "fact-line", HTML(win_line))),
    sec("byfinish", "Scores by Finish",
        div(class = "key", HTML(sprintf('<span><i class="sw" style="background:%s"></i>1st</span><span><i class="sw" style="background:#C9A646"></i>3rd</span><span><i class="sw" style="background:#F2F2F2"></i>6th</span><span><i class="dash"></i>Past avg 1st / 6th</span>', GOLD))),
        svg_chart(p1, h = 3.6), avgt,
        div(class = "note", "Past 3 years: 2022, 2023 and 2025. 2024 left out (10-team league).")),
    pva,
    sec("tow", "Team of the Week", p(class = "dek", "Best possible lineup from every roster: QB, 2 RB, 3 WR, TE, 2 FLEX."),
        mtable(tw, list(Pos = "slotc", Player = "pc", Pts = "s"), left = 2, widths = c("3rem", NA, "3.4rem")),
        div(class = "tow-total", span(sprintf("Total · top team scored %s", fmt1(max(wk$points)))), tags$b(fmt1(sum(tow$score)))),
        div(class = "fact-line", HTML(pline))))
}

# ---- 3. Sim Center --------------------------------------------------------------------------
section_sims <- function(D) {
  W <- D$WEEK
  odds <- D$ohist %>% filter(week == W); prev <- D$ohist %>% filter(week == W - 1)
  o <- odds %>% left_join(D$st %>% select(Name, Franchise, Pos), by = "Name") %>% left_join(prev %>% select(Name, PrevT = Title), by = "Name") %>%
    arrange(desc(Title), desc(Proj)) %>%
    mutate(team = pmap_chr(list(Name, Pos, Current), function(n, p, c) team_cell(n, sprintf("%s · %s F1", ordinal(p), fmtpt(c)))),
           ttl = sprintf('<span class="bar"><i style="width:%.0f%%"></i></span><span class="big">%s</span>', pmin(100, Title / max(Title) * 100), pct_txt(Title)),
           chg = ifelse(is.na(PrevT) | abs(Title - PrevT) < 0.5, nil, updown(Title - PrevT, paste0(ifelse(Title > PrevT, "+", "−"), round(abs(Title - PrevT))))),
           t3 = pct_txt(Top3), t6 = pct_txt(Top6))
  tbl <- mtable(o, list(Team = "team", Title = "ttl", Wk = "chg", "Top 3" = "t3", "Top 6" = "t6"), left = 1, cls = "odds",
                widths = c(NA, "5.4rem", "2.2rem", "2.7rem", "2.7rem"))
  d <- D$ohist %>% filter(week <= W) %>% mutate(T = round(Title, 1))
  hi <- o$Name[o$Title >= 1]
  pal <- setNames(rep("#3a3a3a", n_distinct(d$Name)), unique(d$Name)); pal[hi] <- rep(RACE_COLS, 2)[seq_along(hi)]
  lab <- d %>% filter(week == W, Name %in% hi, T >= 4.5) %>% arrange(desc(T)); yy <- lab$T   # small ones: coloured line, no label
  for (i in seq_along(yy)[-1]) if (yy[i] > yy[i - 1] - 3.2) yy[i] <- yy[i - 1] - 3.2
  lab$ly <- yy
  p <- ggplot() +
    geom_line(data = d %>% filter(!Name %in% hi), aes(week, T, group = Name), colour = "#3a3a3a", linewidth = .7) +
    geom_line(data = d %>% filter(Name %in% hi), aes(week, T, colour = Name, group = Name), linewidth = 1.4) +
    geom_point(data = d %>% filter(Name %in% hi), aes(week, T, colour = Name), size = 1.9) +
    geom_text(data = lab, aes(W + .2, ly, label = paste0(round(T), "% ", Name), colour = Name), hjust = 0, size = 3.1, family = "Inter", fontface = "bold") +
    scale_colour_manual(values = pal) +
    scale_x_continuous(breaks = seq_len(W), labels = paste0("W", seq_len(W)), limits = c(1, W + 1.4)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, max(d$T) + 3)) +
    labs(x = NULL, y = NULL) + theme_gts() + theme(panel.grid.major.x = element_blank())
  tagList(
    sec("odds", sprintf("Title Odds after week %d", W), kicker = "GTS Sim Center", tag = "Sims by <b>Golden Ticket Sims</b>",
        p(class = "dek", sprintf("%s simulated finishes to the season, from this season's results only.", format(ROS_N, big.mark = ","))),
        tbl, div(class = "note", "Title = most F1 points after week 18. Wk = change in title odds since last week. Sims by Golden Ticket Sims.")),
    sec("oddsweek", "Title Odds by Week", kicker = "GTS Sim Center", p(class = "dek", "Each week's odds, rerun with the results to that point."),
        svg_chart(p, h = 3.8), div(class = "key", HTML('<span><i class="sw" style="background:#3a3a3a"></i>Others under 1%</span>')), div(class = "note", "How it works: each team's weekly scoring level is its average so far. Every simulated week draws a score for each team, ranks the 12 and awards 15/10/7/4/2/1 F1 points. A team's level can drift week to week (injuries, trades, waivers), so nothing is locked in. Checked against 2022, 2023 and 2025.")),
    sec("nextweek", sprintf("Week %d Sims", W + 1), kicker = "GTS Sim Center",
        if (is.null(D$wnext)) div(class = "soon", div(class = "soon-k", "GTS Sim Center"),
            div(class = "soon-t", "Coming Thursday, with updates after every game window."),
            div(class = "soon-s", sprintf("Projected finish, win, podium and top-6 odds for every team, from %s simulated weeks.", format(SIM_N, big.mark = ","))))
        else week_sim_table(D$wnext),
        ad_card(), if (is.null(D$wnext)) div(class = "note", "Sims by Golden Ticket Sims.")))
}

# ---- 4. Money & Cap ---------------------------------------------------------------------------
section_money <- function(D) {
  cc <- D$cc
  d <- cc %>% mutate(team = pmap_chr(list(Name, Signed), function(n, s) team_cell(n, paste(s, "signed"))),
                     wf = num1(F1Pts), sf = num1(Seasonal), so = num1(Socialism), tr = num1(Traded),
                     tc = sub("\\.0$", "", fmt1(TotalCap)), sa = paste0("$", Salary), dh = num1(CapHit),
                     sp = paste0('<span class="big', ifelse(CapSpace < 0, " neg", ""), '">', money_s(CapSpace), '</span>'))
  cap <- mtable(d, list(Team = "team", "Wk F1" = "wf", "Ssn F1" = "sf", "Soc" = "so", "Trd" = "tr", "Total" = "tc", "Sal" = "sa", "Dead" = "dh", "Space" = "sp"),
                left = 1, cls = "cap", widths = c(NA, "2.3rem", "2.4rem", "1.9rem", "2rem", "2.3rem", "2.5rem", "2.1rem", "3.1rem"),
                groups = list(c(" ", 1), c("2027 Cap", 5), c("Costs", 2), c(" ", 1)))
  soc_note <- if (all(cc$TotalF1 <= 100)) " No team is over 100 yet." else ""
  tg <- D$tags
  tiles <- div(class = "tiles four tag-tiles", lapply(c("QB", "RB", "WR", "TE"), function(p) tile(paste(p, "tag"), paste0("$", tg$tag[tg$pos == p]))))
  ppg <- D$lu %>% filter(score != 0) %>% count(player_id, name = "g")
  pr <- if (!is.null(D$ytd)) D$ytd else D$lu %>% group_by(player_id) %>% summarise(ytd = sum(score))
  allp <- D$ros %>% filter(pos %in% SKILL) %>% select(player_id, pos) %>% distinct() %>% inner_join(pr, by = "player_id") %>%
    group_by(pos) %>% mutate(prk = rank(-ytd, ties.method = "min")) %>% ungroup()
  nfl <- D$lu %>% distinct(player_id, nfl)
  fa <- D$fas %>% left_join(pr, by = "player_id") %>% left_join(ppg, by = "player_id") %>%
    left_join(allp %>% select(player_id, prk), by = "player_id") %>% left_join(nfl, by = "player_id") %>%
    mutate(ytd = coalesce(ytd, 0), g = coalesce(g, 1L), nfl = coalesce(nfl, norm_team(team))) %>% arrange(desc(ytd)) %>% head(14) %>%
    left_join(tg, by = "pos") %>%
    mutate(n = row_number(), posc = paste0('<span class="slot-g">', pos, '</span>'),
           pl = pmap_chr(list(player_name, nfl, Name), function(n, t, o) team_cell(short_name(n), paste0(t, " · ", o))),
           stc = ifelse(Kind == "pickup", "Pickup", "Expiring"),
           sal = ifelse(is.na(salary) | Kind == "pickup", nil, paste0("$", salary)),
           tagc = paste0('<span class="gold">$', tag, '</span>'), ppgc = paste0('<span class="big">', fmt1(ytd / g), '</span>'), rk = paste0(pos, prk))
  fat <- mtable(fa, list("#" = "n", Pos = "posc", Player = "pl", Status = "stc", "2026 $" = "sal", "Tag $" = "tagc", PPG = "ppgc", Rank = "rk"), left = 3,
                cls = "fa", widths = c("1.3rem", "2rem", NA, "3.6rem", "2.6rem", "2.4rem", "2.4rem", "2.4rem"))
  tagList(
    sec("cap", "2027 Cap Check", cap,
        div(class = "note", paste0("As if the season ended today. Total cap = $230 + weekly F1 + seasonal F1 (by current standing) + socialism + traded. Socialism: total F1 over 100 is shared with teams under 100.", soc_note))),
    sec("fa", "2027 Free Agents", tiles, fat,
        div(class = "note", "Ranked by 2026 points. Pickup: added this season, no contract. Tag: avg of top 3 QB/TE or top 5 RB/WR salaries.")))
}

# ---- 5. Front Office ---------------------------------------------------------------------------
section_office <- function(D) {
  W <- D$WEEK
  wk1 <- as.POSIXct(paste(SEASON_WEEK1_THU, "20:00"), tz = "America/New_York")
  nm <- setNames(D$teams$Name, D$teams$franchise_id)
  tx <- D$tx %>% mutate(when = as.POSIXct(timestamp, tz = "America/New_York"),
                        wk = ifelse(when < wk1, 0L, as.integer(floor(as.numeric(difftime(when, wk1, units = "days")) / 7)) + 1L),
                        Name = nm[franchise_id])
  tw <- tx %>% filter(wk == W, type_desc %in% c("added", "dropped", "traded_for"))
  line <- function(x) {
    sym <- c(added = "+", dropped = "−", traded_for = "⇄")[x$type_desc]; cls <- c(added = "add", dropped = "drop", traded_for = "trade")[x$type_desc]
    right <- ifelse(x$type_desc == "traded_for", paste("from", nm[x$trade_partner] %||% ""),
                    ifelse(x$type_desc == "dropped", "", ifelse(is.na(x$bbid_spent), "FA", paste0("$", formatC(x$bbid_spent, format = "f", digits = 2)))))
    lapply(seq_len(nrow(x)), function(i) tags$li(class = cls[i], span(HTML(paste0(sym[i], " ", htmlEscape(short_name(x$player_name[i])), " <small>", x$pos[i], "</small>"))), tags$small(right[i])))
  }
  movers <- sort(unique(tw$Name))
  cards <- lapply(movers, function(n) { d <- tw %>% filter(Name == n) %>% arrange(match(type_desc, c("traded_for", "added", "dropped")))
    div(class = "mv-card", div(class = "mv-name", n), tags$ul(line(d))) })
  quiet <- setdiff(D$teams$Name, movers)
  # traded cap
  net <- D$trd$net; yrs <- setdiff(names(net), "Name")
  net <- net %>% arrange(desc(.data[[yrs[1]]]))
  for (y in yrs) net[[paste0("c", y)]] <- signed_c(net[[y]])
  tct <- mtable(net, setNames(as.list(c("Name", paste0("c", yrs))), c("Team", yrs)), left = 1, widths = c(NA, rep("3.6rem", length(yrs))))
  nw <- D$trd$new
  newbox <- div(class = "newbox", div(class = "soon-k", "New this week"),
                if (is.null(nw) || !nrow(nw)) span("None this week")
                else tags$ul(lapply(split(nw, paste(nw$Sender, nw$Receiver)), function(g)
                  tags$li(sprintf("%s → %s %s", g$Sender[1], g$Receiver[1], paste0("$", vapply(g$Amount, fmtpt, ""), " (", g$Year, ")", collapse = ", "))))))
  # best pickups (2026 adds, offseason included)
  bp <- D$moves %>% filter(how == "Waiver") %>% head(10) %>%
    mutate(n = row_number(), posc = paste0('<span class="slot-g">', pos, '</span>'),
           added = ifelse(when < wk1, format(when, "%b"), paste("Wk", from_week)),
           pl = pmap_chr(list(player_name, Name, added), function(p, o, a) team_cell(short_name(p), paste0(o, " · added ", a))),
           bidc = ifelse(is.na(bid), "FA", paste0("$", ifelse(bid == round(bid), bid, formatC(bid, format = "f", digits = 1)))),
           up = paste0('<span class="big">', fmt1(Used), '</span>'))
  bpt <- mtable(bp, list("#" = "n", Pos = "posc", Player = "pl", Bid = "bidc", Pts = "up"), left = 3, widths = c("1.7rem", "2rem", NA, "3rem", "3rem"))
  # FAAB and activity
  act <- tx %>% group_by(franchise_id) %>% summarise(trades = n_distinct(timestamp[type == "TRADE"]), adds = sum(type_desc == "added"), .groups = "drop")
  fa <- D$teams %>% select(franchise_id, Name) %>% left_join(D$faab, by = "franchise_id") %>% left_join(act, by = "franchise_id") %>%
    mutate(across(c(trades, adds), ~ coalesce(.x, 0L)), faab = coalesce(faab, 0)) %>% arrange(desc(faab)) %>%
    mutate(bar = sprintf('<span class="bar wide"><i style="width:%.0f%%"></i></span>', pmin(100, faab)),
           left = paste0('<span class="big">$', formatC(faab, format = "f", digits = 2), '</span>'))
  fat <- mtable(fa, list(Team = "Name", " " = "bar", Left = "left", Trades = "trades", Adds = "adds"), left = 2, widths = c("5.2rem", NA, "4rem", "2.8rem", "2.4rem"))
  tagList(
    sec("moves", sprintf("Week %d Moves", W), kicker = sprintf("Week %d", W),
        if (length(cards)) div(class = "mv-grid", cards) else p(class = "dek", "No moves this week."),
        div(class = "note", paste0("+ added  − dropped  ⇄ traded for. Waiver bid on the right; FA = no bid.",
                                   if (length(quiet)) paste0(" No moves: ", and_list(quiet), ".") else ""))),
    sec("traded", "Traded Cap", tct, div(class = "note", "Net cap money received (+) or sent (−) in trades, by cap year."), newbox),
    sec("pickups", "Best Pickups", bpt, div(class = "note", "2026 waiver and free agent adds, ranked by points they scored in their team's best-ball lineup since the add (Pts).")),
    sec("faab", "FAAB and Activity", fat, div(class = "note", "FAAB left out of $100. Trades and adds for the 2026 league year, offseason included.")))
}

# ---- 6. Team Pages ---------------------------------------------------------------------------
team_history <- function(D) {
  w <- bind_rows(D$hall %>% select(year, week, owner, score, f1), D$res %>% transmute(year = SEASON, week, owner = Name, score = points, f1 = F1Pts))
  fin <- w %>% filter(year < SEASON) %>% group_by(year, owner) %>% summarise(F1 = sum(f1), PF = sum(score), .groups = "drop") %>%
    group_by(year) %>% arrange(desc(F1), desc(PF), .by_group = TRUE) %>% mutate(fin = row_number()) %>% ungroup() %>%
    bind_rows(D$st %>% transmute(year = SEASON, owner = Name, F1 = F1Pts, fin = Pos))
  wins <- w %>% group_by(year, week) %>% filter(score == max(score)) %>% ungroup() %>% count(owner, name = "wins")
  list(fin = fin, wins = wins, w = w)
}

section_teams <- function(D) {
  W <- D$WEEK; odds <- D$ohist %>% filter(week == W); H <- team_history(D)
  slug <- function(n) paste0("t-", tolower(gsub("[^A-Za-z]", "", n)))
  chips <- div(class = "team-jump", lapply(D$st$Name, function(n) tags$a(href = paste0("#", slug(n)), n)))
  pages <- lapply(seq_len(nrow(D$st)), function(i) {
    s <- D$st[i, ]; n <- s$Name
    r <- D$res %>% filter(Name == n) %>% arrange(week)
    t5 <- D$lu %>% filter(Name == n, used) %>% group_by(player_name, pos) %>% summarise(p = sum(score), .groups = "drop") %>% slice_max(p, n = 5, with_ties = FALSE)
    o <- odds %>% filter(Name == n); cap <- D$cc %>% filter(Name == n); fb <- D$faab$faab[D$faab$franchise_id == s$franchise_id]
    best <- r %>% slice_max(points, n = 1, with_ties = FALSE)
    fh <- H$fin %>% filter(owner == n) %>% arrange(year); done <- fh %>% filter(year < SEASON)
    nw <- H$wins$wins[H$wins$owner == n]; nw <- if (length(nw)) nw else 0
    band <- function(k) if (k == 1) "win" else if (k <= 3) "pod" else if (k <= 6) "pts" else "out"
    tags$section(class = "tp-page", id = slug(n),
      div(class = "tp-kick", sprintf("Fantasy1 Championship · Week %d · Team Pages", W)),
      div(class = "tp-owner", n), div(class = "tp-fr", sprintf("%s · %s in the standings", s$Franchise, ordinal(s$Pos))),
      div(class = "tiles five", tile("F1", paste0('<span class="gold">', fmtpt(s$F1Pts), '</span>')), tile("Wins", s$Wins), tile("Podiums", s$Podiums),
          tile("Pts fin.", s$PtsFin), tile("Won", money_s(s$Earnings))),
      h3(class = "rs-sub", "Week by week"),
      div(class = "chips", lapply(seq_len(nrow(r)), function(j) div(class = "chip-w", span(paste0("W", r$week[j])),
          div(class = paste("chip", band(ceiling(r$rank[j]))), finish_lbl(r$rank[j])), span(class = "chip-s", fmt1(r$points[j]))))),
      div(class = "tp-cols",
          div(h3(class = "rs-sub", "Top scorers"),
              div(class = "tp-list", lapply(seq_len(nrow(t5)), function(j) div(class = "tp-row", span(short_name(t5$player_name[j]), tags$small(t5$pos[j])), tags$b(fmt1(t5$p[j])))))),
          div(h3(class = "rs-sub", "Outlook"),
              div(class = "tp-list",
                  div(class = "tp-row", span("Title odds"), tags$b(pct_txt(o$Title))),
                  div(class = "tp-row", span("Top 3 odds"), tags$b(pct_txt(o$Top3))),
                  div(class = "tp-row", span("2027 cap space"), tags$b(money_s(cap$CapSpace))),
                  div(class = "tp-row", span("FAAB left"), tags$b(if (length(fb)) paste0("$", formatC(fb, format = "f", digits = 2)) else "–")),
                  div(class = "tp-row", span("Best week"), tags$b(sprintf("%s (Wk %d)", fmt1(best$points), best$week)))))),
      h3(class = "rs-sub", "Finish by season"),
      div(class = "seasons", lapply(2022:SEASON, function(y) { f <- fh$fin[fh$year == y]
        div(class = paste("season", if (length(f) && f == 1 && y < SEASON) "champ"), span(y), tags$b(if (length(f)) paste0(ordinal(f), if (y == SEASON) "*" else "") else "–")) })),
      div(class = "note", sprintf("%d season%s · %d title%s · %d runner-up%s · %d career weekly win%s. *%d is current standing.",
                                  nrow(fh), if (nrow(fh) == 1) "" else "s", sum(done$fin == 1), if (sum(done$fin == 1) == 1) "" else "s",
                                  sum(done$fin == 2), if (sum(done$fin == 2) == 1) "" else "s", nw, if (nw == 1) "" else "s", SEASON)))
  })
  tagList(chips, pages)
}

# ---- 7. All-Time -------------------------------------------------------------------------------
section_alltime <- function(D) {
  H <- team_history(D)
  w <- H$w %>% group_by(year, week) %>% mutate(rk = rank(-score, ties.method = "min")) %>% ungroup()
  fin <- H$fin %>% filter(year < SEASON)
  car <- w %>% group_by(owner) %>% summarise(F1 = sum(f1), wins = sum(rk == 1), pods = sum(rk <= 3), y0 = min(year), y1 = max(year), .groups = "drop") %>%
    left_join(fin %>% group_by(owner) %>% summarise(t = sum(fin == 1), s2 = sum(fin == 2), s3 = sum(fin == 3), avg = mean(fin), .groups = "drop"), by = "owner") %>%
    mutate(across(c(t, s2, s3), ~ coalesce(.x, 0L))) %>% arrange(desc(F1)) %>%
    mutate(rank = row_number(), span_ = ifelse(y0 == y1, yr2(y0), paste0(yr2(y0), "–", yr2(y1))),
           oc = team_cell(paste0(rank, ". ", owner), span_),
           tc = ifelse(t > 0, strrep("\U0001F3C6", t), nil), s2c = num(s2), s3c = num(s3),
           avgc = ifelse(is.na(avg), nil, sprintf("%.1f", avg)), f1c = paste0('<span class="big gold">', vapply(F1, fmtpt, ""), '</span>'))
  at <- mtable(car, list(Owner = "oc", Titles = "tc", "2nd" = "s2c", "3rd" = "s3c", "Avg" = "avgc", Wins = "wins", Pod = "pods", "F1" = "f1c"), left = 1,
               cls = "at", widths = c(NA, "2.6rem", "1.7rem", "1.7rem", "2.4rem", "2.2rem", "2.1rem", "3.2rem"))
  champs <- fin %>% filter(fin <= 3) %>% arrange(desc(year), fin)
  cards <- lapply(sort(unique(champs$year), decreasing = TRUE), function(y) {
    d <- champs %>% filter(year == y)
    div(class = "ch-card", div(class = "ch-year", y), div(class = "ch-name", HTML(paste0("\U0001F3C6 ", htmlEscape(d$owner[1]))), span(paste(fmtpt(d$F1[1]), "F1"))),
        div(class = "ch-rest", sprintf("2nd %s (%s) · 3rd %s (%s)", d$owner[2], fmtpt(d$F1[2]), d$owner[3], fmtpt(d$F1[3]))))
  })
  tagList(
    sec("alltime", "All-Time Standings", at,
        div(class = "note", sprintf("Career F1 = weekly F1 points, 2022 to %d so far. 2024 scored the top 5 only (16/11/7/4/1). Avg fin = average final finish in completed seasons.", SEASON))),
    sec("champions", "Champions", div(class = "ch-grid", cards),
        div(class = "note", "2024 was a 10-team league that scored the top 5 each week (16/11/7/4/1).")))
}

section_body <- function(page, D) switch(page, index = section_standings(D), review = section_review(D), sims = section_sims(D),
                                         money = section_money(D), office = section_office(D), teams = section_teams(D), alltime = section_alltime(D))
