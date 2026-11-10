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
    season_heat(D),
    score_range(D),
    title_math(D),
    sec("pairwise", "Pairwise Rank", p(class = "dek", "Each team's record against every other score, every week, and where that ranks."), pairwise_chart(D),
        div(class = "note", "Pairwise: each weekly score compared with the other 11 teams' scores that week.")),
    sec("h2h", "Head to Head", p(class = "dek", "Row team's record against each column team, every week this season. Sorted by pairwise win %."), h2h_chart(D),
        div(class = "note", "Weeks the row team outscored the column team, all season. Ties count as half a win.")),
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
        div(class = "fact-line", HTML(pline))),
    top_weekly_scores(D))
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
    if (SHOW_ODDS_MOVERS) odds_movers(D),
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
  # 2028: salary already under contract for 2028 (contract_years >= 2), plus 2028 traded cap
  c28 <- D$ros %>% group_by(franchise_id) %>% summarise(Sal28 = sum(salary[coalesce(contract_years, 0) >= 2], na.rm = TRUE),
                                                        Signed28 = sum(coalesce(contract_years, 0) >= 2), .groups = "drop")
  t28 <- if ("2028" %in% names(D$trd$net)) D$trd$net %>% transmute(Name, Trd28 = .data[["2028"]]) else tibble(Name = cc$Name, Trd28 = 0)
  cc <- cc %>% left_join(c28, by = "franchise_id") %>% left_join(t28, by = "Name") %>% mutate(across(c(Sal28, Signed28, Trd28), ~ coalesce(.x, 0)))
  d <- cc %>% mutate(team = pmap_chr(list(Name, Signed, Signed28), function(n, s, s8) team_cell(n, sprintf("%d signed '27 · %d '28", s, s8))),
                     s28 = paste0("$", Sal28, ifelse(Trd28 != 0, paste0('<span class="ts">', ifelse(Trd28 > 0, "+", "−"), abs(Trd28), " trd</span>"), "")),
                     wf = num1(F1Pts), sf = num1(Seasonal), so = num1(Socialism), tr = num1(Traded),
                     tc = sub("\\.0$", "", fmt1(TotalCap)), sa = paste0("$", Salary), dh = num1(CapHit),
                     sp = paste0('<span class="big', ifelse(CapSpace < 0, " neg", ""), '">', money_s(CapSpace), '</span>'))
  cap <- mtable(d, list(Team = "team", "Wk F1" = "wf", "Ssn F1" = "sf", "Soc" = "so", "Trd" = "tr", "Total" = "tc", "Sal" = "sa", "Dead" = "dh", "Space" = "sp", "2028 $" = "s28"),
                left = 1, cls = "cap c28", widths = c(NA, "2.3rem", "2.4rem", "1.9rem", "2rem", "2.3rem", "2.5rem", "2.1rem", "3.1rem", "2.7rem"),
                groups = list(c(" ", 1), c("2027 Cap", 5), c("Costs", 2), c(" ", 1), c("2028", 1)))
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
    sec("cap", "2027 Cap Check", div(class = "scroll-x", cap),
        div(class = "note", paste0("As if the season ended today. Total cap = $230 + weekly F1 + seasonal F1 (by current standing) + socialism + traded. Socialism: total F1 over 100 is shared with teams under 100.", soc_note,
                                   " 2028 $: salary already under contract for 2028 (at 2026 salary), with 2028 traded cap below it."))),
    contract_timeline(D),
    contract_value(D),
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
    traded_ledger(D),
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
  W <- D$WEEK; odds <- D$ohist %>% filter(week == W); H <- team_history(D); rs <- roster_stats(D); ps <- point_share(D)
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
      point_share_team(ps, s$franchise_id),
      h3(class = "rs-sub", "Finish by season"),
      div(class = "seasons", lapply(2022:SEASON, function(y) { f <- fh$fin[fh$year == y]
        div(class = paste("season", if (length(f) && f == 1 && y < SEASON) "champ"), span(y), tags$b(if (length(f)) paste0(ordinal(f), if (y == SEASON) "*" else "") else "–")) })),
      div(class = "note", sprintf("%d season%s · %d title%s · %d runner-up%s · %d career weekly win%s. *%d is current standing.",
                                  nrow(fh), if (nrow(fh) == 1) "" else "s", sum(done$fin == 1), if (sum(done$fin == 1) == 1) "" else "s",
                                  sum(done$fin == 2), if (sum(done$fin == 2) == 1) "" else "s", nw, if (nw == 1) "" else "s", SEASON)),
      outlook_block(D, s$franchise_id),
      roster_block(rs %>% filter(franchise_id == s$franchise_id)))
  })
  tagList(position_strength(D), point_share_league(D), point_share_weeks(D), league_leaders(D), chips, pages)
}

# Weeks each current player has been on his current team, IR and bye weeks included. MFL's weekly rows
# leave out IR players, so the weeks come from add/drop/trade history: a move counts from the first week
# whose Sunday 1pm ET slate starts after it (Wednesday waivers land the next week). Weekly rows are unioned in.
roster_weeks <- function(D) {
  W <- D$WEEK
  sun <- as.numeric(as.POSIXct(paste(SEASON_WEEK1_THU, "20:00"), tz = "America/New_York")) + (seq_len(W) - 1) * 7 * 86400 + 65 * 3600
  ev <- D$tx %>% filter(type_desc %in% c("added", "traded_for", "dropped", "traded_away")) %>%
    transmute(player_id, franchise_id, when = as.numeric(timestamp), join = type_desc %in% c("added", "traded_for"))
  cur <- D$ros %>% filter(pos %in% SKILL) %>% distinct(player_id, franchise_id)
  wk <- cur %>% left_join(ev, by = c("player_id", "franchise_id"), relationship = "many-to-many") %>%
    group_by(player_id, franchise_id) %>% arrange(when, .by_group = TRUE) %>%
    reframe(week = seq_len(W), on = {
      start <- if (all(is.na(when))) TRUE else !join[1]   # no moves, or first move a drop/trade away: on the team from week 1
      vapply(sun, function(t) { k <- which(when < t); if (length(k)) join[max(k)] else start }, logical(1))
    })
  played <- D$lu %>% distinct(player_id, franchise_id, week) %>% mutate(lu = TRUE)
  wk %>% left_join(played, by = c("player_id", "franchise_id", "week")) %>%
    filter(on | coalesce(lu, FALSE)) %>% count(player_id, franchise_id, name = "Weeks")
}

roster_stats <- function(D) {
  st <- D$lu %>% group_by(player_id, franchise_id) %>%
    summarise(Starts = sum(used), Lineup = sum(score[used]), Total = sum(score), G = sum(score != 0), nfl_lu = last(nfl), .groups = "drop")
  D$ros %>% filter(pos %in% SKILL) %>%
    left_join(roster_weeks(D), by = c("player_id", "franchise_id")) %>%
    left_join(st, by = c("player_id", "franchise_id")) %>%
    mutate(across(c(Weeks, Starts, G), ~ coalesce(as.integer(.x), 0L)), across(c(Lineup, Total), ~ coalesce(.x, 0)),
           nfl = coalesce(nfl_lu, norm_team(team)),
           Contract = case_when(is.na(contract_years) ~ "Pickup", contract_years == 0 ~ "Expiring", TRUE ~ paste("thru", SEASON + contract_years)))
}

# One table per team: QB / RB / WR / TE groups, each sorted by lineup points. TOTAL drops first on phones.
roster_block <- function(r) {
  r <- r %>% mutate(pos = factor(pos, SKILL)) %>% arrange(pos, desc(Lineup), desc(Total))
  td <- function(x, cls = "r") tags$td(class = cls, HTML(x))
  hd <- c(Player = "", "2026 $" = "r", Contract = "r", Starts = "r", "Lineup pts" = "r", "Total pts" = "r tot", PPG = "r")
  wd <- c(NA, "2rem", "3.7rem", "2.3rem", "2.7rem", "2.5rem", "2.1rem")   # on the header cells, so hiding TOTAL frees its width
  body <- lapply(SKILL, function(p) {
    g <- r %>% filter(pos == p); if (!nrow(g)) return(NULL)
    tagList(tags$tr(class = "rg", tags$td(colspan = length(hd) - 1, p), tags$td(class = "tot")),
      lapply(seq_len(nrow(g)), function(i) { x <- g[i, ]
        tags$tr(td(team_cell(short_name(x$player_name), x$nfl), ""), td(if (is.na(x$salary)) nil else paste0("$", x$salary)), td(x$Contract, "r nw"),
                td(sprintf("%d/%d", x$Starts, x$Weeks)), td(paste0('<span class="lp">', fmt1(x$Lineup), '</span>')),
                td(fmt1(x$Total), "r tot"), td(if (x$G) fmt1(x$Total / x$G) else nil)) }))
  })
  tagList(h3(class = "rs-sub", "Roster"),
    tags$table(class = "mt ros", `data-quarto-disable-processing` = "true",
      tags$thead(tags$tr(lapply(seq_along(hd), function(j) tags$th(class = hd[[j]], style = if (!is.na(wd[j])) paste0("width:", wd[j]), names(hd)[j])))),
      tags$tbody(body)),
    div(class = "note", "Starts and lineup points count weeks in the best-ball lineup (QB1 RB2 WR3 TE1 FLEX2)."))
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

# ---- Standings: Pairwise Rank and Head to Head -------------------------------------------------
# Every weekly score set against the other 11 that week. Ties count half.
pairwise <- function(res, W) {
  d <- res %>% filter(week <= W) %>% select(week, Name, points)
  h2h <- d %>% inner_join(d, by = "week", suffix = c("", "_o"), relationship = "many-to-many") %>% filter(Name != Name_o) %>%
    group_by(Name, Name_o) %>% summarise(w = sum(points > points_o) + .5 * sum(points == points_o), n = n(), .groups = "drop")
  rec <- h2h %>% group_by(Name) %>% summarise(W = sum(w), L = sum(n) - sum(w), .groups = "drop") %>%
    mutate(pct = W / (W + L), prank = rank(-pct, ties.method = "min"))
  list(h2h = h2h, rec = rec)
}
half <- function(x) ifelse(x == floor(x), format(floor(x), trim = TRUE), paste0(ifelse(floor(x) > 0, floor(x), ""), "½"))
wl <- function(w, l) paste0(half(w), "-", half(l))

pairwise_chart <- function(D) {
  pw <- pairwise(D$res, D$WEEK)$rec %>% left_join(D$st %>% select(Name, Pos), by = "Name") %>%
    mutate(Name = factor(Name, levels = rev(D$st$Name)))
  p <- ggplot(pw, aes(y = Name)) +
    geom_segment(aes(x = Pos, xend = prank, yend = Name), colour = "#4a4a4a", linewidth = 1.1) +
    geom_point(aes(x = prank), colour = "#F2F2F2", size = 3) +
    geom_point(aes(x = Pos), colour = GOLD, size = 3.4) +
    geom_text(aes(x = 14.6, label = wl(W, L)), colour = "#d6d6d6", size = 3.1, family = "Inter", hjust = 1) +
    scale_x_continuous(breaks = c(1, 3, 6, 9, 12), labels = c("1st", "3rd", "6th", "9th", "12th"), limits = c(.6, 14.7), expand = c(0, 0), position = "top") +
    labs(x = NULL, y = NULL) + theme_gts() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = "#e8e8e8", size = 10, face = "bold"),
          axis.text.x = element_text(size = 8.5))
  tagList(div(class = "key", HTML(sprintf('<span><i class="sw" style="background:%s;border-radius:50%%"></i>F1 rank</span><span><i class="sw" style="background:#F2F2F2;border-radius:50%%"></i>Pairwise rank</span><span>Right: Pairwise W-L</span>', GOLD))),
          svg_chart(p, w = 4.4, h = .8 + .27 * nrow(pw)))
}

h2h_chart <- function(D) {
  pw <- pairwise(D$res, D$WEEK); ord <- pw$rec %>% arrange(desc(pct), Name) %>% pull(Name)
  m <- pw$h2h %>% mutate(share = w / n, lbl = wl(w, n - w), Name = factor(Name, levels = rev(ord)), Name_o = factor(Name_o, levels = c(ord, "Pairwise")))
  tot <- pw$rec %>% transmute(Name = factor(Name, levels = rev(ord)), lbl = sprintf("%s
%d%%", wl(W, L), round(100 * pct)))
  p <- ggplot(m, aes(Name_o, Name)) +
    geom_tile(aes(fill = share), colour = "#0a0a0a", linewidth = .8) +
    geom_text(aes(label = lbl), colour = "#f2f2f2", size = 2.45, family = "Inter", fontface = "bold") +
    geom_text(data = tot, aes(x = 13.3, y = Name, label = lbl), colour = "#ffffff", size = 2.3, lineheight = .85, family = "Inter", fontface = "bold", inherit.aes = FALSE) +
    scale_fill_gradient2(low = "#8E3B36", mid = "#262626", high = "#2F7A4D", midpoint = .5, limits = c(0, 1)) +
    scale_x_discrete(drop = FALSE, labels = c(ord, "Pairwise W-L / %"), position = "top", expand = expansion(add = c(.5, 1))) +
    labs(x = NULL, y = NULL) + theme_gts() +
    theme(panel.grid = element_blank(), axis.text.x.top = element_text(angle = 55, hjust = 0, vjust = 0, size = 7.5, colour = "#bdbdbd"),
          axis.text.y = element_text(colour = "#e8e8e8", size = 7.5, face = "bold"), plot.margin = margin(4, 26, 2, 0))
  svg_chart(p, w = 4.1, h = 4.6)
}

# ---- Team Pages: Position Strength (league grid) ------------------------------------------------
# "Deep": healthy rostered players among the league's top 12 QB / 24 RB / 36 WR / 12 TE by points per game (2+ games)
STARTABLE_N <- c(QB = 12, RB = 24, WR = 36, TE = 12)
out_ids <- function(D) {
  o <- D$ros$player_id[D$ros$roster_status == "INJURED_RESERVE"]
  if (!is.null(D$inj)) o <- c(o, D$inj$player_id[D$inj$out])
  unique(o)
}
startable <- function(D) {
  ppg <- D$lu %>% distinct(week, player_id, .keep_all = TRUE) %>% group_by(player_id) %>%
    summarise(g = sum(score != 0), ppg = ifelse(g > 0, sum(score) / g, 0), .groups = "drop")
  D$ros %>% filter(pos %in% SKILL, !player_id %in% out_ids(D)) %>% left_join(ppg, by = "player_id") %>%
    filter(coalesce(g, 0L) >= 2) %>% group_by(pos) %>% arrange(desc(ppg), .by_group = TRUE) %>%
    filter(row_number() <= STARTABLE_N[first(pos)]) %>% ungroup() %>% count(franchise_id, pos, name = "deep")
}

position_strength <- function(D) {
  slots <- c(SKILL, "FLEX")
  base <- D$lu %>% filter(used, !is.na(slot)) %>% group_by(franchise_id, slot) %>% summarise(p = sum(score), .groups = "drop")
  g <- D$teams %>% select(franchise_id, Name) %>% tidyr::crossing(slot = slots) %>%
    left_join(base, by = c("franchise_id", "slot")) %>% mutate(p = coalesce(p, 0))
  g <- bind_rows(g, g %>% group_by(franchise_id, Name) %>% summarise(p = sum(p), .groups = "drop") %>% mutate(slot = "Total")) %>%
    group_by(slot) %>% mutate(rk = rank(-p, ties.method = "min")) %>% ungroup() %>%
    left_join(startable(D), by = c("franchise_id", slot = "pos")) %>%
    mutate(cell = sprintf('<span class="pv">%s</span><span class="pr">%s%s</span>', round(p), vapply(rk, ordinal, ""),
                          ifelse(slot %in% SKILL, paste0(" · ", coalesce(deep, 0L)), "")),
           cls = case_when(rk <= 3 ~ "r ps top", rk >= 10 ~ "r ps low", TRUE ~ "r ps"))
  ord <- g %>% filter(slot == "Total") %>% arrange(rk) %>% pull(Name)
  rows <- lapply(ord, function(n) { x <- g %>% filter(Name == n)
    tags$tr(tags$td(span(class = "tn", n)), lapply(c(slots, "Total"), function(s) tags$td(class = x$cls[x$slot == s], HTML(x$cell[x$slot == s])))) })
  sec("strength", "Position Strength",
      tags$table(class = "mt pstr", `data-quarto-disable-processing` = "true",
                 tags$thead(tags$tr(tags$th("Team"), lapply(c(slots, "Total"), function(s) tags$th(class = "r", s)))), tags$tbody(rows)),
      div(class = "note", "Season lineup points by best-ball slot. Rank among 12 teams. Second number under QB/RB/WR/TE: healthy rostered players among the league's top 12 QB, 24 RB, 36 WR and 12 TE by points per game (2+ games)."))
}

# ---- Team Pages: Bye and Injury Outlook ------------------------------------------------------
outlook_block <- function(D, fid, n_weeks = 4) {
  W <- D$WEEK; out <- out_ids(D)
  r <- D$ros %>% filter(franchise_id == fid, pos %in% SKILL) %>% mutate(team = norm_team(team))
  r <- if (!is.null(D$byes)) r %>% left_join(D$byes, by = "team") else r %>% mutate(bye = NA_integer_)
  hl <- r %>% filter(!player_id %in% out)
  wks <- seq(W + 1, min(W + n_weeks, LAST_WEEK))
  bye_rows <- lapply(wks, function(w) {
    b <- hl %>% filter(bye %in% w)
    div(class = "tp-row", span(tags$b(paste0("Wk ", w)), tags$small(if (nrow(b)) paste(sprintf("%s %s", short_name(b$player_name), b$pos), collapse = ", ") else "no byes")))
  })
  inj <- if (!is.null(D$inj)) r %>% inner_join(D$inj %>% filter(out), by = "player_id") else r[0, ]
  ir_only <- r %>% filter(roster_status == "INJURED_RESERVE", !player_id %in% inj$player_id) %>%
    mutate(status = "IR", details = NA_character_, exp_return = NA_character_)
  inj <- bind_rows(inj, ir_only) %>% arrange(match(pos, SKILL))
  inj_rows <- if (nrow(inj)) lapply(seq_len(nrow(inj)), function(i) { x <- inj[i, ]
    ret <- if (!is.na(x$exp_return) && nzchar(x$exp_return)) paste("back", sub(", 20\\d\\d$", "", x$exp_return))
    div(class = "tp-row", span(short_name(x$player_name), tags$small(paste(c(x$pos, if (!is.na(x$details)) x$details), collapse = " · "))),
        tags$b(paste(c(x$status, ret), collapse = " · "))) })
    else list(div(class = "tp-row", span("None"), HTML(nil)))
  tagList(h3(class = "rs-sub", "Bye and Injury Outlook"),
    div(class = "tp-cols",
        div(div(class = "ol-k", sprintf("Byes, weeks %d–%d", min(wks), max(wks))), div(class = "tp-list", bye_rows)),
        div(div(class = "ol-k", "IR, Out, Doubtful"), div(class = "tp-list", inj_rows))))
}

# ---- Money & Cap: Contract Value ------------------------------------------------------------------
POS_COLS <- c(QB = GOLD, RB = "#82E0AA", WR = "#85C1E9", TE = "#E59866")
contract_value <- function(D) {
  v <- roster_stats(D) %>% filter(!is.na(contract_years), salary > 0) %>%
    left_join(D$teams %>% select(franchise_id, Name), by = "franchise_id") %>% mutate(ppd = Lineup / salary)
  v$res <- resid(lm(Lineup ~ salary, data = v))
  lab <- bind_rows(v %>% slice_max(res, n = 5, with_ties = FALSE) %>% mutate(up = TRUE),
                   v %>% slice_min(res, n = 5, with_ties = FALSE) %>% mutate(up = FALSE)) %>%
    mutate(txt = sprintf("%s (%s)", sub(",.*$", "", player_name), Name))
  p <- ggplot(v, aes(salary, Lineup)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "#555555", linewidth = .6, linetype = "22") +
    geom_point(aes(colour = pos), size = 1.9, alpha = .85) +
    ggrepel::geom_text_repel(data = lab %>% filter(up), aes(label = txt), colour = "#e8e8e8", size = 2.7, family = "Inter",
                             nudge_y = 25, segment.colour = "#444", min.segment.length = 0, seed = 1, max.overlaps = Inf) +
    ggrepel::geom_text_repel(data = lab %>% filter(!up), aes(label = txt), colour = "#9a9a9a", size = 2.7, family = "Inter",
                             nudge_y = -25, segment.colour = "#444", min.segment.length = 0, seed = 1, max.overlaps = Inf) +
    scale_colour_manual(values = POS_COLS) + scale_x_continuous(labels = function(x) paste0("$", x)) +
    labs(x = "2026 salary", y = "Lineup points") + theme_gts()
  lst <- function(d) mtable(d %>% mutate(pl = pmap_chr(list(player_name, Name), function(n, o) team_cell(short_name(n), o)),
                                         sal = paste0("$", salary), lp = paste0('<span class="big">', fmt1(Lineup), '</span>')),
                            list(Player = "pl", "$" = "sal", "Lineup pts" = "lp"), left = 1, cls = "cap", widths = c(NA, "2.4rem", "4.2rem"))
  sec("value", "Contract Value",
      div(class = "key", HTML(paste(sprintf('<span><i class="sw" style="background:%s;border-radius:50%%"></i>%s</span>', POS_COLS, names(POS_COLS)), collapse = ""))),
      svg_chart(p, w = 4.1, h = 3.6),
      div(class = "tp-cols",
          div(h3(class = "rs-sub", "Best value"), lst(v %>% arrange(desc(ppd)) %>% head(5))),
          div(h3(class = "rs-sub", "Least value ($10+)"), lst(v %>% filter(salary >= 10) %>% arrange(ppd, desc(salary)) %>% head(5)))),
      div(class = "note", "Lineup points per dollar of 2026 salary. Pickups excluded. Lineup points are with the current team. Labels: the five furthest above and below the trend line."))
}

# ---- Standings: Score Range ---------------------------------------------------------------------
score_range <- function(D) {
  d <- D$res %>% filter(week <= D$WEEK)
  med <- d %>% group_by(Name) %>% summarise(m = median(points), .groups = "drop") %>% arrange(m)
  d <- d %>% mutate(Name = factor(Name, levels = med$Name))
  lg <- median(d$points)
  p <- ggplot(d, aes(points, Name)) +
    geom_vline(xintercept = lg, colour = "#555555", linetype = "22", linewidth = .5) +
    geom_point(colour = "#bdbdbd", alpha = .7, size = 1.9) +
    geom_point(data = med %>% mutate(Name = factor(Name, levels = med$Name)), aes(m, Name), shape = 124, size = 6, colour = GOLD) +
    annotate("text", x = lg, y = nrow(med) + .7, label = paste("League median", fmt1(lg)), colour = "#8a8a8a", size = 2.7, family = "Inter") +
    scale_y_discrete(expand = expansion(add = c(.6, 1.1))) +
    labs(x = "Weekly score", y = NULL) + theme_gts() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = "#e8e8e8", size = 9.5, face = "bold"))
  sec("range", "Score Range",
      div(class = "key", HTML(sprintf('<span><i class="sw" style="background:#bdbdbd;border-radius:50%%"></i>Weekly score</span><span><i class="sw" style="background:%s;width:3px"></i>Team median</span><span><i class="dash"></i>League median</span>', GOLD))),
      svg_chart(p, w = 3.9, h = .9 + .28 * nrow(med)),
      div(class = "note", "Each dot is one weekly score. Teams sorted by median."))
}

# ---- Standings: Title Math -----------------------------------------------------------------------
title_math <- function(D) {
  W <- D$WEEK; left <- LAST_WEEK - W; top <- 15
  st <- D$st; L <- max(st$F1Pts)
  d <- st %>% mutate(behind = L - F1Pts, maxgain = top * left, alive = behind <= maxgain,
                     team = pmap_chr(list(Name, Franchise), function(n, f) team_cell(n, htmlEscape(f))),
                     f1 = paste0('<span class="big">', vapply(F1Pts, fmtpt, ""), '</span>'),
                     bh = ifelse(behind == 0, nil, paste0("−", vapply(behind, fmtpt, ""))),
                     mg = ifelse(behind == 0, nil, vapply(maxgain, fmtpt, "")),
                     stt = ifelse(alive, '<span class="pos-v">Alive</span>', '<span class="neg-v">Out</span>'))
  sec("titlemath", "Title Math",
      p(class = "dek", sprintf("%d weeks left, so the most F1 any team can still gain on the leader is %d (15 a week).", left, top * left)),
      mtable(d, list(Team = "team", F1 = "f1", Behind = "bh", Status = "stt"),
             left = 1, cls = "cap tm", widths = c(NA, "2.6rem", "3.4rem", "3.4rem")),
      div(class = "note", sprintf("Weekly F1: 15, 10, 7, 4, 2, 1 for 1st to 6th, over an %d-week season. Alive: behind by no more than %d per week left (win every week, leader scores none).", LAST_WEEK, top)))
}

# ---- Sim Center: Title Odds Movers -------------------------------------------------------------
odds_movers <- function(D) {
  W <- D$WEEK
  if (!(W - 1) %in% D$ohist$week) return(NULL)
  d <- D$ohist %>% filter(week %in% c(W - 1, W)) %>% select(Name, week, Title, Top3) %>%
    tidyr::pivot_wider(names_from = week, values_from = c(Title, Top3)) %>%
    setNames(c("Name", "t0", "t1", "p0", "p1")) %>% mutate(dt = t1 - t0, dp = p1 - p0) %>% arrange(dt) %>%
    mutate(Name = factor(Name, levels = Name), up = dt >= 0,
           lbl = sprintf("%s  ·  Top 3 %s (%s)", pct_txt(t1), pct_txt(p1), sgn(round(dp), 0)))
  xmax <- max(c(d$t0, d$t1))
  p <- ggplot(d, aes(y = Name)) +
    geom_segment(aes(x = t0, xend = t1, yend = Name, colour = up), linewidth = 1.1,
                 arrow = arrow(length = unit(.13, "cm"), type = "closed")) +
    geom_point(aes(x = t0), colour = "#5a5a5a", size = 1.6) +
    geom_text(aes(x = xmax * 1.04, label = lbl), hjust = 0, colour = "#d6d6d6", size = 2.7, family = "Inter") +
    scale_colour_manual(values = c(`TRUE` = POS_C, `FALSE` = NEG_C)) +
    scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, xmax * 1.85), breaks = pretty(c(0, xmax), 4)) +
    labs(x = "Title odds", y = NULL) + theme_gts() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = "#e8e8e8", size = 9.5, face = "bold"))
  b <- d %>% slice_max(dt, n = 1, with_ties = FALSE); w <- d %>% slice_min(dt, n = 1, with_ties = FALSE)
  sec("movers", "Title Odds Movers", tag = "Sims by <b>Golden Ticket Sims</b>",
      p(class = "dek", sprintf("Title odds after week %d vs. after week %d.", W, W - 1)),
      svg_chart(p, w = 4, h = .9 + .28 * nrow(d)),
      div(class = "fact-line", HTML(sprintf("<b>Biggest rise: %s, %s to %s. Biggest drop: %s, %s to %s.</b>",
                                            b$Name, pct_txt(b$t0), pct_txt(b$t1), w$Name, pct_txt(w$t0), pct_txt(w$t1)))),
      div(class = "note", "Arrow runs from last week's title odds to this week's. Right: title odds now, top 3 odds now (change in points)."))
}

# ---- Money & Cap: Contract Timeline -------------------------------------------------------------
contract_timeline <- function(D) {
  yrs <- (SEASON + 1):(SEASON + max(4, max(D$ros$contract_years, na.rm = TRUE)))
  d <- D$ros %>% filter(coalesce(contract_years, 0) >= 1) %>%
    tidyr::crossing(year = yrs) %>% filter(SEASON + contract_years >= year) %>%
    group_by(franchise_id, year) %>% summarise(sal = sum(salary), n = n(), .groups = "drop") %>%
    left_join(D$teams %>% select(franchise_id, Name), by = "franchise_id")
  ord <- d %>% filter(year == SEASON + 1) %>% arrange(sal) %>% pull(Name)
  d <- d %>% mutate(Name = factor(Name, levels = ord), year = factor(year, levels = rev(yrs))) %>%
    arrange(Name, desc(year)) %>% group_by(Name) %>% mutate(x1 = cumsum(sal), x0 = x1 - sal) %>% ungroup()
  cols <- setNames(c(GOLD, "#B8963A", "#7A6420", "#4a4020", "#2f2a18", "#221f14")[seq_along(yrs)], yrs)
  p <- ggplot(d) +
    geom_rect(aes(xmin = x0, xmax = x1, ymin = as.numeric(Name) - .38, ymax = as.numeric(Name) + .38, fill = year), colour = "#0a0a0a", linewidth = .5) +
    geom_text(data = d %>% filter(sal >= 32), aes(x = (x0 + x1) / 2, y = as.numeric(Name) + .1, label = paste0("$", sal),
              colour = year %in% yrs[1:2]), size = 2.45, family = "Inter", fontface = "bold") +
    geom_text(data = d %>% filter(sal >= 32), aes(x = (x0 + x1) / 2, y = as.numeric(Name) - .2, label = n,
              colour = year %in% yrs[1:2]), size = 2.05, family = "Inter") +
    scale_fill_manual(values = cols) + scale_colour_manual(values = c(`TRUE` = "#0a0a0a", `FALSE` = "#e8e8e8")) +
    scale_y_continuous(breaks = seq_along(ord), labels = ord, expand = c(0, .3)) +
    scale_x_continuous(labels = function(x) paste0("$", x), expand = expansion(mult = c(0, .02))) +
    labs(x = "Salary committed", y = NULL) + theme_gts() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = "#e8e8e8", size = 9.5, face = "bold"))
  sec("timeline", "Contract Timeline",
      div(class = "key", HTML(paste(sprintf('<span><i class="sw" style="background:%s"></i>%s</span>', cols[as.character(yrs)], yrs), collapse = ""))),
      svg_chart(p, w = 4, h = .9 + .34 * length(ord)),
      div(class = "note", "Salary under contract by season. Excludes traded cap and dead money. In each block: $ committed, and players signed beneath it (shown where the block is wide enough). Sorted by 2027 committed."))
}

# ---- F1 Points by Player (last year's Point Share, F1C_violin.Rmd) -----------------------------
# Each week a team's F1 points are split across its lineup by each starter's share of the lineup score.
POINT_SHARE_NOTE <- "Each week, a team's F1 points are split among its starters in proportion to their share of the lineup score."
point_share <- function(D) {
  D$lu %>% filter(used) %>% group_by(week, franchise_id) %>% mutate(share = score / sum(score)) %>% ungroup() %>%
    left_join(D$res %>% select(week, franchise_id, F1Pts), by = c("week", "franchise_id")) %>%
    mutate(f1 = coalesce(share * F1Pts, 0)) %>%
    group_by(player_id, player_name, pos, franchise_id, Name) %>%
    summarise(F1 = sum(f1), Lineup = sum(score), Starts = n(), .groups = "drop") %>%
    arrange(desc(F1), desc(Lineup))
}

point_share_league <- function(D, n = 20) {
  d <- point_share(D) %>% head(n) %>%
    mutate(k = row_number(), posc = paste0('<span class="slot-g">', pos, '</span>'),
           pl = pmap_chr(list(player_name, Name), function(p, o) team_cell(short_name(p), o)),
           f1c = paste0('<span class="big">', fmt1(F1), '</span>'), lp = fmt1(Lineup))
  sec("pointshare", "F1 Points by Player",
      p(class = "dek", "Which players have earned their teams the most F1 points this season."),
      mtable(d, list("#" = "k", Pos = "posc", Player = "pl", "F1 earned" = "f1c", "Lineup pts" = "lp"), left = 3,
             cls = "cap tm", widths = c("1.4rem", "2rem", NA, "3.2rem", "3.4rem")),
      div(class = "note", POINT_SHARE_NOTE))
}

point_share_team <- function(ps, fid, n = 5) {
  t <- ps %>% filter(franchise_id == fid) %>% head(n)
  div(h3(class = "rs-sub", "F1 points by player"),
      div(class = "tp-list", if (nrow(t)) lapply(seq_len(nrow(t)), function(j) div(class = "tp-row",
          span(short_name(t$player_name[j]), tags$small(sprintf("%s · %s lineup pts", t$pos[j], fmt1(t$Lineup[j])))), tags$b(fmt1(t$F1[j]))))
          else div(class = "tp-row", span("No F1 points yet"), HTML(nil))))
}

# ---- Standings: Season Heat Map --------------------------------------------------------------------
SHOW_ODDS_MOVERS <- FALSE   # Title Odds Movers is built but kept off the site for now (odds_movers())
season_heat <- function(D) {
  W <- D$WEEK; ord <- D$st$Name
  d <- D$res %>% filter(week <= W) %>%
    mutate(Name = factor(Name, levels = rev(ord)), fin = vapply(rank, finish_lbl, ""), r = pmin(rank, 12))
  p <- ggplot(d, aes(week, Name)) +
    geom_tile(aes(fill = r), colour = "#0a0a0a", linewidth = 1.2) +
    geom_text(aes(label = round(points), colour = r <= 3), size = 2.6, family = "Inter", fontface = "bold", nudge_y = .12) +
    geom_text(aes(label = fin, colour = r <= 3), size = 1.75, family = "Inter", nudge_y = -.25, alpha = .85) +
    scale_fill_gradientn(colours = c(GOLD, "#B8963A", "#5a4b1e", "#2a2a2a", "#161616"), values = c(0, .12, .35, .65, 1), limits = c(1, 12)) +
    scale_colour_manual(values = c(`TRUE` = "#0a0a0a", `FALSE` = "#e8e8e8")) +
    scale_x_continuous(breaks = seq_len(W), labels = paste0("W", seq_len(W)), position = "top", expand = c(0, 0)) +
    labs(x = NULL, y = NULL) + theme_gts() +
    theme(panel.grid = element_blank(), axis.text.y = element_text(colour = "#e8e8e8", size = 8.5, face = "bold"), axis.text.x = element_text(size = 7.5))
  sec("heat", "Season Heat Map",
      p(class = "dek", "Every weekly score, shaded by that week's finish. Rows in standings order."),
      svg_chart(p, w = 4.2, h = .7 + .3 * length(ord)),
      div(class = "key", HTML(sprintf('<span><i class="sw" style="background:%s"></i>1st</span><span><i class="sw" style="background:#5a4b1e"></i>Middle</span><span><i class="sw" style="background:#161616;border:1px solid #333"></i>12th</span>', GOLD))),
      div(class = "note", "Big number: weekly score. Small: that week's finish."))
}

# ---- Week in Review: Top Weekly Scores (from last year's report) --------------------------------
top_weekly_scores <- function(D, n = 10) {
  top <- D$res %>% filter(week <= D$WEEK) %>% arrange(desc(points)) %>% head(n)
  rows <- lapply(seq_len(nrow(top)), function(i) { x <- top[i, ]
    pl <- D$lu %>% filter(week == x$week, franchise_id == x$franchise_id, used) %>% slice_max(score, n = 5, with_ties = FALSE)
    div(class = "tp-row",
        span(tags$b(sprintf("%d. %s", i, x$Name)), tags$small(sprintf("Wk %d · %s", x$week, finish_lbl(x$rank))),
             tags$small(class = "tw-pl", paste(sprintf("%s %s", sub(",.*$", "", pl$player_name), fmt1(pl$score)), collapse = " · "))),
        tags$b(fmt1(x$points)))
  })
  sec("topweeks", "Top Weekly Scores", p(class = "dek", "The season's best team weeks, with the five starters who scored most."),
      div(class = "tp-list tw", rows))
}

# ---- Team Pages: League Leaders by position (from last year's report) ---------------------------
league_leaders <- function(D, n = 5) {
  pts <- if (!is.null(D$ytd)) D$ytd %>% rename(Pts = ytd) else D$lu %>% distinct(week, player_id, .keep_all = TRUE) %>% group_by(player_id) %>% summarise(Pts = sum(score))
  g <- D$lu %>% distinct(week, player_id, .keep_all = TRUE) %>% group_by(player_id) %>% summarise(G = sum(score != 0), .groups = "drop")
  r <- D$ros %>% filter(pos %in% SKILL) %>% left_join(D$teams %>% select(franchise_id, Name), by = "franchise_id") %>%
    inner_join(pts, by = "player_id") %>% left_join(g, by = "player_id") %>% mutate(ppg = ifelse(coalesce(G, 0L) > 0, Pts / G, NA))
  boxes <- lapply(SKILL, function(p) {
    d <- r %>% filter(pos == p) %>% arrange(desc(Pts)) %>% head(n) %>%
      mutate(pl = pmap_chr(list(player_name, Name), function(a, o) team_cell(short_name(a), o)),
             pt = paste0('<span class="big">', fmt1(Pts), '</span>'), pg = ifelse(is.na(ppg), nil, fmt1(ppg)))
    div(h3(class = "rs-sub", p), mtable(d, list(Player = "pl", Pts = "pt", PPG = "pg"), left = 1, cls = "cap", widths = c(NA, "3.2rem", "2.6rem")))
  })
  sec("leaders", "League Leaders", p(class = "dek", "Top season scorers at each position among rostered players."),
      div(class = "tp-cols", boxes[[1]], boxes[[2]]), div(class = "tp-cols", boxes[[3]], boxes[[4]]),
      div(class = "note", "Season fantasy points on any team; owner is the current team. PPG counts games with a score."))
}

# ---- Team Pages: best single weeks of F1 points by a player ---------------------------------------
point_share_weeks <- function(D, n = 10) {
  d <- D$lu %>% filter(used) %>% group_by(week, franchise_id) %>% mutate(share = score / sum(score)) %>% ungroup() %>%
    left_join(D$res %>% select(week, franchise_id, F1Pts), by = c("week", "franchise_id")) %>%
    mutate(f1 = share * F1Pts) %>% filter(f1 > 0) %>% arrange(desc(f1), desc(score)) %>% head(n) %>%
    mutate(k = row_number(), posc = paste0('<span class="slot-g">', pos, '</span>'),
           pl = pmap_chr(list(player_name, Name, week), function(p, o, w) team_cell(short_name(p), sprintf("%s · Wk %d", o, w))),
           f1c = paste0('<span class="big">', fmt1(f1), '</span>'), sc = fmt1(score), tf = vapply(F1Pts, fmtpt, ""))
  sec("pointweeks", "Best Single Weeks, F1 Points",
      mtable(d, list("#" = "k", Pos = "posc", Player = "pl", "F1 earned" = "f1c", Pts = "sc", "Team F1" = "tf"), left = 3,
             cls = "cap tm", widths = c("1.4rem", "2rem", NA, "3.2rem", "2.6rem", "2.8rem")),
      div(class = "note", POINT_SHARE_NOTE))
}

# ---- Front Office: Traded Cap Ledger -----------------------------------------------------------------
# Every row of the league sheet's traded tab, in sheet order, so the league can check the Traded Cap totals.
traded_ledger <- function(D) {
  tr <- D$trd$all
  if (is.null(tr) || !nrow(tr)) return(NULL)
  if (!"is_new" %in% names(tr)) tr$is_new <- FALSE
  yrs <- sort(unique(tr$Year))
  body <- lapply(yrs, function(y) { g <- tr %>% filter(Year == y) %>% arrange(sheet_row)
    tagList(tags$tr(class = "rg", tags$td(colspan = 4, sprintf("%d cap · $%s", y, fmtpt(sum(g$Amount))))),
      lapply(seq_len(nrow(g)), function(i) { x <- g[i, ]
        tags$tr(tags$td(class = "lg-n", x$sheet_row), tags$td(HTML(paste0(htmlEscape(x$Sender), if (x$is_new) ' <span class="new-tag">NEW</span>'))),
                tags$td(x$Receiver), tags$td(class = "r", HTML(paste0('<span class="big">$', fmtpt(x$Amount), '</span>')))) }))
  })
  led <- tags$table(class = "mt ros ledger", `data-quarto-disable-processing` = "true",
    tags$thead(tags$tr(tags$th(style = "width:1.8rem", "#"), tags$th("From"), tags$th("To"), tags$th(class = "r", style = "width:3.2rem", "$"))),
    tags$tbody(body))
  # per-team check: received, sent and net per cap year (net equals the Traded Cap table)
  chk <- bind_rows(tr %>% transmute(Name = Receiver, Year, rec = Amount, snt = 0), tr %>% transmute(Name = Sender, Year, rec = 0, snt = Amount)) %>%
    group_by(Name, Year) %>% summarise(rec = sum(rec), snt = sum(snt), .groups = "drop") %>% mutate(net = rec - snt)
  ck <- D$teams %>% select(Name) %>% tidyr::crossing(Year = yrs) %>% left_join(chk, by = c("Name", "Year")) %>%
    mutate(across(c(rec, snt, net), ~ coalesce(.x, 0))) %>% arrange(Year, desc(net))
  ctab <- lapply(yrs, function(y) { d <- ck %>% filter(Year == y) %>%
      mutate(rc = ifelse(rec == 0, nil, paste0("$", vapply(rec, fmtpt, ""))), sc = ifelse(snt == 0, nil, paste0("$", vapply(snt, fmtpt, ""))), nc = signed_c(net))
    div(h3(class = "rs-sub", sprintf("%d check", y)),
        mtable(d, list(Team = "Name", Received = "rc", Sent = "sc", Net = "nc"), left = 1, cls = "cap", widths = c(NA, "3.6rem", "3.2rem", "3.4rem"))) })
  sec("ledger", "Traded Cap Ledger",
      p(class = "dek", "Every traded cap transaction, as entered in the league sheet."),
      led, div(class = "tp-cols", ctab),
      div(class = "note", "Every row from the league's traded cap sheet. Nicknames shown as owner names. # is the sheet row. Net matches the Traded Cap table."))
}
