# Money & Cap section: the 2027 cap check as last year's report did it (F1C_violin.Rmd:1094-1150) on today's
# standings, plus next offseason's top free agents with franchise tag estimates. No projections.

# Cap check "if the season ended today": weekly F1 + seasonal F1 for the current rank, socialism (two passes),
# traded revenue and the $230 base, less 2027 salary and dead money.
cap_check <- function(lg, st) {
  cap <- cap_table(lg, st)
  rank_now <- st$Pos[match(cap$franchise_id, st$franchise_id)]
  cap %>% mutate(Seasonal = seasonal_award(rank_now, lg), TotalF1 = F1Pts + Seasonal,
                 Socialism = round(socialism(TotalF1), 1),
                 TotalCap = CAP_BASE + TotalF1 + Socialism + Traded,
                 CapSpace = TotalCap - Salary - CapHit) %>%
    arrange(desc(CapSpace))
}

num <- function(x, d = 0) ifelse(x == 0, '<span class="nil">–</span>',
                                 paste0(ifelse(x < 0, "−", ""), formatC(abs(x), format = "f", digits = d, big.mark = ",")))
num1 <- function(x) ifelse(x == round(x), num(x), num(x, 1))

cap_check_table <- function(cc) {
  d <- cc %>% mutate(team = pmap_chr(list(Name, Signed), function(n, s) team_cell(n, paste(s, "signed"))),
                     wf = num1(F1Pts), sf = num1(Seasonal), so = num1(Socialism), tr = num1(Traded),
                     tc = vapply(TotalCap, function(x) fmt1(x) %>% sub("\\.0$", "", .), ""),
                     sa = num(Salary), dh = num1(CapHit),
                     sp = paste0('<span class="big', ifelse(CapSpace < 0, " neg", ""), '">', money_s(CapSpace), '</span>'))
  mtable(d, list(Team = "team", "Wkly F1" = "wf", "Seas F1" = "sf", "Soc" = "so", "Trd" = "tr", "Cap" = "tc",
                 "Sal" = "sa", "Dead" = "dh", "Space" = "sp"),
         left = 1, cls = "cap", widths = c(NA, "2.3rem", "2.3rem", "2rem", "2rem", "2.3rem", "2.2rem", "2rem", "3.1rem"))
}

# Top pending free agents by position (expiring deals and no-contract pickups), season points on any team
fa_block <- function(ros, lu, lg, cc, n = 5) {
  tags_ <- franchise_tags(ros)
  fa <- pending_fas(ros, lu, lg)
  lapply(c("QB", "RB", "WR", "TE"), function(p) {
    tg <- tags_$tag[tags_$pos == p]
    d <- fa %>% filter(pos == p) %>% head(n) %>%
      mutate(pl = pmap_chr(list(player_name, Name, Kind, salary), function(nm, o, k, s)
                 team_cell(short_name(nm), paste0(o, " · ", if (k == "pickup") "pickup" else paste0("$", s, " deal ends")))),
             pts = fmt1(Pts), used = fmt1(Used))
    afford <- cc %>% filter(CapSpace >= tg) %>% pull(Name)
    div(class = "fa-pos",
        div(class = "fa-head", tags$b(p), span(sprintf("Est. tag $%d", tg))),
        mtable(d, list(Player = "pl", "Pts" = "pts", "Started" = "used"), left = 1, widths = c(NA, "3rem", "3.6rem")),
        div(class = "afford", HTML(sprintf("Cap space for a $%d tag today: <b>%s</b>", tg,
                                           if (length(afford)) and_list(afford) else "nobody"))))
  })
}

# Traded cap $ summary: net per team for every year in the ledger (league sheet `traded` tab), plus the
# rows that are new since last week's report. Each run saves a snapshot to data/cache/traded-wNN.rds; the
# diff against the previous week's snapshot is "added this week".
traded_summary <- function(lg, week) {
  tr <- load_traded() %>% mutate(row = paste(Sender, Receiver, Amount, Year))
  dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  saveRDS(tr, file.path(CACHE_DIR, sprintf("traded-w%02d.rds", week)))
  prev_f <- file.path(CACHE_DIR, sprintf("traded-w%02d.rds", week - 1))
  new <- if (file.exists(prev_f)) {
    prev <- readRDS(prev_f)
    # match row by row so a repeated identical row still counts once per occurrence
    k <- table(tr$row); kp <- table(factor(prev$row, levels = names(k)))
    extra <- pmax(as.integer(k) - as.integer(kp), 0); names(extra) <- names(k)
    tr %>% group_by(row) %>% filter(row_number() <= extra[row[1]]) %>% ungroup()
  } else NULL
  net <- bind_rows(lapply(sort(unique(tr$Year)), function(y)
    traded_by_owner(tr, lg$teams$Name, y) %>% mutate(Year = y))) %>%
    tidyr::pivot_wider(names_from = Year, values_from = Traded, values_fill = 0)
  list(net = lg$teams %>% select(Name) %>% left_join(net, by = "Name") %>% mutate(across(-Name, ~ replace_na(.x, 0))),
       new = new, first_week = is.null(new))
}
