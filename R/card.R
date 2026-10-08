# WhatsApp image cards: standalone HTML at 540x675, screenshotted at 2x -> 1080x1350 PNG

EDGE <- "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"

card_css <- '
@import url("https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap");
* { box-sizing: border-box; margin: 0; padding: 0; }
html, body { width: 540px; height: 675px; background: #0a0a0a; color: #e8e8e8; overflow: hidden;
  font-family: "Inter", "Segoe UI", sans-serif; }
.pos, .num, .total { font-feature-settings: "tnum" 1; }
.pos { white-space: nowrap; }
.prov { color: #E8C547; }
.fin { display: inline-block; width: 30px; white-space: nowrap; text-align: left; font-size: 10px; font-weight: 500; color: #5c5c5c; margin-right: 4px; }
.fin.top { color: #E8C547; }
.wkc { display: flex; justify-content: space-between; align-items: baseline; }
.card { display: flex; flex-direction: column; height: 100%; padding: 26px 28px 18px; }
.head { display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 14px; }
.kicker { color: #E8C547; font-size: 10px; font-weight: 600; letter-spacing: .16em; text-transform: uppercase; }
.title { font-size: 30px; font-weight: 800; color: #fff; letter-spacing: -.02em; margin-top: 4px; line-height: 1.05; }
.logo { width: 46px; height: 46px; object-fit: contain; border-radius: 6px; }
.cols, .row { display: grid; align-items: center; column-gap: 8px; }
.cols { color: #5c5c5c; font-size: 9px; letter-spacing: .12em; text-transform: uppercase;
  padding-bottom: 6px; border-bottom: 1px solid #262626; }
.rows { flex: 1; display: flex; flex-direction: column; }
.row { flex: 1; border-bottom: 1px solid #1a1a1a; }
.row:last-child { border-bottom: 0; }
.row.me { background: #151515; box-shadow: -10px 0 0 #151515, 10px 0 0 #151515; }
.pos { font-size: 15px; font-weight: 600; color: #8a8a8a; }
.first .pos, .first .total { color: #E8C547; }
.mv { font-size: 9px; font-weight: 600; margin-left: 3px; vertical-align: 2px; }
.mv.up { color: #4cc38a; } .mv.down { color: #e5484d; }
.name { min-width: 0; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; font-size: 15px; font-weight: 600; color: #fff; }
.name small { font-size: 11px; font-weight: 400; color: #666; margin-left: 6px; }
.num { text-align: right; font-size: 13px; color: #bdbdbd; }
.num.gain { color: #e8e8e8; font-weight: 600; }
.num.zero { color: #444; }
.total { text-align: right; font-size: 20px; font-weight: 700; color: #fff; }
.r { text-align: right; }
.foot { display: flex; justify-content: space-between; color: #5c5c5c; font-size: 10px; padding-top: 10px;
  border-top: 1px solid #262626; margin-top: 4px; }
.foot b { color: #e8e8e8; font-weight: 600; }
.slot { font-size: 10px; font-weight: 600; letter-spacing: .08em; color: #5c5c5c; }
.sub { font-size: 11px; color: #777; margin-top: 1px; }
.total.neg { color: #e5484d; }
'

card_page <- function(body) {
  logo <- base64enc::dataURI(file = "assets/logo.png", mime = "image/png")
  # Sponsor line (R/brand.R): every card footer credits Golden Ticket Sims, plus the promo once it is set
  if (!exists("GTS_PROMO")) source("R/brand.R")
  promo   <- if (nzchar(GTS_PROMO) && GTS_PROMO != "GTS_PROMO") paste0(" · ", esc(GTS_PROMO)) else ""
  sponsor <- paste0('<span class="spon">Presented by <b>Golden Ticket Sims</b>', promo, '</span>')
  body <- gsub("<span>Golden Ticket Sims</span>", sponsor, body, fixed = TRUE)
  body <- gsub("{{SPONSOR}}", sponsor, body, fixed = TRUE)
  paste0('<!doctype html><html><head><meta charset="utf-8"><style>', card_css,
         '.spon { color: #8a8a8a; } .spon b { color: #E8C547; font-weight: 600; }',
         '</style></head><body>', gsub("{{LOGO}}", logo, body, fixed = TRUE), '</body></html>')
}

esc <- function(x) htmltools::htmlEscape(x)

finish_lbl <- function(rk) {
  n <- floor(rk)
  suf <- if (n %% 100 %in% 11:13) "th" else c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1]
  paste0(if (rk != n) "T" else "", n, suf)
}

standings_card <- function(st, wk, week, my_id) {
  grid <- "grid-template-columns: 44px 1fr 86px 32px 38px 38px;"
  pending <- sum(wk$pending)
  status  <- if (pending > 0) sprintf('<span class="prov">Provisional · %d players still to play</span>', pending) else "Final · 2026 season"
  wk <- wk %>% select(franchise_id, points, wkRank = rank, wkF1 = F1Pts)
  st <- st %>% left_join(wk, by = "franchise_id")
  rows <- paste0(vapply(seq_len(nrow(st)), function(i) {
    r <- st[i, ]
    mv <- if (is.na(r$Move) || r$Move == 0) "" else
      sprintf('<span class="mv %s">%s%d</span>', if (r$Move > 0) "up" else "down", if (r$Move > 0) "+" else "", r$Move)
    sprintf('<div class="row%s" style="%s"><div class="pos">%d%s</div><div class="name">%s<small>%s</small></div><div class="num wkc"><span class="fin%s">%s</span><span>%s</span></div><div class="num %s">%s</div><div class="num %s">%s</div><div class="total">%s</div></div>',
            if (r$Pos == 1) " first" else "", grid,
            r$Pos, mv, esc(r$Name), esc(r$Franchise),
            if (r$wkRank <= 3) " top" else "", finish_lbl(r$wkRank), fmt1(r$points),
            if (r$wkF1 > 0) "gain" else "zero", if (r$wkF1 > 0) paste0("+", fmtpt(r$wkF1)) else "–",
            if (r$Earnings > 0) "gain" else "zero", if (r$Earnings > 0) money(r$Earnings) else "–", fmtpt(r$F1Pts))
  }, ""), collapse = "")
  card_page(sprintf(
    '<div class="card">
       <div class="head"><div><div class="kicker">Fantasy1 Championship · Week %d</div><div class="title">Standings</div></div><img class="logo" src="{{LOGO}}"></div>
       <div class="cols" style="%s"><div>#</div><div>Team</div><div class="r">Wk %d</div><div class="r">+F1</div><div class="r">Won</div><div class="r">F1 pts</div></div>
       <div class="rows">%s</div>
       <div class="foot"><span>%s</span><span>Golden Ticket Sims</span></div>
     </div>', week, grid, week, rows, status))
}

lineup_card <- function(tow, week, best_team, pending = 0) {
  grid <- "grid-template-columns: 40px 1fr 56px;"
  rows <- paste0(vapply(seq_len(nrow(tow)), function(i) {
    r <- tow[i, ]
    sprintf('<div class="row" style="%s"><div class="slot">%s</div><div style="min-width:0"><div class="name">%s<small>%s</small></div><div class="sub">%s%s</div></div><div class="total">%s</div></div>',
            grid, r$slot, esc(short_name(r$player_name)), esc(r$nfl), esc(r$Name),
            if (!r$starter) " · on bench" else "", fmt1(r$score))
  }, ""), collapse = "")
  card_page(sprintf(
    '<div class="card">
       <div class="head"><div><div class="kicker">Fantasy1 Championship · Week %d</div><div class="title">Team of the Week</div></div><img class="logo" src="{{LOGO}}"></div>
       <div class="cols" style="%s"><div>Pos</div><div>Player</div><div class="r">Pts</div></div>
       <div class="rows">%s</div>
       <div class="foot"><span>Total <b>%s</b> &nbsp;·&nbsp; top team %s</span><span>%s</span></div>
     </div>', week, grid, rows, fmt1(sum(tow$score)), fmt1(best_team),
     if (pending > 0) '<span class="prov">Provisional</span>' else "Golden Ticket Sims"))
}

signed <- function(x) if (x > 0) paste0("+", fmtpt(x)) else if (x < 0) paste0("−", fmtpt(abs(x))) else "–"

cap_card <- function(cap, week, my_id) {
  grid <- "grid-template-columns: 1fr 50px 44px 40px 36px 56px;"
  rows <- paste0(vapply(seq_len(nrow(cap)), function(i) {
    r <- cap[i, ]
    sprintf('<div class="row" style="%s"><div class="name">%s<small>%d signed</small></div><div class="num">%s</div><div class="num %s">%s</div><div class="num %s">%s</div><div class="num %s">%s</div><div class="total%s">%s</div></div>',
            grid, esc(r$Name), r$Signed,
            money(r$Salary),
            if (r$Traded == 0) "zero" else "", signed(r$Traded),
            if (r$CapHit == 0) "zero" else "", if (r$CapHit == 0) "–" else fmtpt(r$CapHit),
            if (r$F1Pts == 0) "zero" else "", if (r$F1Pts == 0) "–" else paste0("+", fmtpt(r$F1Pts)),
            if (r$Space < 0) " neg" else "", paste0(if (r$Space < 0) "−" else "", money(abs(r$Space))))
  }, ""), collapse = "")
  card_page(sprintf(
    '<div class="card">
       <div class="head"><div><div class="kicker">Fantasy1 Championship · Week %d</div><div class="title">2027 Cap Check</div></div><img class="logo" src="{{LOGO}}"></div>
       <div class="cols" style="%s"><div>Team</div><div class="r">2027 Sal</div><div class="r">Traded</div><div class="r">Dead</div><div class="r">F1</div><div class="r">Space</div></div>
       <div class="rows">%s</div>
       <div class="foot"><span>$%d base + traded + F1 so far − salary − dead money. Seasonal F1 and socialism added at season end.</span></div>
       <div class="foot" style="border-top:0;margin-top:0;padding-top:4px"><span></span>{{SPONSOR}}</div>
     </div>', week, grid, rows, CAP_BASE))
}

pct <- function(x) if (x >= 99.5) "99%" else if (x < 1) "<1%" else paste0(round(x), "%")

odds_card <- function(sim, week, note = "") {
  grid <- "grid-template-columns: 30px 1fr 44px 40px 40px 40px 56px;"
  rows <- paste0(vapply(seq_len(nrow(sim)), function(i) {
    r <- sim[i, ]
    sprintf('<div class="row%s" style="%s"><div class="pos">%d</div><div class="name">%s<small>%s</small></div><div class="num">%s</div><div class="num %s">%s</div><div class="num %s">%s</div><div class="num">%s</div><div class="total">%s</div></div>',
            if (r$Pos == 1) " first" else "", grid,
            r$Pos, esc(r$Name), esc(r$Franchise),
            fmt1(r$mean),   # sim expected best-ball score
            if (r$win < 1) "zero" else "", pct(r$win),
            if (r$top3 < 1) "zero" else "", pct(r$top3),
            pct(r$top6),
            fmtpt(r$expF1))   # expected F1 points — the sort column
  }, ""), collapse = "")
  card_page(sprintf(
    '<div class="card">
       <div class="head"><div><div class="kicker">Fantasy1 Championship · Week %d</div><div class="title">Projected Finish</div></div><img class="logo" src="{{LOGO}}"></div>
       <div class="cols" style="%s"><div>#</div><div>Team</div><div class="r">Proj</div><div class="r">Win</div><div class="r">Top 3</div><div class="r">Top 6</div><div class="r">Exp F1</div></div>
       <div class="rows">%s</div>
       <div class="foot"><span>%s</span><span>Golden Ticket Sims</span></div>
     </div>', week, grid, rows, esc(note)))
}

shoot <- function(html, png) {
  src <- tempfile(fileext = ".html")
  writeLines(html, src, useBytes = TRUE)
  png <- normalizePath(png, mustWork = FALSE)
  system2(EDGE, c("--headless=new", "--disable-gpu", "--hide-scrollbars", "--force-device-scale-factor=2",
                  "--window-size=540,675", "--virtual-time-budget=4000",
                  shQuote(paste0("--screenshot=", png)), shQuote(paste0("file:///", normalizePath(src, "/")))),
          stdout = FALSE, stderr = FALSE)
  png
}
