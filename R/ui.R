# HTML building blocks for the mobile layout (styles in styles.scss)

suppressPackageStartupMessages(library(htmltools))

fmt1  <- function(x) formatC(x, format = "f", digits = 1)
fmtpt <- function(x) ifelse(x == round(x), format(x), fmt1(x))
money <- function(x) paste0("$", format(round(x), big.mark = ","))
short_name <- function(x) sub("^(.*), (.*)$", "\\2 \\1", x)   # "Allen, Josh" -> "Josh Allen"

count_lbl <- function(n, one, many = paste0(one, "s")) if (n == 0) "" else paste(n, if (n == 1) one else many)
sep <- function(...) paste(Filter(nzchar, c(...)), collapse = " · ")

standings_html <- function(st, my_id) {
  leader <- max(st$F1Pts)
  multi  <- any(grepl("-", st$Last3))
  rows <- lapply(seq_len(nrow(st)), function(i) {
    r <- st[i, ]
    move <- if (is.na(r$Move) || r$Move == 0) ""
            else if (r$Move > 0) paste0("+", r$Move) else as.character(r$Move)
    stats <- sep(
      count_lbl(r$Wins, "win"), count_lbl(r$Podiums, "podium"), count_lbl(r$PtsFin, "points finish", "points finishes"),
      paste0(format(round(r$PF), big.mark = ","), " PF"),
      if (r$Earnings > 0) money(r$Earnings) else "",
      if (multi) paste0("Last 3: ", r$Last3) else ""
    )
    div(class = paste("lb", if (r$Pos == 1) "first", if (r$franchise_id == my_id) "me"),
        div(class = "lb-pos", r$Pos, if (nzchar(move)) span(class = paste("lb-move", if (r$Move > 0) "up" else "down"), move)),
        div(class = "lb-main",
            div(class = "lb-name", r$Name, span(class = "lb-team", r$Franchise)),
            div(class = "lb-stats", stats)),
        div(class = "lb-val",
            div(class = "lb-pts", fmtpt(r$F1Pts)),
            div(class = "lb-gap", if (r$F1Pts == leader) "" else paste0("−", fmtpt(leader - r$F1Pts)))))
  })
  div(class = "board", div(class = "board-head", span("Team"), span("F1 pts")), rows)
}

results_html <- function(wk, my_id) {
  wk <- wk %>% arrange(rank)
  rows <- lapply(seq_len(nrow(wk)), function(i) {
    r <- wk[i, ]
    award <- sep(if (r$F1Pts > 0) paste0("+", fmtpt(r$F1Pts)) else "", if (r$Earnings > 0) money(r$Earnings) else "")
    div(class = paste("lb", if (r$franchise_id == my_id) "me", if (r$F1Pts == 0) "dim"),
        div(class = "lb-pos", fmtpt(r$rank)),
        div(class = "lb-main", div(class = "lb-name", r$Name, span(class = "lb-team", r$Franchise))),
        div(class = "lb-val", div(class = "lb-score", fmt1(r$points)), div(class = "lb-gap", award)))
  })
  div(class = "board", div(class = "board-head", span("Team"), span("Score")), rows)
}

lineup_html <- function(tow, best_team_score) {
  rows <- lapply(seq_len(nrow(tow)), function(i) {
    r <- tow[i, ]
    div(class = "lb",
        div(class = "lb-pos slot", r$slot),
        div(class = "lb-main",
            div(class = "lb-name", short_name(r$player_name), span(class = "lb-team", r$nfl)),
            div(class = "lb-stats", sep(r$Name, if (!r$starter) "on bench" else ""))),
        div(class = "lb-val", div(class = "lb-score", fmt1(r$score))))
  })
  total <- sum(tow$score)
  div(class = "board",
      div(class = "board-head", span("Player"), span("Pts")),
      rows,
      div(class = "board-foot",
          span("Total"),
          span(fmt1(total), span(class = "lb-gap", paste0(" vs ", fmt1(best_team_score), " top team")))))
}
