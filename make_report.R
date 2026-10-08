# Build the weekly report:  Rscript make_report.R 4
# 1. Pulls everything once (MFL, league sheet, cache) into data/cache/report-wNN.rds (+ report-latest.rds)
# 2. Renders the site pages (Standings landing page + six section pages, archive, this week's archive page)
# 3. Writes share/F1C-Week-NN.html, every section in one self-contained file
# Then: git add -A; git commit -m "Week N"; git push

args <- commandArgs(trailingOnly = TRUE)
setwd(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))))
WEEK <- if (length(args)) as.integer(args[1]) else stop("Pass the week number, e.g. Rscript make_report.R 4")

QUARTO <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe"
Sys.setenv(QUARTO_R = "C:/Program Files/R/R-4.4.2/bin")
for (f in c("data", "ui", "sim", "brand", "report", "card", "standings_section", "sim_section", "money_section", "build_data")) source(file.path("R", paste0(f, ".R")))

# 1. data (--render-only reuses the data already built for this week)
dir.create("data/cache", recursive = TRUE, showWarnings = FALSE)
rds <- sprintf("data/cache/report-w%02d.rds", WEEK)
if (!"--render-only" %in% args) {
  D <- build_report_data(WEEK)
  saveRDS(D, rds)
  message("Data built: ", rds)
} else {
  D <- readRDS(rds); D$wnext <- next_week_sim(WEEK); saveRDS(D, rds)   # pick up a fresh make_preview.R run
}
if (!is.null(readRDS(rds)$wnext)) message("Week ", WEEK + 1, " sim: cards/", sprintf("week-%02d-sim.csv", WEEK + 1))
file.copy(rds, "data/cache/report-latest.rds", overwrite = TRUE)

# 2. site: section pages read report-latest; the week's archive page reads its own week's file
stem <- sprintf("week-%02d", WEEK)
wq <- file.path("weeks", paste0(stem, ".qmd"))
wdate <- if (file.exists(wq)) sub("^date: ", "", grep("^date: ", readLines(wq), value = TRUE)[1]) else as.character(Sys.Date())   # re-runs keep the week's first date
writeLines(c("---", sprintf('title: "Week %d"', WEEK), sprintf("date: %s", wdate), "---", "", "```{r}",
             sprintf('PAGE <- "index"; REPORT_FILE <- "%s"; NAV_PREFIX <- "../"', rds), "```", "", "{{< include ../_page.qmd >}}"),
           file.path("weeks", paste0(stem, ".qmd")), useBytes = TRUE)
writeLines(c("---", 'title: "Archive"', 'subtitle: "Every weekly report, 2026"', "listing:", "  contents: weeks",
             '  sort: "date desc"', "  type: table", "  fields: [title, date]", "  sort-ui: false", "  filter-ui: false", "---"),
           "archive.qmd", useBytes = TRUE)
pages <- c("index.qmd", "review.qmd", "sims.qmd", "money.qmd", "office.qmd", "teams.qmd", "alltime.qmd", file.path("weeks", paste0(stem, ".qmd")), "archive.qmd")
for (f in pages) {
  st <- system2(QUARTO, c("render", f), stdout = "", stderr = "")
  if (st != 0) stop("Site render failed: ", f)
}

# 3. share copy: every section on one page, outside the website project (no navbar)
tmp <- file.path(tempdir(), "f1c-share"); unlink(tmp, recursive = TRUE); dir.create(tmp)
file.copy("styles.scss", tmp)
root <- normalizePath(".", winslash = "/")
writeLines(c(
  "---", sprintf('title: "F1C Week %d"', WEEK), "format:", "  html:",
  "    theme: [darkly, styles.scss]", "    embed-resources: true", "    toc: false", "    page-layout: article",
  "    include-in-header:", "      text: |",
  '        <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap" rel="stylesheet">',
  '        <meta name="viewport" content="width=device-width, initial-scale=1">',
  "execute:", "  echo: false", "  warning: false", "  message: false",
  "knitr:", "  opts_knit:", sprintf('    root.dir: "%s"', root), "---", "",
  "```{r}",
  'for (f in c("data", "ui", "sim", "brand", "report", "card", "standings_section", "sim_section", "money_section", "sections")) source(file.path("R", paste0(f, ".R")))',
  sprintf('D <- readRDS("%s")', rds),
  'tagList(page_head(D, "Weekly Report", "Fantasy1 Championship Weekly"), page_nav("", function(p) paste0("#pg-", p)),',
  '  lapply(names(PAGES), function(p) tags$div(id = paste0("pg-", p), class = "share-sec", tags$div(class = "share-kick", PAGES[[p]]), section_body(p, D))), page_foot(D))',
  "```"),
  file.path(tmp, "share.qmd"), useBytes = TRUE)
st <- system2(QUARTO, c("render", shQuote(file.path(tmp, "share.qmd"))), stdout = "", stderr = "")
if (st != 0) stop("Share render failed")
dir.create("share", showWarnings = FALSE)
out <- file.path("share", sprintf("F1C-Week-%02d.html", WEEK))
file.copy(file.path(tmp, "share.html"), out, overwrite = TRUE)
message("Site: docs/index.html (+ review, sims, money, office, teams, alltime)\nShare copy: ", out, " (", round(file.size(out) / 1e6, 1), " MB)")
