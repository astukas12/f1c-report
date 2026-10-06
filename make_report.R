# Render the weekly report:  Rscript make_report.R 4
# Writes docs/weeks/week-04.html (site page) and share/F1C-Week-04.html (one self-contained file).

args <- commandArgs(trailingOnly = TRUE)
setwd(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))))
WEEK <- if (length(args)) as.integer(args[1]) else stop("Pass the week number, e.g. Rscript make_report.R 4")

QUARTO <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe"
Sys.setenv(QUARTO_R = "C:/Program Files/R/R-4.4.2/bin")
stem <- sprintf("week-%02d", WEEK)
page <- file.path("weeks", paste0(stem, ".qmd"))

if (!file.exists(page)) writeLines(c(
  "---", sprintf('title: "Week %d"', WEEK), 'subtitle: "Fantasy1 Championship \u00b7 2026"',
  sprintf("date: %s", Sys.Date()), "---", "", "```{r}", sprintf("WEEK <- %d", WEEK), "```", "",
  "{{< include ../_weekly.qmd >}}"), page, useBytes = TRUE)

# Landing page = the latest week; archive lists every week page
writeLines(c("---", 'title: "Fantasy1 Championship Weekly"', "---", "", "```{r}", sprintf("WEEK <- %d", WEEK), "```", "",
             "{{< include _weekly.qmd >}}"), "index.qmd", useBytes = TRUE)
writeLines(c("---", 'title: "Archive"', 'subtitle: "Every weekly report, 2026"', "listing:", "  contents: weeks",
             '  sort: "date desc"', "  type: table", "  fields: [title, date]", "  sort-ui: false", "  filter-ui: false", "---"),
           "archive.qmd", useBytes = TRUE)

# 1. Site pages
for (f in c("index.qmd", page, "archive.qmd")) {
  st <- system2(QUARTO, c("render", f), stdout = "", stderr = "")
  if (st != 0) stop("Site render failed: ", f)
}

# 2. Self-contained copy: same report body, rendered outside the website project so it has no navbar
tmp <- file.path(tempdir(), "f1c-share"); unlink(tmp, recursive = TRUE); dir.create(tmp)
file.copy("styles.scss", tmp)
root <- normalizePath(".", winslash = "/")
body <- readLines("_weekly.qmd", encoding = "UTF-8")
writeLines(c(
  "---", sprintf('title: "F1C Week %d"', WEEK), "format:", "  html:",
  "    theme: [darkly, styles.scss]", "    embed-resources: true", "    toc: false", "    page-layout: article",
  "    include-in-header:", "      text: |",
  '        <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap" rel="stylesheet">',
  '        <meta name="viewport" content="width=device-width, initial-scale=1">',
  "execute:", "  echo: false", "  warning: false", "  message: false",
  "knitr:", "  opts_knit:", sprintf('    root.dir: "%s"', root), "---", "",
  "```{r}", sprintf("WEEK <- %d", WEEK), "```", "", body),
  file.path(tmp, "share.qmd"), useBytes = TRUE)
st <- system2(QUARTO, c("render", shQuote(file.path(tmp, "share.qmd"))), stdout = "", stderr = "")
if (st != 0) stop("Share render failed")
dir.create("share", showWarnings = FALSE)
out <- file.path("share", sprintf("F1C-Week-%02d.html", WEEK))
file.copy(file.path(tmp, "share.html"), out, overwrite = TRUE)
message("Site page: docs/weeks/", stem, ".html\nShare copy: ", out, " (", round(file.size(out) / 1e6, 1), " MB)")
