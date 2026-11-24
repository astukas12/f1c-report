# edge.R: run headless Edge so it can never hang the caller (24 Nov 2026).
# Why: make_preview.R's screenshots (card.R shoot(), brand.R phone_shots()) call msedge --headless --screenshot via system2 with no time limit.
# On 20 Nov 12:02 and 22 Nov 09:34 Edge did not exit after the screenshot; make_preview.R waited on it for days, and live_refresh.R's 45-minute
# timeout only stops its immediate child on Windows, so the whole tree (cmd -> Rscript -> make_preview -> msedge) stayed alive and the GTS overnight
# queue saw "another R job running" for 4 days. Fix: give each Edge run its own --user-data-dir, a hard time limit, and kill every process of that profile afterwards.
EDGE_EXE <- "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
edge_run <- function(args, timeout = 120, exe = EDGE_EXE) {
  prof <- tempfile("edge_prof_"); dir.create(prof)
  args <- c(args, shQuote(paste0("--user-data-dir=", normalizePath(prof, "/"))), "--no-first-run", "--disable-extensions")
  st <- tryCatch(suppressWarnings(system2(exe, args, stdout = FALSE, stderr = FALSE, timeout = timeout)), error = function(e) 124L)
  # kill anything still alive that carries this profile (the screenshot is written before Edge is supposed to exit)
  key <- gsub("'", "''", basename(prof))
  try(system2("powershell", c("-NoProfile", "-Command", shQuote(sprintf("Get-CimInstance Win32_Process -Filter \"Name='msedge.exe'\" | Where-Object { $_.CommandLine -like '*%s*' } | ForEach-Object { taskkill /PID $_.ProcessId /T /F | Out-Null }", key))), stdout = FALSE, stderr = FALSE, timeout = 30), silent = TRUE)
  unlink(prof, recursive = TRUE, force = TRUE)
  invisible(st)
}
