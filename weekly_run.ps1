# F1C weekly report, Tuesday 3:45 AM ET (Windows task "F1C weekly report", 16 Nov 2026, Andrew).
# Builds cards + report on final MFL scores, retries every 30 min until 9 AM if the score check stops it,
# then commits the site/report files only and pushes to main.
# Status for the project thread: logs/weekly_run_status.txt (one line: OK|FAIL week url/reason)
param([int]$Week = 0)
$ErrorActionPreference = "Continue"
$env:HOME = "C:\Users\astuk"
Set-Location "C:\Users\astuk\OneDrive\Documents\nascaR\Fantasy\F1C_Review"
New-Item -ItemType Directory -Force logs | Out-Null
$R = "C:\Program Files\R\R-4.6.1\bin\Rscript.exe"
if ($Week -le 0) { $Week = 9 + [math]::Floor(((Get-Date).Date - [datetime]"2026-11-10").TotalDays / 7) }
$wk = "{0:D2}" -f $Week
$log = "logs\weekly_run_w$wk.log"
$status = "logs\weekly_run_status.txt"
function Log($m) { "$(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')  $m" | Tee-Object -FilePath $log -Append }
"RUNNING $Week started $(Get-Date -Format s)" | Set-Content $status -Encoding utf8
$deadline = (Get-Date).Date.AddHours(9)
$ok = $false
while (-not $ok) {
  Log "Week $Week attempt: make_cards"
  & $R make_cards.R $Week *>> $log
  $c = $LASTEXITCODE
  if ($c -eq 0) { Log "make_report"; & $R make_report.R $Week *>> $log; $c = $LASTEXITCODE }
  if ($c -eq 0 -and (Test-Path "docs\img\week-$wk-standings.png")) { $ok = $true; break }
  Log "Build stopped (exit $c)"
  if ((Get-Date).AddMinutes(30) -gt $deadline) { break }
  Start-Sleep -Seconds 1800
}
if (-not $ok) { "FAIL $Week build did not pass by 9 AM, see $log" | Set-Content $status -Encoding utf8; exit 1 }
git add docs data/cache img weeks archive.qmd *>> $log
git commit -m "Week $Week" *>> $log
$pushed = $false
foreach ($i in 1..4) { git push origin HEAD:main *>> $log; if ($LASTEXITCODE -eq 0) { $pushed = $true; break }; Start-Sleep -Seconds (15 * $i) }
if (-not $pushed) { "FAIL $Week built but push failed, see $log" | Set-Content $status -Encoding utf8; exit 1 }
Log "Pushed"
"OK $Week https://astukas12.github.io/f1c-report/img/week-$wk-standings.png" | Set-Content $status -Encoding utf8
