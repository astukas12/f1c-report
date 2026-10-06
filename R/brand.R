# Golden Ticket Sims sponsorship: the one place to set the link and promo used by the report and the cards.
# PLACEHOLDERS until Andrew supplies the real link / promo.

GTS_URL     <- "GTS_URL"          # e.g. the GTS home or sign-up page
GTS_PROMO   <- "GTS_PROMO"        # promo code or offer line; "" hides it
GTS_TAGLINE <- "Sims for the sports nobody else sims."
GTS_SPORTS  <- "NFL, NASCAR, golf, tennis, F1, MMA and more"
GTS_CTA     <- "Run the sims"

gts_link_ok <- function() !identical(GTS_URL, "GTS_URL") && nzchar(GTS_URL)

# Small logo for embedding (the full one is 1.7 MB); built once from assets/logo.png
logo_small <- function(px = 192) {
  out <- "assets/logo-sm.png"
  if (!file.exists(out)) {
    img <- png::readPNG("assets/logo.png")
    ragg::agg_png(out, width = px, height = px, background = "transparent")
    par(mar = c(0, 0, 0, 0)); plot.new(); rasterImage(img, 0, 0, 1, 1, interpolate = TRUE); dev.off()
  }
  base64enc::dataURI(file = out, mime = "image/png")
}

# Phone-width screenshots of the share copy: the page in a 390px iframe, cut into n tall frames
phone_shots <- function(html, n = 3, frame_h = 1400) {
  edge <- "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
  total <- n * frame_h
  wrap <- tempfile(fileext = ".html")
  writeLines(sprintf('<!doctype html><html><body style="margin:0;background:#0a0a0a"><iframe src="file:///%s" style="border:0;width:390px;height:%dpx;display:block"></iframe></body></html>',
                     normalizePath(html, "/"), total), wrap)
  full <- tempfile(fileext = ".png")
  system2(edge, c("--headless=new", "--disable-gpu", "--hide-scrollbars", "--force-device-scale-factor=2",
                  sprintf("--window-size=600,%d", total), "--virtual-time-budget=8000",
                  shQuote(paste0("--screenshot=", normalizePath(full, mustWork = FALSE))),
                  shQuote(paste0("file:///", normalizePath(wrap, "/")))), stdout = FALSE, stderr = FALSE)
  img <- png::readPNG(full)
  outs <- character()
  for (i in seq_len(n)) {
    rows <- ((i - 1) * frame_h * 2 + 1):min(i * frame_h * 2, dim(img)[1])
    out <- sub("\\.html$", sprintf("-phone-%d.png", i), html)
    png::writePNG(img[rows, 1:780, , drop = FALSE], out)
    outs <- c(outs, out)
  }
  outs
}
