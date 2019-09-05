gapline <- function(ts, col) {
  dt <- diff(ts)
  ks <- which(dt > 36 * 60 * 60)
  if (length(ks) > 0) 
    abline(v = .POSIXct(ts[ks] + dt[ks]/2, "GMT"), lwd = 2, 
           col = col)
}