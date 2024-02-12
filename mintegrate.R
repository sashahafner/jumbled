# Integration of interval-based rate measurements

mintegrate <- function(x, y, method = 'midpoint', lwr = min(x), upr = max(x), ylwr = y[which.min(x)], value = 'all') {

  method <- substr(tolower(method), 1, 1)

  if (length(x) != length(y)) {
    stop('Lengths of x and y are not equal.')
  }

  # Sort
  ord <- (1:length(x))[order(x)]
  y <- y[order(x)]
  x <- x[order(x)]

  if (method == 'l') {
    a <- cumsum(y * diff(c(lwr, x)))
  }

  if (method == 'r') {
    a <- cumsum(y * diff(c(x, upr)))
  }

  if (method == 'm') {
    a <- cumsum(c(0, y[-length(y)] * diff(x)) / 2 +  y * diff(c(lwr, x)) / 2)
  }

  if (method == 't') {
    x <- c(lwr, x)
    y <- c(ylwr, y)
    a <- cumsum((y[-length(y)] + diff(y) / 2) * diff(x)) 
    x <- x[-1]
    y <- y[-1]
  }

  if (value == 'all') {
    return(a[ord])
  } else if (value == 'total') {
    return(a[which.max(x)])
  }

}
