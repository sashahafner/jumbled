# Integration of interval-based rate measurements

mintegrate <- function(x, y, method = 'midpoint', lwr = min(x), upr = max(x), ylwr = y[which.min(x)], value = 'all', by = NULL) {

  method <- substr(tolower(method), 1, 1)

  if (length(x) != length(y)) {
    stop('Lengths of x and y are not equal.')
  }

  # Grouped operation
  if (!is.null(by)) {
    dat <- data.frame(x = x, y = y, g = by)
    res <- by(dat, dat$g, function(d) 
              mintegrate(d$x, d$y, method = method, lwr = lwr, 
                         upr = upr, ylwr = ylwr, value = value), simplify = FALSE)
    return(as.numeric(do.call(c, res)))
  }

  # Check for duplicates
  if (any(duplicated(x))) {
    stop('Duplicates in x.')
  }

  # Convert x to numeric (could be difftime for example)
  if (!is.numeric(x) && !is.integer(x)) {
    warning('Converting x to numeric. Check values with value = "xy".')
    x <- as.numeric(x)
  }

  # Sort
  ord <- (1:length(x))[order(x)]
  y <- y[order(x)]
  x <- x[order(x)]

  if (method == 'l') {
    a <- cumsum(y * diff(c(lwr, x)))
  } else if (method == 'r') {
    a <- cumsum(y * diff(c(x, upr)))
  } else if (method == 'm') {
    a <- cumsum(c(0, y[-length(y)] * diff(x)) / 2 +  y * diff(c(lwr, x)) / 2)
  } else if (method == 't') {
    x <- c(lwr, x)
    y <- c(ylwr, y)
    a <- cumsum((y[-length(y)] + diff(y) / 2) * diff(x)) 
    x <- x[-1]
    y <- y[-1]
  } else {
    stop('method argument not recognized')
  }

  if (value == 'all') {
    return(a[order(ord)])
  } else if (value == 'xy') {
    return(cbind(x[order(ord)], a[order(ord)]))
  } else if (value == 'total') {
    return(a[which.max(x)])
  } else {
    stop('value argument not recognized.')
  }

}
