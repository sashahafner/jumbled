# Create groups based on consecutive values of a variable

consec <- function(x, value = 'int') {

  x <- as.integer(as.factor(x))
  g <- cumsum(c(0, abs(diff(x))))
  g <- as.integer(factor(g, levels = g[!duplicated(g)]))

  if (tolower(substr(value, 1, 1)) == 'f') {
    g <- factor(g)
  }

  return(g)

}
