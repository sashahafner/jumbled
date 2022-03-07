# Version of aggregate function that accepts multiple FUN functions
# Note that FUN is a named list of functions

# Example
#dat <- data.frame(grp = rep(1:3, each = 3), a = rnorm(9), b = runif(9))
#dat
#aggregate(dat[, c('a', 'b')], dat[, c('grp'), drop = FALSE], mean)
#aggregate2(dat[, c('a', 'b')], dat[, c('grp'), drop = FALSE], list(mean = mean, sd = sd, n = length))

aggregate2 <- function(x, by, FUN, ...) {

  for (i in 1:length(FUN)) {
    d <- aggregate(x = x, by = by, FUN = FUN[[i]]) 
    names(d)[!names(d) %in% names(by)] <- paste0(names(d)[!names(d) %in% names(by)], '.', names(FUN)[[i]])
    if (i == 1) {
      res <- d
    } else {
      res <- cbind(res, d[, !names(d) %in% names(by), drop = FALSE])
    }
  }

  return(res)

}
