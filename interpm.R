# Replace missing values in multiple data frame columns with interpolated values

interpm <- function(dat, x, ys, ...) {

  for (i in ys) {
    rout <- which(is.na(dat[, i])) 
    dat[rout, i] <- approx(dat[, x], dat[, i], xout = rout, ...)$y 
  }

  return(dat)

}

