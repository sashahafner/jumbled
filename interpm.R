# Replace missing values in multiple data frame columns with interpolated values

interpm <- function(dat, x, ys, ...) {

  for (i in ys) {
    rout <- which(is.na(dat[[i]])) 
    if (length(rout) > 0) {
      dat[[i]][rout] <- approx(dat[[x]][-rout], dat[[i]][-rout], xout = dat[[x]][rout], ...)$y 
    } 
  }

  return(dat)

}
