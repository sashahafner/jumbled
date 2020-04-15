# Rounddf function for export tables

rounddf <- function(x, digits = rep(2, ncol(x)), func = round) {
  if (length(digits) == 1) digits = rep(digits, ncol(x))
  for(i in 1:ncol(x)) if(class(x[, i])[1] == 'numeric') x[, i] <- func(x[, i], digits[i])
  return(x)
}
