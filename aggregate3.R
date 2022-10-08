# Version of aggregate function that accepts multiple FUN functions
# Note that FUN is a named list of functions
# This one differs from aggregat2 in that it uses the formula notation

aggregate3 <- function(dat, form, FUN, ...) {

  for (i in 1:length(FUN)) {
    d <- aggregate(x = form, data = dat, FUN = FUN[[i]]) 
    nn <- gsub(' ', '', unlist(strsplit(strsplit(as.character(form), '~')[[3]], '\\+')))
    names(d)[!names(d) %in% nn] <- paste0(names(d)[!names(d) %in% nn], '.', names(FUN)[[i]])
    if (i == 1) {
      res <- d
    } else {
      res <- cbind(res, d[, !names(d) %in% nn, drop = FALSE])
    }
  }

  return(res)

}
