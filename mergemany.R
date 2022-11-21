# Base merge() but with any number of data frames

mergemany <- function(..., by = intersect(names(x), names(y)),
           by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all,
           sort = TRUE, suffixes = c(".x",".y"), no.dups = TRUE,
           incomparables = NULL) {

  dfl <- list(...)

  x <- dfl[[1]]

  for (y in dfl[-1]) {
    x <- merge(x, y, by = by, by.x = by.x, by.y = by.y, all = all, 
               all.x = all.x, all.y = all.y, sort = sort, 
               suffixes = suffixes, no.dups = no.dups, 
               incomparables = incomparables)
  }

  return(x)

}
