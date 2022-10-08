# expand.grid analog that accepts (any number of) data frame (or vector) arguments
dfcombos <- function(..., fixrows = TRUE) {

  d <- list(...)
  
  d <- lapply(d, as.data.frame)
  i <- lapply(d, function(x) seq(nrow(x)))
  
  ind <- expand.grid(i)
  
  for (j in 1:length(d)) {
    if (j > 1) {
      res <- cbind(res, d[[j]][ind[, j], , drop = FALSE])
    } else {
      res <- d[[j]][ind[, j], , drop = FALSE]
    }
  }

  if (isTRUE(fixrows)) {
    rownames(res) <- 1:nrow(res)
  }
  
  return(res)
}
