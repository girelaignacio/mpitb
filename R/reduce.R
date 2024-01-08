reduce.svyby <- function(x, indicator = NULL, ...) {
  rownames(x) <- NULL
  colnames(x) <- c("level","coefficient","se","lb","ub")
  keep.colnames <- colnames(x)
  attributes(x) <- NULL
  x <- as.data.frame(x); colnames(x) <- keep.colnames
  if(!is.null(indicator)){x$indicator <- indicator}
  x
}
