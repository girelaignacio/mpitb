retrieve.cutoffs <- function(object){
  out <- lapply(object, FUN = function(x) rep(attr(x,"k"), sum(sapply(x, FUN = function(y) nrow(y)))))
  out <- do.call("c",out)
  out
}
