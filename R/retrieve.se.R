retrieve.se <- function(object){
  # retrieve standard errors
  out <- unlist(lapply(object, FUN = function(X) unlist(sapply(X, FUN = function(x) attr(x,"se")), use.names = F)))
  out
}
