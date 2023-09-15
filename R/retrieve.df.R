retrieve.df <- function(object){
  # retrieve degrees of freedom
  out <- unlist(lapply(object, FUN = function(X) unlist(sapply(X, FUN = function(x) attr(x,"df")), use.names = F)))
  out
}
