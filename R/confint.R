#' Extract the confident intervals from the calculated measures
#'
#' @param object an object of class `mpitb_measure`.
#' @param parm "coefficient"
#' @param level numeric
#' @param ... other arguments
#'
#' @return
#' @export
#'
#' @examples
confint.mpitb_measure <- function(object, parm, level, ...){

  if (!(parm == "coefficient")){stop("Only for coefficients")}

  cutoffs <- lapply(object, FUN = function(x) rep(attr(x,"k"), sum(sapply(x, FUN = function(y) length(y)))))
  cutoffs <- do.call("c",cutoffs)

  b <- unlist(object)

  se <- unlist(lapply(object, FUN = function(X) unlist(sapply(X, FUN = function(x) attr(x,"se")), use.names = F)))

  degfs <- unlist(lapply(object, FUN = function(X) unlist(sapply(X, FUN = function(x) attr(x,"df")), use.names = F)))

  fac <- stats::qt(level + (1 - level)/2, df=degfs) #fac <- qt(level,df=degfs-1)*se/sqrt(degfs)?
  lb <- b - fac * se
  ub <- b + fac * se

  output <- cbind(cutoffs,lb,ub)
  colnames(output) <- c("Cut-offs",paste("Lower Bound (",level*100,"%)", sep = ""), paste("Upper Bound (",level*100,"%)", sep = ""))
  rownames(output) <-  names(b)

  output
}

