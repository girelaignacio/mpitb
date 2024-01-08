#' print() method for "mpitb_measure" class
#'
#' @param x an object of class "mpitb_measure"
#' @param ... other arguments passed to or from other methods
#'
#' @export
print.mpitb_measure <- function(x, ...) {
  cat("\nCall: ", deparse(attr(x,"call")), "\n\n")

  cat("Estimated AF measure:", sub(".*_","",class(x)[1]),"\n\n")

  cat("\t Confidence intervals are estimated with a ",attr(x,"level")*100,"% level of confidence")
}
