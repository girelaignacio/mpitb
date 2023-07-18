#' Extract the coefficients from the calculated measures
#'
#' @param object an object of class `mpitb_measure`.
#' @param k numeric value vector of the poverty cut-offs (k) for which the coefficients are desired.
#' @param over character
#' @param level character
#' @param ... other arguments
#'
#' @return
#' @export
#'
#' @examples
coef.mpitb_measure <- function(object, k = NULL, over = NULL, level = NULL, ...) {

  cutoffs <- lapply(object, FUN = function(x) rep(attr(x,"k"), sum(sapply(x, FUN = function(y) length(y)))))
  cutoffs <- do.call("c",cutoffs)

  coeffs <- unlist(object)

  output <- cbind(cutoffs,coeffs)
  colnames(output) <- c("Cut-offs", "Coefficient")


  # check if `over`is character and unique
  if (!is.null(over)){
    coeffs <- subset(coeffs, grepl(over, rownames(coeffs)))
  }

  # check if `level`is character and unique
  if(!is.null(level)){
    coeffs <- subset(coeffs, grepl(level, rownames(coeffs)))
  }

  output
}
