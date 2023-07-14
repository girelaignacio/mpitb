#' Extract the coefficients from the calculated measures by poverty cut-off
#'
#' @param object an object of class `mpitb_measure`.
#' @param cutoff numeric value vector of the poverty cut-offs (k) for which the coefficients are desired.
#' @param over character
#' @param level character
#' @param ... other arguments
#'
#' @return
#' @export
#'
#' @examples
coef.mpitb_measure <- function(object, cutoff = NULL, over = NULL, level = NULL, ...) {
  # check if `cutoff`is numeric and unique
  coeffs <- sapply(object, FUN = function(x) if(attr(x,"k") == cutoff) unlist(x))

  # check if `over`is character and unique
  if (!is.null(over)){
    coeffs <- subset(coeffs, grepl(over, rownames(coeffs)))
  }

  # check if `level`is character and unique
  if(!is.null(level)){
    coeffs <- subset(coeffs, grepl(level, rownames(coeffs)))
  }

  colnames(coeffs) <- "coefficient"
  coeffs
}
