#' Extract the coefficients from the calculated measures
#'
#' @param object an object of class `mpitb_measure`.
#' @param k numeric value vector of the poverty cut-offs (k) for which the coefficients are desired.
#' @param over character
#' @param level character
#' @param ... other arguments
#'
#' @export
#'
#' @examples
#'
#' library(mpitb)
#' data <- survey::svydesign(id=~PSU, weights = ~Weight, strata = ~Strata,
#'         data = swz_mics14)
#' indicators <- c("Water","Assets","School","Nutrition")
#' weights <- c(1/6,1/6,1/3,1/3)
#' cutoff <- c(25,50)
#' over <- c("Region","Area")
#'
#' set <- mpitb.set(data, indicators, cutoff, weights, over,
#'       name = "Example", desc = "SWZ MICS survey 2014")
#'
#' M0 <- mpitb.M0(set)
#' as.data.frame(M0)
#' coef(M0)
#'

coef.mpitb_measure <- function(object, k = NULL, over = NULL, level = NULL, ...) {

  #cutoffs <- lapply(object, FUN = function(x) rep(attr(x,"k"), sum(sapply(x, FUN = function(y) length(y)))))
  #cutoffs <- do.call("c",cutoffs)
  cutoffs <- retrieve.cutoffs(object)

  #coeffs <- unlist(object)
  coeffs <- retrieve.coefficients(object)

  out <- cbind(cutoffs,coeffs)
  colnames(out) <- c("Cut-offs", "Coefficient")


  # check if `over`is character and unique
  if (!is.null(over)){
    coeffs <- subset(coeffs, grepl(over, rownames(coeffs)))
  }

  # check if `level`is character and unique
  if(!is.null(level)){
    coeffs <- subset(coeffs, grepl(level, rownames(coeffs)))
  }

  return(out)
}
