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
#' set <- mpitb.set(data, indicators, weights, cutoff, over,
#'       name = "Example", description = "SWZ MICS survey 2014")
#'
#' M0 <- mpitb.M0(set)
#' coef(M0)
#'

coef.mpitb_measure <- function(object, k = NULL, over = NULL, level = NULL, ...) {


  x <- as.data.frame(object)

  if ("indicator" %in% colnames(x)){
    x <- x[,c("subgroup","level","k","indicator","coefficient")]
  }else{x <- x[,c("subgroup","level","k","coefficient")]}


  # # check if `over`is character and unique
  # if (!is.null(over)){
  #   coeffs <- subset(coeffs, grepl(over, rownames(coeffs)))
  # }
  #
  # # check if `level`is character and unique
  # if(!is.null(level)){
  #   coeffs <- subset(coeffs, grepl(level, rownames(coeffs)))
  # }

  return(x)
}
