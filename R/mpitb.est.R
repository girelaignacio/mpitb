#' Estimation of Alkire-Foster measures
#'
#' @param object a `mpitb_set` class object
#' @param measures a character vector with the desired AF measures. By default: c("M0", "A", "H", "Headcounts")
#' @param ... other arguments
#'
#' @return a `mpitb_est` class object which is a list with the same length as `measures` argument whose elements are `mpitb_measure` class objects of the desired AF measures
#' @export
#' @rdname mpitb.est
#'
#' @examples
#' library(mpitb)
#'
#' data_syn <- syn_cdta
#' data_syn1 <- subset(data_syn, t == 1)
#' data_syn1 <- survey::svydesign(id=~psu, weights = ~weight , strata = ~stratum, data = data_syn1)
#'
#' indicators <- c("d_nutr","d_cm","d_satt","d_educ", "d_elct","d_sani","d_wtr","d_hsg","d_ckfl","d_asst")
#' weights <- c(1/6,1/6,1/6,1/6, 1/18,1/18,1/18,1/18,1/18,1/18)
#' cutoff <- c(33,50)
#' over <- c("region","area")
#' set <- mpitb.set(data_syn1, indicators, weights, cutoff, subgroup = over, name = "syn_data", desc = "This is an example taken from the synthetic cleaned data from mpitb STATA package")
#'
#' # Asumme we want to calculate H and A measures
#'
#' estimations <- mpitb.est(set, measures = c("H","A"))
#' class(estimations)
#' length(estimations)
#'
mpitb.est <- function(object, ...) {UseMethod("mpitb.est", object)}

#' @rdname mpitb.est
#' @export
mpitb.est.mpitb_set <- function(object, measures = c("M0","H","A","censored.headcounts","headcounts"), ...){
  estimations <- mapply(mpitb.measure, measures , MoreArgs = list(object=object), SIMPLIFY = FALSE)
  class(estimations) <- "mpitb_est"
  return(estimations)
}
