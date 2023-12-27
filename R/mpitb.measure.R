#' Alkire-Foster measure
#'
#' Calculate any available AF measure (Adjusted Headcount Ratio - M0 -, Incidence - H -, Intensity - A - and censored and uncensored indicators headcount ratios )
#'
#' @param object `mpitb_set` object
#' @param measure desired AF measure. The options are "M0", "H", "A", "censored.headcounts", "headcounts".
#' @param ... other arguments
#'
#' @return a `mpitb_measure` class object
#' @export
#' @rdname mpitb.measure
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
#' # Asumme we want to calculate M0 measure
#'
#' M0<- mpitb.measure(set, measure = "M0")

mpitb.measure <- function(object, ...) UseMethod("mpitb.measure", object)

#' @rdname mpitb.measure
#' @export
mpitb.measure.mpitb_set <- function(object, measure = c("M0","H","A","censored.headcounts","headcounts"), ...){
    selected.measure <- switch(measure,
                               M0 = mpitb.M0(object),
                               H = mpitb.H(object),
                               A = mpitb.A(object),
                               censored.headcounts = mpitb.headcounts(object, censored = T),
                               headcounts = mpitb.headcounts(object, censored = F)
    )
    selected.measure
  }
