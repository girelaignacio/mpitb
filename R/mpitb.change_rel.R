#' Relative rate of change
#'
#' @description
#' Calculate the relative rate of change between two AF measures (e.g. \eqn{M_0}) which is the difference in poverty as a percentage of the initial poverty level \eqn{t^1}.
#' \deqn{\delta(M_0)= \frac{M_0^{t^2} - M_0^{t^1}}{M_0^{t^1}} \times 100 }
#'
#' @param period_t1 an \code{mpitb_measure} class object.
#' @param period_t2 a \code{mpitb_measure} class object. Should be the same AF measure class as `period_t1` with the same parameters (cutoffs, subgroups, etc.).
#'
#' @return \code{mpitb_change} and \code{mpitb_measure} class object
#'
#' @export
#'
#' @references \emph{Alkire, S., Roche, J. M., & Vaz, A. (2017). Changes over time in multidimensional poverty: Methodology and results for 34 countries. World Development, 94, 232-249}. 10.1016/j.worlddev.2017.01.011
#'
#' @rdname  mpitb.change_rel
#'
#' @examples
mpitb.change_rel <- function(period_t1, period_t2, ...) UseMethod(" mpitb.change_rel")

#' @rdname  mpitb.change_rel
#' @export
mpitb.change_rel.default <- function(period_t0, period_t1, ...){

  # .check if they are from the same mpitb_measure
  if(all(class(period_t0) != class(period_t1))){
    stop("Error: mpitb_measures are not from the same AF class measure")
  }

  # .check if they have the same cut-offs
  if(all(unique(retrieve.cutoffs(period_t0)) != unique(retrieve.cutoffs(period_t1)))){
    stop("Error: mpitb_mesures do not have the same cut-offs. These measures are not comparable")
  }

  relative.change <- function(t0,t1) {
    b <- (t1 - t0)/t0 * 100
    attributes(b) <- NULL
    change <- b
    se <- 100 * sqrt((attr(t1,"se")^2/t0^2)+((attr(t0,"se")^2*t1^2)/(t0^4)))
    attributes(se) <- NULL
    attr(change,"se") <- se
    attr(change, "lb") <- b - stats::qnorm(0.975) * se
    attr(change, "ub") <- b + stats::qnorm(0.975) * se
    names(change) <- names(t0)
    return(change)
  }
  mapply.rel.change <- function(t0,t1) {
    rel.change <- mapply(relative.change, t0,t1)
    attr(rel.change,"k") <- attr(t0,"k")
    return(rel.change)
  }

  relative.change <- mapply(mapply.rel.change, period_t0, period_t1, SIMPLIFY=FALSE)

  class(relative.change) <- c("mpitb_change", "mpitb_measure")

  relative.change
}
