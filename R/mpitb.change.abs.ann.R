#' Annualized absolute rate of change
#'
#' @description
#' Calculate the annualized absolute rate of change between two AF measures (e.g. \eqn{M_0}) which is simply the difference between the final \eqn{t^2} and the initial period \eqn{t^2} divided by the difference in the two time periods
#' \deqn{\bar{\Delta}(M_0)= \frac{M_0^{t^2} - M_0^{t^1}}{t^2 - t^1} }
#'
#' @param period_t1 a \code{mpitb_measure} class object.
#' @param period_t2 a \code{mpitb_measure} class object. Should be the same AF measure class as `period_t1` with the same parameters (cutoffs, subgroups, etc.).
#' @param ... other arguments
#'
#' @return \code{mpitb_change} and \code{mpitb_measure} class object
#' @export
#'
#' @rdname  mpitb.change_abs.ann
#'
#' @references \emph{Alkire, S., Roche, J. M., & Vaz, A. (2017). Changes over time in multidimensional poverty: Methodology and results for 34 countries. World Development, 94, 232-249}. 10.1016/j.worlddev.2017.01.011
#'
#' @examples
mpitb.change_abs.ann <- function(period_t1, period_t2, ...) UseMethod(" mpitb.change_abs.ann")

#' @rdname  mpitb.change_abs.ann
#' @export
mpitb.change_abs.ann <- function(period_t1, period_t2, ...){

  # .check if they are from the same mpitb_measure
  if(all(class(period_t1) != class(period_t2))){
    stop("Error: mpitb_measures are not from the same AF class measure")
  }

  # .check if they have the same cut-offs
  if(all(unique(retrieve.cutoffs(period_t1)) != unique(retrieve.cutoffs(period_t2)))){
    stop("Error: mpitb_mesures do not have the same cut-offs. These measures are not comparable")
  }

  yt1 <- attr(period_t1,"year")
  yt2 <- attr(period_t2,"year")

  ann.absolute.change <- function(t1,t2){
    dyt <- (yt2 - yt1)
    b <- (t2-t1)/dyt
    attributes(b) <- NULL
    change <- b
    se <- (sqrt(attr(t2,"se")^2 + attr(t1,"se")^2)) * (1/dyt)
    attr(change,"se") <- se
    attr(change, "lb") <- b - qnorm(0.975) * se
    attr(change, "ub") <- b + qnorm(0.975) * se
    names(change) <- names(t1)
    return(change)
  }
  mapply.ann.abs.change <- function(t1,t2) {
    ann.abs.change <- mapply(ann.absolute.change, t1,t2)
    attr(ann.abs.change,"k") <- attr(t1,"k")
    return(ann.abs.change)
  }

  annualized.absolute.change <- mapply(mapply.ann.abs.change, period_t1, period_t2, SIMPLIFY=FALSE)

  class(annualized.absolute.change) <- c("mpitb_change", "mpitb_measure")

  annualized.absolute.change
}
