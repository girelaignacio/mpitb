#' Absolute rate of change
#'
#' @description
#' Calculate the absolute rate of change between two AF measures (e.g. \eqn{M_0}) which is simply the difference between the final \eqn{t^2} and the initial period \eqn{t^2}
#' \deqn{\Delta(M_0)= M_0^{t^2} - M_0^{t^1}}
#'
#' @param period_t1 a \code{mpitb_measure} class object.
#' @param period_t2 a \code{mpitb_measure} class object. Should be the same AF measure class as `period_t1` with the same parameters (cutoffs, subgroups, etc.).
#' @param ... other arguments
#'
#' @return \code{mpitb_change} and \code{mpitb_measure} class object
#' @export
#'
#' @references \emph{Alkire, S., Roche, J. M., & Vaz, A. (2017). Changes over time in multidimensional poverty: Methodology and results for 34 countries. World Development, 94, 232-249}. 10.1016/j.worlddev.2017.01.011
#'
#' @rdname  mpitb.change_abs
#'
#'
#' @examples
mpitb.change_abs <- function(period_t1, period_t2, ...) UseMethod(" mpitb.change_abs")

#' @rdname  mpitb.change_abs
#' @export
mpitb.change_abs.default <- function(period_t1, period_t2, ...){

  # .check if they are from the same mpitb_measure
  if(all(class(period_t1) != class(period_t2))){
    stop("Error: mpitb_measures are not from the same AF class measure")
  }

  # .check if they have the same cut-offs
  if(all(unique(retrieve.cutoffs(period_t1)) != unique(retrieve.cutoffs(period_t2)))){
    stop("Error: mpitb_mesures do not have the same cut-offs. These measures are not comparable")
  }

  absolute.change <- function(t1,t2) {
    b <- t2 - t1
    attributes(b) <- NULL
    change <- b
    se <- sqrt(attr(t2,"se")^2 + attr(t1,"se")^2)
    #dfs <- attr(t1,"dfs") - 1 + attr(t2,"dfs") - 1
    attr(change,"se") <- se
    attr(change, "lb") <- b - stats::qnorm(0.975) * se
    attr(change, "ub") <- b + stats::qnorm(0.975) * se
    names(change) <- names(t1)
    return(change)
  }
  mapply.abs.change <- function(t1,t2) {
    abs.change <- mapply(absolute.change, t1,t2)
    attr(abs.change,"k") <- attr(t1,"k")
    return(abs.change)
  }

  absolute.change <- mapply(mapply.abs.change, period_t1, period_t2, SIMPLIFY=FALSE)

  class(absolute.change) <- c("mpitb_change", "mpitb_measure")

  absolute.change
}
