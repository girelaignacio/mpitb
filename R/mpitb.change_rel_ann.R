#' Annualized relative rate of change
#'
#' @description
#' Calculate the annualized relative rate of an AF measure (e.g. \eqn{M_0}) which is the compound rate of reduction in per year between the initial and the final periods.
#' \deqn{\bar{\delta}(M_0)= \left[ \left( \frac{M_0^{t^2}}{M_0^{t^1}} \right)^{\frac{1}{t^2 - t^1}} - 1 \right] \times 100 }
#'
#' @param period_t1 a \code{mpitb_measure} class object.
#' @param period_t2 a \code{mpitb_measure} class object. Should be the same AF measure class as `period_t1` with the same parameters (cutoffs, subgroups, etc.).
#' @param ... other arguments
#'
#' @return \code{mpitb_change} and \code{mpitb_measure} class object
#'
#' @export
#'
#' @references \emph{Alkire, S., Roche, J. M., & Vaz, A. (2017). Changes over time in multidimensional poverty: Methodology and results for 34 countries. World Development, 94, 232-249}. 10.1016/j.worlddev.2017.01.011
#'
#' @rdname  mpitb.change_rel_ann
#'
#' @examples
mpitb.change_rel_ann <- function(period_t1, period_t2, ...) UseMethod(" mpitb.change_rel_ann")

#' @rdname  mpitb.change_rel_ann
#' @export
mpitb.change_rel_ann.default <- function(period_t1, period_t2){

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

  ann.relative.change <- function(t1,t2){
    dyt <- (yt2 - yt1)
    b <- ( ( (t2/t1)^(1/dyt) ) - 1) * 100
    attributes(b) <- NULL
    change <- b
    se <- 100 * sqrt((attr(t2,"se")^2/t1^2)+((attr(t1,"se")^2*t2^2)/(t1^4))) * (1/dyt)
    attributes(se) <- NULL
    attr(change,"se") <- se
    attr(change, "lb") <- b - stats::qnorm(0.975) * se
    attr(change, "ub") <- b + stats::qnorm(0.975) * se
    names(change) <- names(t1)
    return(change)
  }
  mapply.ann.rel.change <- function(t1,t2) {
    ann.rel.change <- mapply(ann.relative.change, t1,t2)
    attr(ann.rel.change,"k") <- attr(t1,"k")
    return(ann.rel.change)
  }

  annualized.relative.change <- mapply(mapply.ann.rel.change, period_t1, period_t2, SIMPLIFY=FALSE)

  class(annualized.relative.change) <- c("mpitb_change", "mpitb_measure")

  annualized.relative.change
}
