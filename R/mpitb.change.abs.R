mpitb.change.abs <- function(mpitb_measure_t0, mpitb_measure_t1){

  # .check if they are from the same mpitb_measure
  if(all(class(mpitb_measure_t0) != class(mpitb_measure_t1))){
    stop("Error: mpitb_measures are not from the same AF class measure")
  }

  # .check if they have the same cut-offs
  if(all(unique(retrieve.cutoffs(mpitb_measure_t0)) != unique(retrieve.cutoffs(mpitb_measure_t1)))){
    stop("Error: mpitb_mesures do not have the same cut-offs. These measures are not comparable")
  }

  absolute.change <- function(t0,t1) {
    b <- t1 - t0
    attributes(b) <- NULL
    change <- b
    se <- sqrt(attr(t1,"se")^2 + attr(t0,"se")^2)
    attr(change,"se") <- se
    attr(change, "lb") <- b - qnorm(0.975) * se
    attr(change, "ub") <- b + qnorm(0.975) * se
    names(change) <- names(t0)
    return(change)
  }
  mapply.abs.change <- function(t0,t1) {
    abs.change <- mapply(absolute.change, t0,t1)
    attr(abs.change,"k") <- attr(t0,"k")
    return(abs.change)
  }

  absolute.change <- mapply(mapply.abs.change, mpitb_measure_t0, mpitb_measure_t1, SIMPLIFY=FALSE)

  class(absolute.change) <- c("mpitb_change", "mpitb_measure")

  absolute.change
}
