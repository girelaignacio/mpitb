mpitb.change.abs <- function(mpitb_measure_t1, mpitb_measure_t2){

  # .check if they are from the same mpitb_measure
  if(all(class(mpitb_measure_t1) != class(mpitb_measure_t2))){
    stop("Error: mpitb_measures are not from the same AF class measure")
  }

  # .check if they have the same cut-offs
  if(all(unique(retrieve.cutoffs(mpitb_measure_t1)) != unique(retrieve.cutoffs(mpitb_measure_t2)))){
    stop("Error: mpitb_mesures do not have the same cut-offs. These measures are not comparable")
  }

  absolute.change <- function(t1,t2) {
    b <- t2 - t1
    attributes(b) <- NULL
    change <- b
    se <- sqrt(attr(t2,"se")^2 + attr(t1,"se")^2)
    attr(change,"se") <- se
    attr(change, "lb") <- b - qnorm(0.975) * se
    attr(change, "ub") <- b + qnorm(0.975) * se
    names(change) <- names(t1)
    return(change)
  }
  mapply.abs.change <- function(t1,t2) {
    abs.change <- mapply(absolute.change, t1,t2)
    attr(abs.change,"k") <- attr(t1,"k")
    return(abs.change)
  }

  absolute.change <- mapply(mapply.abs.change, mpitb_measure_t1, mpitb_measure_t2, SIMPLIFY=FALSE)

  class(absolute.change) <- c("mpitb_change.abs", "mpitb_measure")

  absolute.change
}
