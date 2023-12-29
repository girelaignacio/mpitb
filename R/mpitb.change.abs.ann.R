mpitb.change.abs.ann <- function(mpitb_measure_t0, mpitb_measure_t1){

  # .check if they are from the same mpitb_measure
  if(all(class(mpitb_measure_t0) != class(mpitb_measure_t1))){
    stop("Error: mpitb_measures are not from the same AF class measure")
  }

  # .check if they have the same cut-offs
  if(all(unique(retrieve.cutoffs(mpitb_measure_t0)) != unique(retrieve.cutoffs(mpitb_measure_t1)))){
    stop("Error: mpitb_mesures do not have the same cut-offs. These measures are not comparable")
  }

  yt0 <- attr(mpitb_measure_t0,"year")
  yt1 <- attr(mpitb_measure_t1,"year")

  ann.absolute.change <- function(t0,t1){
    dyt <- (yt1 - yt0)
    b <- (t1-t0)/dyt
    attributes(b) <- NULL
    change <- b
    se <- (sqrt(attr(t1,"se")^2 + attr(t0,"se")^2)) * (1/dyt)
    attr(change,"se") <- se
    attr(change, "lb") <- b - qnorm(0.975) * se
    attr(change, "ub") <- b + qnorm(0.975) * se
    names(change) <- names(t0)
    return(change)
  }
  mapply.ann.abs.change <- function(t0,t1) {
    ann.abs.change <- mapply(ann.absolute.change, t0,t1)
    attr(ann.abs.change,"k") <- attr(t0,"k")
    return(ann.abs.change)
  }

  annualized.absolute.change <- mapply(mapply.ann.abs.change, mpitb_measure_t0, mpitb_measure_t1, SIMPLIFY=FALSE)

  class(annualized.absolute.change) <- c("mpitb_change", "mpitb_measure")

  annualized.absolute.change
}
