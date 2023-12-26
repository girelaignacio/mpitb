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
    change <- t2 - t1
    attr(change,"se") <- sqrt(attr(t2,"se")^2 + attr(t1,"se")^2)
    attr(change, "k") <- attr(mpitb_measure_t1,"k")
    return(change)
  }
  mapply.abs.change <- function(t1,t2) mapply(absolute.change, t1,t2)

  absolute.change <- mapply(mapply.abs.change, mpitb_measure_t1, mpitb_measure_t2, SIMPLIFY=FALSE)

  class(absolute.change) <- c("mpitb_change.abs", "mpitb_measure")

  absolute.change
}
