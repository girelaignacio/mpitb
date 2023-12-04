mpitb.change.rel <- function(mpitb_measure_t1, mpitb_measure_t2){

  # .check if they are from the same mpitb_measure
  if(all(class(mpitb_measure_t1) != class(mpitb_measure_t2))){
    stop("Error: mpitb_measures are not from the same AF class measure")
  }

  # .check if year attribute in mpitb_measure
  if(attributes(mpitb_measure_t1)$year >= attributes(mpitb_measure_t1)$year){
    warning("mpitb_measures are not ordered. It is assumed that order of arguments can change according to `year' attribute")
    swap(mpitb_measure_t1, mpitb_measure_t2)
  }
  # .check if they have the same cut-offs
  if(all(unique(retrieve.cutoffs(mpitb_measure_t1)) != unique(retrieve.cutoffs(mpitb_measure_t2)))){
    stop("Error: mpitb_mesures do not have the same cut-offs. These measures are not comparable")
  }

  relative.change <- function(t1,t2) {
    # (y/x - 1)*100 standard error
    # 100 * sqrt((se.y^2/x^2)+((se.x^2*y^2)/(x^4)))
    change <- (t2 - t1)/t1 * 100
    attr(change,"se") <- 100 * sqrt((attr(t2,"se")^2/t1^2)+((attr(t1,"se")^2*t2^2)/(t1^4)))
    return(change)
  }
  mapply.rel.change <- function(t1,t2) mapply(relative.change, t1,t2)

  relative.change <- mapply(mapply.rel.change, mpitb_measure_t1, mpitb_measure_t2, SIMPLIFY=FALSE)

  relative.change
}
