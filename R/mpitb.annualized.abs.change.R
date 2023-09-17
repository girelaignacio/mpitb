mpitb.annualized.abs.change <- function(mpitb_measure_t1, mpitb_measure_t2){

  # .check if they are from the same mpitb_measure
  if(all(class(mpitb_measure_t1) != class(mpitb_measure_t2))){
    stop("Error: mpitb_measures are not from the same AF class measure")
  }

  # .check if year attribute in mpitb_measure
  if(attributes(mpitb_measure_t1)$year >= attributes(mpitb_measure_t1)$year){
    warning("mpitb_measures are not ordered. It is assumed that order of arguments can change according to `year' attribute")
    .swap(mpitb_measure_t1, mpitb_measure_t2)
  }
  # .check if they have the same cut-offs
  if(all(unique(retrieve.cutoffs(mpitb_measure_t1)) != unique(retrieve.cutoffs(mpitb_measure_t2)))){
    stop("Error: mpitb_mesures do not have the same cut-offs. These measures are not comparable")
  }

  t1.year <- attributes(mpitb_measure_t1)$year
  t2.year <- attributes(mpitb_measure_t2)$year

  ann.absolute.change <- function(t1,t2){

    change <- (t2-t1)/(t2.year - t1.year)

    return(change)
  }
  mapply.ann.abs.change <- function(t1,t2) mapply(ann.absolute.change, t1,t2)

  annualized.absolute.change <- mapply(mapply.ann.abs.change, mpitb_measure_t1, mpitb_measure_t2, SIMPLIFY=FALSE)

  annualized.absolute.change
}
