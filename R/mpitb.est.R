#' Estimation of Alkire-Foster measures
#'
#' @param object a `mpitb_set` class object
#' @param measures Character vector with the desired AF measures. By default: c("M0", "A", "H", "Headcounts")
#' @param ...
#'
#' @return
#' @export
#' @rdname mpitb.headcounts
#'
#' @examples
mpitb.est <- function(object, measures, ...) {UseMethod("mpitb.est", object)}

#' @rdname mpitb.headcounts
#' @export
mpitb.est.mpitb_set <- function(object, measures = c("M0","H","A","Headcounts"), ...){
  estimation <- vector("list", length = length(measures))
  names(estimation) <- measures
  if ("M0" %in% measures) {estimation[["M0"]] <- mpitb.M0(object)}

  if ("H" %in% measures) {estimation[["H"]] <- mpitb.H(object)}

  if ("A" %in% measures) {estimation[["A"]] <- mpitb.A(object)}

  if ("Headcounts" %in% measures) {estimation[["Headcounts"]] <- mpitb.headcounts(object)}

  class(estimation) <- "mpitb_est"
  return(estimation)
}
