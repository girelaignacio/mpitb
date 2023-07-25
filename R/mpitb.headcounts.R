#' Censored Headcount Ratios
#'
#' Calculate the censored headcount ratios for each indicator or the proportion of people who are multidimensionally poor and deprived in each indicator
#'
#' @param object `mpitb_set` object
#' @param ... other arguments
#'
#' @return `mpitb_headcounts` and `mpitb_measure` class
#' @export
#' @rdname mpitb.headcounts
#'
#' @examples
mpitb.headcounts <- function(object, ...) UseMethod("mpitb.headcounts", object)

#' @rdname mpitb.headcounts
#' @export
mpitb.headcounts.mpitb_set <- function(object, ...) {
  data <- object$data
  over <- object$over
  indicators <- object$indicators
  K <- object$K
  output <- vector("list", length = length(K))
  for (i in 1:length(K)){
    k <- K[i]
    mylist <- survey::svybys(survey::make.formula(indicators), bys = survey::make.formula(over),data,survey::svymean)

    Hj <- lapply(mylist, FUN = function(x) mpitb.measure(x, data))
    names(Hj) <- over
    attr(Hj, "k") <- k
    output[[i]] <- Hj
  }
  class(output) <- c("mpitb_headcounts", "mpitb_measure")
  output
}
