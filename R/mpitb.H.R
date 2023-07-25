#' Incidence (H)
#'
#' Calculate the Incidence of poverty or the proportion of multidimensionally poor people.
#'
#' @param object `mpitb_set` object
#' @param ... other arguments
#'
#' @return `mpitb_H` and `mpitb_measure` class
#'
#' @export
#' @rdname mpitb.H
#'
#' @examples
mpitb.H <- function(object, ...) UseMethod("mpitb.H", object)

#' @rdname mpitb.H
#' @export
mpitb.H.mpitb_set <- function(object, ...){
  data <- object$data
  over <- object$over
  c.score <- data$variables$c.score
  K <- object$K
  output <- vector("list", length = length(K))
  for (i in 1:length(K)){
    k <- K[i]
    poor.mpi <- ifelse(c.score >= k,1,0)
    data <- update.survey.design(data, mpi.k = poor.mpi)
    mylist <- svybys(survey::make.formula("mpi.k"), bys = survey::make.formula(over), data, survey::svymean)

    H <- lapply(mylist, FUN = function(x) mpitb.measure(x, data))

    names(H) <- over

    H <- lapply(H, "*",100)

    attr(H, "k") <- k
    output[[i]] <- H
  }
  class(output) <- c("mpitb_H", "mpitb_measure")
  output
}