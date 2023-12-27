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
  subgroup <- object$subgroup
  score <- data$variables$score
  K <- object$K
  output <- vector("list", length = length(K))
  for (i in 1:length(K)){
    k <- K[i]/100
    poor.mpi <- ifelse(score >= k,1,0)
    data <- update.survey.design(data, mpi.k = poor.mpi)
    mylist <- survey::svybys(survey::make.formula("mpi.k"), bys = survey::make.formula(subgroup), data, survey::svymean)

    H <- lapply(mylist, FUN = function(x) mpitb.measurebys(x, data))

    names(H) <- subgroup

    H <- lapply(H, "*",100)

    attr(H, "k") <- k*100
    output[[i]] <- H
  }
  attr(output, "Year") <- object$year
  class(output) <- c("mpitb_H", "mpitb_measure")
  output
}
