#' Intensity (A)
#'
#' Calculates the Intensity of poverty or the average deprivation score of the multidimensionally poor people
#'
#' @param object `mpitb_set` object
#' @param ... other arguments
#'
#' @return `mpitb_A` and `mpitb_measure` class
#' @export
#' @rdname mpitb.A
#'
#' @examples

mpitb.A <- function(object, ...) UseMethod("mpitb.A", object)

#' @rdname mpitb.A
#' @export
mpitb.A.mpitb_set <- function(object, ...){
  data <- object$data
  subgroup <- object$subgroup
  score <- data$variables$score
  K <- object$K
  output <- vector("list", length = length(K))
  for (i in 1:length(K)){
    k <- K[i]/100
    poor.mpi <- as.factor(ifelse(score >= k,1,0))
    censored.score <- censored.deprivations.score(score, k)
    data <- update.survey.design(data, score.k = censored.score, mpi.k = poor.mpi)
    mylist <- survey::svybys(survey::make.formula("score.k"), survey::make.formula(subgroup), design = subset(data, data$variables[,'mpi.k']==1), survey::svymean)

    A <- lapply(mylist, FUN = function(x) mpitb.measure(x, data))

    names(A) <- subgroup

    A <- lapply(A, "*",100)

    attr(A, "k") <- k*100
    output[[i]] <- A
  }
  attr(output, "Year") <- object$year
  class(output) <- c("mpitb_A", "mpitb_measure")
  output
}
