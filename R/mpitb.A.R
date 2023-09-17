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
#' #' library(mpitb)
#' data <- survey::svydesign(id=~PSU, weights = ~Weight, strata = ~Strata,
#'         data = swz_mics14)
#' indicators <- c("Water","Assets","School","Nutrition")
#' weights <- c(1/6,1/6,1/3,1/3)
#' cutoff <- c(25,50)
#' over <- c("Region","Area")
#'
#' set <- mpitb.set(data, indicators, cutoff, weights, over,
#'       name = "Example", desc = "SWZ MICS survey 2014")
#'
#' A <- mpitb.A(set)
#' as.data.frame(A)
#'

mpitb.A <- function(object, ...) UseMethod("mpitb.A", object)

#' @rdname mpitb.A
#' @export
mpitb.A.mpitb_set <- function(object, ...){
  data <- object$data
  over <- object$over
  c.score <- data$variables$c.score
  K <- object$K
  output <- vector("list", length = length(K))
  for (i in 1:length(K)){
    k <- K[i]
    poor.mpi <- as.factor(ifelse(c.score >= k,1,0))
    censored.c.score <- censored.deprivations.score(c.score, k)
    data <- update.survey.design(data, c.k = censored.c.score, mpi.k = poor.mpi)
    mylist <- svybys(survey::make.formula("c.k"), survey::make.formula(over), design = subset(data, data$variables[,'mpi.k']==1), survey::svymean)

    A <- lapply(mylist, FUN = function(x) mpitb.measure(x, data))

    names(A) <- over

    A <- lapply(A, "*",100)

    attr(A, "k") <- k*100
    output[[i]] <- A
  }
  attr(output, "Year") <- object$year
  class(output) <- c("mpitb_A", "mpitb_measure")
  output
}
