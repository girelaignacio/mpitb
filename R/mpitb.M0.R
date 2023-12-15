#' Adjusted Headcount Ratio (M0)
#'
#' Calculate the Adjusted Headcount Ratio (M0 Alkire-Foster class measure)
#'
#' @param object `mpitb_set` object
#' @param ... other arguments
#'
#' @return `mpitb_M0` and `mpitb_measure` class
#'
#' @export
#' @rdname mpitb.M0
#'
#' @examples
#' library(mpitb)
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
#' M0 <- mpitb.M0(set)
#' as.data.frame(M0)
mpitb.M0 <- function(object, ...) UseMethod("mpitb.M0", object)

#' @rdname mpitb.M0
#' @export
mpitb.M0.mpitb_set <- function(object, ...){
  data <- object$data
  over <- object$over
  c.score <- data$variables$c.score
  K <- object$K
  output <- vector("list", length = length(K))
  for (i in 1:length(K)){
    k <- K[i]
    censored.c.score <- censored.deprivations.score(c.score, k)
    data <- update.survey.design(data, c.k = censored.c.score)
    mylist <- svybys(survey::make.formula("c.k"), bys = survey::make.formula(over), data, survey::svymean)

    M0 <- lapply(mylist, FUN = function(x) mpitb.measure(x, data))

    names(M0) <- over
    attr(M0, "k") <- k*100
    output[[i]] <- M0
  }
  attr(output, "year") <- object$year
  class(output) <- c("mpitb_M0", "mpitb_measure")
  output
}
