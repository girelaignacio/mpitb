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
#' subgroup <- c("Region","Area")
#'
#' set <- mpitb.set(data, indicators, weights, cutoff, subgroup,
#'       name = "Example", desc = "SWZ MICS survey 2014")
#'
#' M0 <- mpitb.M0(set)
#' as.data.frame(M0)
mpitb.M0 <- function(object, ...) UseMethod("mpitb.M0", object)

#' @rdname mpitb.M0
#' @export
mpitb.M0.mpitb_set <- function(object, ...){
  data <- object$data
  subgroup <- object$subgroup
  score <- data$variables$score
  K <- object$K
  output <- vector("list", length = length(K))
  for (i in 1:length(K)){
    k <- K[i]/100
    censored.score <- censored.deprivations.score(score, k)
    data <- update.survey.design(data, score.k = censored.score)

    #by.list <- survey::svybys(survey::make.formula("score.k"), bys = survey::make.formula(subgroup), data, survey::svymean)
    by.list <- survey::svybys(survey::make.formula("score.k"), bys = survey::make.formula(subgroup), data, survey::svyciprop, vartype = c("se","ci"))

    M0 <- lapply(by.list, FUN = function(x) mpitb.measurebys(x, data))

    names(M0) <- subgroup
    attr(M0, "k") <- k*100
    output[[i]] <- M0
  }
  attr(output, "year") <- object$year
  class(output) <- c("mpitb_M0", "mpitb_measure")
  output
}
