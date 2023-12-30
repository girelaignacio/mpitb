#' Adjusted Headcount Ratio (\eqn{M_0})
#'
#' @description
#' Calculate the Adjusted Headcount Ratio (\eqn{M_0})
#' \deqn{M_0 = \frac{1}{n} \sum_{i=1}^n c_i(k) = \frac{q}{n} \times \frac{1}{q} \sum_{i=1}^n c_i(k) = H \times A} where  \eqn{c_i(k) = \sum_{j=1}^d w_j d_{ij}(k)} is the censored deprivation score
#' for each observation, \eqn{n} is the total number of observations, and \eqn{q} is the number of people identified as multidimensional poor.
#'
#' @param object a \code{mpitb_set} class object.
#' @param ... other arguments
#'
#' @return \code{mpitb_M0} and \code{mpitb_measure} class objects
#'
#' @export
#'
#' @references \emph{Alkire, S., Foster, J. E., Seth, S., Santos, M. E., Roche, J., & Ballon, P. (2015). Multidimensional poverty measurement and analysis. Oxford University Press.}
#'
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
#'
#' ## to observe the results in a data.frame format
#' as.data.frame(M0)
mpitb.M0 <- function(object, ...) UseMethod("mpitb.M0", object)

#' @rdname mpitb.M0
#' @export
mpitb.M0.mpitb_set <- function(object, ...){
  data <- object$data
  subgroup <- object$subgroup
  score <- data$variables$score
  K <- object$K
  level <- attr(object, "level")
  output <- vector("list", length = length(K))
  for (i in 1:length(K)){
    k <- K[i]/100
    censored.score <- censored.deprivations.score(score, k)
    data <- update.survey.design(data, score.k = censored.score)

    #by.list <- survey::svybys(survey::make.formula("score.k"), bys = survey::make.formula(subgroup), data, survey::svymean)
    by.list <- survey::svybys(survey::make.formula("score.k"), bys = survey::make.formula(subgroup), data, survey::svyciprop, vartype = c("se","ci"), level = level)

    M0 <- lapply(by.list, FUN = function(x) mpitb.measurebys(x, data))

    names(M0) <- subgroup
    attr(M0, "k") <- k*100
    output[[i]] <- M0
  }
  attr(output, "year") <- object$year
  class(output) <- c("mpitb_M0", "mpitb_measure")
  output
}
