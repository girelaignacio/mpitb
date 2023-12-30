#' Incidence of poverty (\eqn{H})
#'
#' Calculate the incidence of poverty (\eqn{H}) which is the proportion people who are identified as poor in multidimensional terms, e.g.,
#' is a function that counts the number of observations whose censored deprivation score is greater than zero \eqn{q = \sum_{i=1}^n 1_{{c_i(k) > 0}}} divided by the total number of population (\eqn{n}).
#' \deqn{H = \frac{q}{n}}
#'
#' @param object a \code{mpitb_set} class object
#' @param ... other arguments
#'
#' @return \code{mpitb_H} and \code{mpitb_measure} class objects
#'
#' @export
#'
#' @references \emph{Alkire, S., Foster, J. E., Seth, S., Santos, M. E., Roche, J., & Ballon, P. (2015). Multidimensional poverty measurement and analysis. Oxford University Press.}
#'
#' @rdname mpitb.H
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
#' H <- mpitb.H(set)
#'
#' ## to observe the results in a data.frame format
#' as.data.frame(H)
mpitb.H <- function(object, ...) UseMethod("mpitb.H", object)

#' @rdname mpitb.H
#' @export
mpitb.H.mpitb_set <- function(object, ...){
  data <- object$data
  subgroup <- object$subgroup
  score <- data$variables$score
  K <- object$K
  level <- attr(object, "level")
  output <- vector("list", length = length(K))
  for (i in 1:length(K)){
    k <- K[i]/100
    poor.mpi <- ifelse(score >= k,1,0)
    data <- update.survey.design(data, mpi.k = poor.mpi)
    by.list <- survey::svybys(survey::make.formula("mpi.k"), bys = survey::make.formula(subgroup), data, survey::svyciprop, vartype = c("se","ci"), level = level)

    H <- lapply(by.list, FUN = function(x) mpitb.measurebys(x, data))

    names(H) <- subgroup

    #H <- lapply(H, "*",100)

    attr(H, "k") <- k*100
    output[[i]] <- H
  }
  attr(output, "Year") <- object$year
  class(output) <- c("mpitb_H", "mpitb_measure")
  output
}
