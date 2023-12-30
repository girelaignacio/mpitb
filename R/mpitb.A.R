#' Intensity of poverty (\eqn{A})
#'
#' Calculates the intensity of poverty (\eqn{A}) which the average deprivation score of the people identified as multidimensional poor, i.e.,
#' is the sum of the \eqn{n} censored deprivations scores (\eqn{c_i(k)}) divided by the number of multidimensional poor people (\eqn{q}).
#' \deqn{A = \frac{1}{q} \sum_{i=1}^{n} c_i(k) }
#'
#' @param object `mpitb_set` object
#' @param ... other arguments
#'
#' @return `mpitb_A` and `mpitb_measure` class
#'
#' @export
#'
#' @references \emph{Alkire, S., Foster, J. E., Seth, S., Santos, M. E., Roche, J., & Ballon, P. (2015). Multidimensional poverty measurement and analysis. Oxford University Press.}
#'
#' @rdname mpitb.A
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
#' A <- mpitb.A(set)
#'
#' ## to observe the results in a data.frame format
#' as.data.frame(A)

mpitb.A <- function(object, ...) UseMethod("mpitb.A", object)

#' @rdname mpitb.A
#' @export
mpitb.A.mpitb_set <- function(object, ...){
  data <- object$data
  subgroup <- object$subgroup
  score <- data$variables$score
  K <- object$K
  level <- attr(object, "level")
  output <- vector("list", length = length(K))
  for (i in 1:length(K)){
    k <- K[i]/100
    poor.mpi <- as.factor(ifelse(score >= k,1,0))
    censored.score <- censored.deprivations.score(score, k)
    data <- update.survey.design(data, score.k = censored.score, mpi.k = poor.mpi)
    by.list <- survey::svybys(survey::make.formula("score.k"), survey::make.formula(subgroup), design = subset(data, data$variables[,'mpi.k']==1), survey::svyciprop, vartype = c("se","ci"), level = level)

    A <- lapply(by.list, FUN = function(x) mpitb.measurebys(x, data))

    names(A) <- subgroup

    #A <- lapply(A, "*",100)

    attr(A, "k") <- k*100
    output[[i]] <- A
  }
  attr(output, "time") <- object$time
  class(output) <- c("mpitb_A", "mpitb_measure")
  output
}
