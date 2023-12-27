#' Extract the confident intervals from the calculated measures
#'
#' @param object an object of class `mpitb_measure`.
#' @param parm "coefficient"
#' @param level numeric
#' @param ... other arguments
#'
#' @export
#'
#' @examples
#'
#' library(mpitb)
#' data <- survey::svydesign(id=~PSU, weights = ~Weight, strata = ~Strata,
#'         data = swz_mics14)
#' indicators <- c("Water","Assets","School","Nutrition")
#' weights <- c(1/6,1/6,1/3,1/3)
#' cutoff <- c(25,50)
#' over <- c("Region","Area")
#'
#' set <- mpitb.set(data, indicators, weights, cutoff, over,
#'       name = "Example", desc = "SWZ MICS survey 2014")
#'
#' M0 <- mpitb.M0(set)
#' as.data.frame(M0)
#'
#' confint(M0, parm = "coefficient", level = 0.95)


confint.mpitb_measure <- function(object, parm, level, ...){

  stopifnot("`confint` method only available for `parm = 'coefficient'`" = parm == "coefficient")

  cutoffs <- retrieve.cutoffs(object)

  b <- retrieve.coefficients(object)

  # Confidence intervals assumming t-student
  #se <- retrieve.se(object)
  #
  #degfs <- retrieve.df(object)
  #alpha <- 1-level
  #t.score <- stats::qt(p=alpha/2, df=degfs-1,lower.tail=FALSE)
  #lb <- b - t.score * se
  #ub <- b + t.score * se

  # p-value <- pt(b[1]/se[1], degfs[1]-1, lower.tail = FALSE)*2

  lb <- unlist(lapply(object, FUN = function(X) unlist(sapply(X, FUN = function(x) attr(x,"lb")), use.names = F)))
  ub <- unlist(lapply(object, FUN = function(X) unlist(sapply(X, FUN = function(x) attr(x,"ub")), use.names = F)))


  out <- cbind(cutoffs,lb,ub)
  colnames(out) <- c("Cut-offs",paste("Lower Bound (",level*100,"%)", sep = ""), paste("Upper Bound (",level*100,"%)", sep = ""))
  rownames(out) <-  names(b)

  return(out)
}

