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
#' set <- mpitb.set(data, indicators, cutoff, weights, over,
#'       name = "Example", desc = "SWZ MICS survey 2014")
#'
#' M0 <- mpitb.M0(set)
#' as.data.frame(M0)
#'
#' confint(M0, parm = "coefficient", level = 0.95)


confint.mpitb_measure <- function(object, parm, level, ...){

  if (!(parm == "coefficient")){stop("Only for coefficients")}

  #cutoffs <- lapply(object, FUN = function(x) rep(attr(x,"k"), sum(sapply(x, FUN = function(y) length(y)))))
  #cutoffs <- do.call("c",cutoffs)
  cutoffs <- retrieve.cutoffs(object)

  #b <- unlist(object)
  b <- retrieve.coefficients(object)

  #se <- unlist(lapply(object, FUN = function(X) unlist(sapply(X, FUN = function(x) attr(x,"se")), use.names = F)))
  se <- retrieve.se(object)

  #degfs <- unlist(lapply(object, FUN = function(X) unlist(sapply(X, FUN = function(x) attr(x,"df")), use.names = F)))
  degfs <- retrieve.df(object)
  #fac <- stats::qt(level + (1 - level)/2, df=degfs)
  #qt(p=alpha/2, df=degfs-1,lower.tail=F)
  alpha <- 1-level
  sample.se <- se/sqrt(degfs)
  #degfs <- degfs - 1
  t.score <- stats::qt(p=alpha/2, df=degfs-1,lower.tail=FALSE)
  fac <- stats::qt(alpha/2,df=degfs, lower.tail = FALSE)/sqrt(degfs)
  lb <- b - t.score * sample.se
  ub <- b + t.score * sample.se

  out <- cbind(cutoffs,lb,ub)
  colnames(out) <- c("Cut-offs",paste("Lower Bound (",level*100,"%)", sep = ""), paste("Upper Bound (",level*100,"%)", sep = ""))
  rownames(out) <-  names(b)

  return(out)
}

