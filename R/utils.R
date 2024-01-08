mpitb.measurebys <- function(X, data){
  # Retrieve coefficients ####
  x <- stats::coef(X)

  # Retrieve standard errors with survey::SE() function ####
  se <- survey::SE(X)
  # Retrieve confidence interval bounds ####
  lb <- X$ci_l
  ub <- X$ci_u
  # Catch name of subgroup
  subgroup <- names(X)[1]
  # Retrieve degrees of freedom of the survey design ####
  dfs <- sapply(rownames(X), FUN=function(x) survey::degf(subset(data,data$variables[, subgroup] == x)))
  attr(x,"subgroup") <- subgroup
  attr(x,"se") <- se
  attr(x, "lb") <- lb
  attr(x,"ub") <- ub
  attr(x, "df") <- dfs

  return(x)
}



update.survey.design <- function(object, ...) {
  dots <- substitute(list(...))[-1]
  newnames <- names(dots)

  for(j in seq(along=dots)){
    object$variables[, newnames[j]] <- eval(dots[[j]], object$variables, parent.frame())
  }
  object$call<-sys.call(-1)

  object
}

