mpitb.measure <- function(X, data){
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

as.data.frame.mpitb_est <- function(x, row.names = NULL, optional = FALSE, ...) {
  list.measures <- lapply(x, FUN = as.data.frame)
  # add column with names of the measure
  mpitb_est.df <- do.call("rbind", list.measures)
  mpitb_est.df
}

convert.to.data.frame_columns <- function(X) {
  # Create subgroups columns ####
  len.levels <- sapply(X, FUN = function(x) length(x))
  subg.names <- names(sapply(X, FUN = function(x) length(x)))
  col.subg <- rep(subg.names, len.levels)
  # Create cut-offs columns ####
  col.k <- rep(attr(X,"k"), sum(sapply(X, FUN = function(x) length(x))))
  # Create levels columns ####
  col.levels.global <- unlist(sapply(X, FUN = function(x) names(x)), use.names = F)
  col.levels <- sub('\\:.*',"",col.levels.global)
  col.indicators <- sub('.*:','',col.levels.global)
  dataframe.k <- cbind.data.frame(col.subg, col.levels, col.indicators ,col.k)
  col.coeff <- matrix(unlist(X) , nrow = length(col.subg))
  col.se <- unlist(sapply(X, FUN = function(x) attr(x,"se")), use.names = F)
  # Create levels with confidence intervals
  col.lb <- unlist(sapply(X, FUN = function(x) attr(x,"lb")), use.names = F)
  col.ub <- unlist(sapply(X, FUN = function(x) attr(x,"ub")), use.names = F)
  dataframe.k <- cbind(dataframe.k,col.coeff, col.se, col.lb, col.ub)
  colnames(dataframe.k) <- c("Subgroup","Level","Indicator","Cut-off","Coefficient","Standard Error","Lower Bound", "Upper Bound")
  return(dataframe.k)
}

convert.to.data.frame_rows <- function(X) {
  # Create subgroups columns ####
  len.levels <- sapply(X, FUN = function(x) length(x))
  subg.names <- names(sapply(X, FUN = function(x) length(x)))
  col.subg <- rep(subg.names, len.levels)
  # Create cut-offs columns ####
  col.k <- rep(attr(X,"k"), sum(sapply(X, FUN = function(x) length(x))))
  # Create levels columns ####
  col.levels <- unlist(sapply(X, FUN = function(x) names(x)), use.names = F)
    # Join
  dataframe.k <- cbind.data.frame(col.subg, col.levels ,col.k)
  # Create levels with coefficients
  col.coeff <- matrix(unlist(X) , nrow = length(col.subg))
  # Create levels with standard errors
  col.se <- unlist(sapply(X, FUN = function(x) attr(x,"se")), use.names = F)
  # Create levels with confidence intervals
  col.lb <- unlist(sapply(X, FUN = function(x) attr(x,"lb")), use.names = F)
  col.ub <- unlist(sapply(X, FUN = function(x) attr(x,"ub")), use.names = F)
  dataframe.k <- cbind(dataframe.k,col.coeff, col.se, col.lb, col.ub)
  colnames(dataframe.k) <- c("Subgroup","Level","Cut-off","Coefficient","Standard Error","Lower Bound", "Upper Bound")
  return(dataframe.k)
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

svybys2<-function(formula,  bys,  design, FUN, ...){
  tms <- attr(stats::terms(bys),"variables")[-1]

  lapply(tms, function(tm){
    eval(bquote(survey::svyby(.(formula),by=~.(tm),
                      design=.(design), FUN=.(FUN), ...)))
  })
}
