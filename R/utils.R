#' eSwatini MICS 2014 merged
#'
#' @name swz_mics14
#' @docType data
#' @author OPHI Summer School 2022
#' @references \url{https://mics.unicef.org/surveys}
"swz_mics14"

mpitb.measure <- function(X, data){
  b <- stats::coef(X)
  se <- suppressWarnings(sqrt(diag(stats::vcov(X))))
  over <- names(X)[1]
  dfs <- sapply(rownames(X), FUN=function(x) survey::degf(subset(data,data$variables[, over] == x)))
  #fac <- qt(level + (1 - level)/2, df=dfs) #¿qt(0.975,df=n-1)*se/sqrt(n)?
  #lb <- b - fac * se
  #ub <- b + fac * se
  #b <- list(b*100)
  #b <- cbind(b,se,lb,ub)
  attr(b,"over") <- over
  attr(b,"se") <- se
  attr(b, "df") <- dfs

  return(b)
}
as.data.frame.mpitb_est <- function(x, row.names = NULL, optional = FALSE, ...) {
  list.measures <- lapply(x, FUN = as.data.frame)
  # add column with names of the measure
  mpitb_est.df <- do.call("rbind", list.measures)
  mpitb_est.df
}

convert2data.frame <- function(X) {
  len.levels <- sapply(X, FUN = function(x) length(x))
  over.names <- names(sapply(X, FUN = function(x) length(x)))
  col.k <- rep(attr(X,"k"), sum(sapply(X, FUN = function(x) length(x))))
  col.over <- rep(over.names, len.levels)
  col.levels <- unlist(sapply(X, FUN = function(x) names(x)), use.names = F)
  df.k <- cbind.data.frame(col.over, col.levels, col.k)
  col.coeff <- matrix(unlist(X) , nrow = length(col.over))
  col.se <- unlist(sapply(X, FUN = function(x) attr(x,"se")), use.names = F)
  df.k <- cbind(df.k,col.coeff, col.se)
  colnames(df.k) <- c("Over","Level","Cut-off","Coefficient","Standard Error")
  return(df.k)
}



change.K.scale <- function(x) {
  x <- x/100
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

svybys<-function(formula,  bys,  design, FUN, ...){
  tms <- attr(stats::terms(bys),"variables")[-1]

  lapply(tms, function(tm){
    eval(bquote(survey::svyby(.(formula),by=~.(tm),
                      design=.(design), FUN=.(FUN), ...)))
  })
}
