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
    attr(M0, "k") <- k
    output[[i]] <- M0
  }
  class(output) <- c("mpitb_M0", "mpitb_measure")
  output
}

#' Incidence (H)
#'
#' Calculate the Incidence of poverty or the proportion of multidimensionally poor people.
#'
#' @param object `mpitb_set` object
#' @param ... other arguments
#'
#' @return `mpitb_H` and `mpitb_measure` class
#'
#' @export
#' @rdname mpitb.H
#'
#' @examples
mpitb.H <- function(object, ...) UseMethod("mpitb.H", object)

#' @rdname mpitb.H
#' @export
mpitb.H.mpitb_set <- function(object, ...){
  data <- object$data
  over <- object$over
  c.score <- data$variables$c.score
  K <- object$K
  output <- vector("list", length = length(K))
  for (i in 1:length(K)){
    k <- K[i]
    poor.mpi <- ifelse(c.score >= k,1,0)
    data <- update.survey.design(data, mpi.k = poor.mpi)
    mylist <- svybys(survey::make.formula("mpi.k"), bys = survey::make.formula(over), data, survey::svymean)

    H <- lapply(mylist, FUN = function(x) mpitb.measure(x, data))

    names(H) <- over

    H <- lapply(H, "*",100)

    attr(H, "k") <- k
    output[[i]] <- H
  }
  class(output) <- c("mpitb_H", "mpitb_measure")
  output
}

#' Intensity (A)
#'
#' Calculates the Intensity of poverty or the average deprivation score of the multidimensionally poor people
#'
#' @param object `mpitb_set` object
#' @param ... other arguments
#'
#' @return `mpitb_A` and `mpitb_measure` class
#' @export
#' @rdname mpitb.A
#'
#' @examples
mpitb.A <- function(object, ...) UseMethod("mpitb.A", object)

#' @rdname mpitb.A
#' @export
mpitb.A.mpitb_set <- function(object, ...){
  data <- object$data
  over <- object$over
  c.score <- data$variables$c.score
  K <- object$K
  output <- vector("list", length = length(K))
  for (i in 1:length(K)){
    k <- K[i]
    poor.mpi <- as.factor(ifelse(c.score >= k,1,0))
    censored.c.score <- censored.deprivations.score(c.score, k)
    data <- update.survey.design(data, c.k = censored.c.score, mpi.k = poor.mpi)
    mylist <- svybys(survey::make.formula("c.k"), survey::make.formula(over), design = subset(data, data$variables[,'mpi.k']==1), survey::svymean)

    A <- lapply(mylist, FUN = function(x) mpitb.measure(x, data))

    names(A) <- over

    A <- lapply(A, "*",100)

    attr(A, "k") <- k
    output[[i]] <- A
  }
  class(output) <- c("mpitb_A", "mpitb_measure")
  output
}

#' Censored Headcount Ratios
#'
#' Calculate the censored headcount ratios for each indicator or the proportion of people who are multidimensionally poor and deprived in each indicator
#'
#' @param object `mpitb_set` object
#' @param ... other arguments
#'
#' @return `mpitb_headcounts` and `mpitb_measure` class
#' @export
#' @rdname mpitb.headcounts
#'
#' @examples
mpitb.headcounts <- function(object, ...) UseMethod("mpitb.headcounts", object)

#' @rdname mpitb.headcounts
#' @export
mpitb.headcounts.mpitb_set <- function(object, ...) {
  data <- object$data
  over <- object$over
  indicators <- object$indicators
  K <- object$K
  output <- vector("list", length = length(K))
  for (i in 1:length(K)){
    k <- K[i]
    mylist <- survey::svybys(survey::make.formula(indicators), bys = survey::make.formula(over),data,survey::svymean)

    Hj <- lapply(mylist, FUN = mpitb.measure)
    names(Hj) <- over
    attr(Hj, "k") <- k
    output[[i]] <- Hj
  }
  class(output) <- c("mpitb_headcounts", "mpitb_measure")
  output
}

mpitb.est <- function(object, ...) UseMethod("mpitb.est", object)

mpitb.est.mpitb_set <- function(object, measures = c("M0","H","A","Headcounts"), ...){
  estimation <- vector("list", length = length(measures))
  names(estimation) <- measures
  if ("M0" %in% measures) {estimation[["M0"]] <- mpitb.M0(object)}

  if ("H" %in% measures) {estimation[["H"]] <- mpitb.H(object)}

  if ("A" %in% measures) {estimation[["A"]] <- mpitb.A(object)}

  if ("Headcounts" %in% measures) {estimation[["Headcounts"]] <- mpitb.headcounts(object)}

  class(estimation) <- "mpitb_est"
  return(estimation)
}
