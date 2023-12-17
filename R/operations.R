# Deprivations matrix ####

#' Obtain the deprivations matrix (G0) from the survey data.
#'
#' @param data a `survey.design` object containing the complex household survey design
#' @param indicators a character vector with the names of the indicators.
#'
#' @return a numeric matrix with binary entries representing the deprivations matrix
#' @export
#'
#' @examples
#' library(mpitb)
#' data <- survey::svydesign(id=~PSU, weights = ~Weight, strata = ~Strata,
#'         data = swz_mics14)
#' indicators <- c("Water","Assets","School","Nutrition")
#'
#' deprivations.matrix <- G0.matrix(data, indicators)
G0.matrix <- function(data, indicators) {

  G0 <- as.matrix(data$variables[, indicators])

  return(G0)
}

# Weighted Deprivations matrix ####
# /* Multiply the deprivation matrix by the weight of each indicator */

weighted.G0.matrix <- function(X, weights) {

  G0.w <- X %*% diag(weights)

  return(G0.w)
}

# Deprivations score ####

#' Calculate deprivations score from the deprivations matrix
#'
#' @param X a numeric matrix containing the weighted deprivations matrix, i.e., a matrix with binary entries where 0 means no deprivations whereas 1 means that the individual suffers deprivations of that indicator.
#'
#' @return a numeric vector containing the deprivations score.
#' @export
#'
#' @examples
#' library(mpitb)
#' data <- survey::svydesign(id=~PSU, weights = ~Weight, strata = ~Strata,
#'         data = swz_mics14)
#' indicators <- c("Water","Assets","School","Nutrition")
#' weights <- c(1/6,1/6,1/3,1/3)
#'
#' score <- deprivations.score(G0.matrix(data, indicators))
deprivations.score <- function(X){
  # x : numeric matrix
  c <- apply(X, MARGIN = 1, FUN = sum)

  return(c)
}

# Censored deprivations score ####

censored.deprivations.score <- function(cvector, cutoff){
  c.k <- sapply(cvector, function(x) ifelse(x >= cutoff,x,0))
  return(c.k)
}

# Censored deprivations matrix ####
censored.G0.matrix <- function(object, ...){}








