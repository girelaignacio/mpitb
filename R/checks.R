#### CHECKS FUNCTIONS

.check.arguments <- function(object, set, K, weights) {
  if (missing(object)) {
    stop("Error: data not found")
  }

  if (missing(set)) {
    stop("Error: set of dimensions and indicators not found")
  }

  if (missing(K)) {
    stop("Error: Poverty cut-off value (K) not found")
  }
}

.check.survey.design <- function(object) {
  # check if data is a survey.design object
  if (!inherits(object,"survey.design"))
    stop("Data is not a survey design") # if not a survey.design -> convert to survey.dsign assuming equal probability
}

.check.K.values <- function(x) {

  # check if poverty cut-offs is a numeric vector
  if (!inherits(x,"numeric")){
    stop("Poverty cut-offs (K) are not numeric values")
  }

  # check if poverty cut-offs are between 1 and 100
  if (any(x < 1) | any(x > 100)) {
    stop("Poverty cut-offs (K) are out of range. \n K must be between 1 and 100")
  }

}

.check.subgroups <- function(x) {
  if (!missing(x)) {
    subgroups <- c("national",x)
    #check that subgroups are in data columns name
  }
  else {
    subgroups <- c("national")
  }
  subgroups
}
