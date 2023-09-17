#' Set the specification of the project
#'
#' @param data `survey.design` object
#' @param indicators Vector or a List.
#' @param K Numerical. Vector of poverty cut-offs. Can be values between 1 and 100.
#' @param weights Weights of each indicator.
#' @param over Character.
#' @param year Character.
#' @param name Character.
#' @param desc Character.
#' @param ... other arguments
#'
#' @return `mpitb_set` object
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

mpitb.set <- function(data, indicators, K, weights, over = NULL, year,
                      name = NULL, desc = NULL, ...) {

  #### check.arguments ####
  if (missing(data)) {
    stop("Error: data not found")
  }
  if (missing(indicators)) {
    stop("Error: set of dimensions and indicators not found")
  }
  # set can be a list (when dimensions and indicators are under studies or vector when only indicators are under study)
  if (missing(K)) {
    stop("Error: Poverty cut-off value (K) not found")
  }

  if (!is.null(over)) {
    if (!all(over %in% colnames(data))){
      stop("Error: groups not found in data")
    }
    over <- c("Overall", over)
  } else { over <- c("Overall") }

  # check if data is a survey.design object
  if (!inherits(data, "survey.design")) {
    cat("object `data` is not survey.design class. Coerced to survey.design class.")
    # if not a survey.design -> coerced to survey.design assuming equal probability
    data <- as.data.frame(data)
    data <- survey::svydesign(id=~rownames(data), data = data)
  }
  # check if poverty cut-offs is a numeric vector
  if (!inherits(K,"numeric")){
    stop("Poverty cut-offs (K) are not numeric values")
  }

  # check if poverty cut-offs are between 1 and 100
  if (any(K < 1) | any(K > 100)) {
    stop("Poverty cut-offs (K) are out of range. \n K must be between 1 and 100")
  }

    #.transformation of arguments and variables ####
  K <- change.K.scale(K)

  # check if year is in data columns
  if (!missing(year)){
    # if character <- check if column
    if (!(year %in% colnames(data)))
    {stop("Error: year column not found in data")}else{
        year <- unique(data$variables[,year])
      }
  }


  data[,"Overall"] <- "Overall"

  # Create the deprivations and weighted deprivations matrix ####
  # G0 <- matrix: Deprivation Matrix
  G0 <- G0.matrix(data, indicators)
  # G0_w <- matrix: Weighted Deprivation Matrix
  G0_w <- G0_w.matrix(G0, weights)

  # Calculate the deprivations score ####
  # cens.score <- numeric: Deprivations score
  cens.score <- deprivations.score(G0_w)
  # add deprivations score to survey variables data frame
  data <- update.survey.design(data, c.score = cens.score)

  # Define names and class ####
  set <- list()
  set$data <- data
  # check if name is character
  if (!is.null(name)){
    if(!is.character(name)){
      name <- "Unnnamed project"
      attr(set, "name") <- name
    } else {attr(set, "name") <- name}
  } else {
    name <- "Unnamed project"
    attr(set, "name") <- name
  }

  # check if description is a character
  if (!is.null(desc)){
    if(!is.character(desc)){
      desc <- "Preferred specification"
      attr(set, "desc") <- desc
    } else {attr(set, "desc") <- desc}
  } else {
    desc <- "Preferred specification"
    attr(set, "desc") <- desc
  }

  if (!missing(year)){set$year <- unique(year)}

  set$indicators <- indicators
  set$K <- K

  set$weights <- weights
  attr(set$weights, "scheme") <- "equal"

  set$over <- over

  class(set) <- "mpitb_set"

  return(set)
}

#' print() method for "mpitb_set" class
#'
#' @param x An object of class "mpitb_set"
#' @param ... Other arguments passed to or from other methods
#'
#' @export
print.mpitb_set <- function(x, ...) {
  cat("--- Specification --- \n",
      "Name: ", attr(x,"name"),"\n",
      "Weighting scheme: ", attr(x$weights,"scheme"),"\n",
      "Description: ", attr(x,"desc"),"\n\n"
  )

  cat("--- Survey design --- \n")
  cat(dim(x$data)[1], "observations\n")
  print(x$data)

  cat("\n--- Parameters ---\n",
      "Cut-offs: ", length(x$K), "(",x$K,")\n",
      "Indicators: ",length(x$indicators),"\n")

  cat("\n--- Number of subgroups ---\n")
  for (i in x$over[-1]){
    cat(i,":", length(levels(x$data$variables[,i])), "levels\n\n\n")
  }

  missings <- apply(X = x$data$variables[,x$indicators],MARGIN = 2,FUN = function(x) sum(is.na(x)))
  if (sum(missings) == 0)
  {cat("No missing values found\n")}
  else{cat( sum(missings), "missing values found\n")}

  invisible(x)
}
