#' Set the specification of the project
#'
#' @param data `survey.design` object
#' @param indicators Vector or a List.
#' @param weights Weights of each indicator.
#' @param K Numerical. Vector of poverty cut-offs. Can be values between 1 and 100.
#' @param subgroup Character.
#' @param year Character.
#' @param name Character.
#' @param description Character.
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
#' subgroup <- c("Region","Area")
#'
#' set <- mpitb.set(data, indicators, weights, cutoff, subgroup,
#'       name = "Example", description = "SWZ MICS survey 2014")

mpitb.set <- function(data, indicators, weights, K = 1, subgroup = NULL, year = NULL,
                      name = "Unnamed project", description = "No description", ...) {
  this.call <- match.call()
  # Print this call so that the user can check if arguments are correctly assigned
  print(this.call)

  #### Check arguments ####

    ### `data` argument
      ## check if `data` missing argument
  if (missing(data)) {stop("Error: `data` not found")}
      ## check if `data` has non-NULL dim()
  nd <- dim(data)
  if (is.null(nd)) {stop("`data` should have non-NULL dimension")}
      ## check if `data` is a `survey.design` class object. If not, it is coerced to a `survey.design` class assuming simple random sampling
  if (!inherits(data, "survey.design")) {
    warning("`data` is not `survey.design` class. Coerced to survey.design class.")
    data <- as.data.frame(data)
    data <- survey::svydesign(id=~rownames(data), data = data)
    }

    ### `indicators` argument
      ## check if `indicators` missing argument
  if (missing(indicators)) {stop("Error: `indicators` not found")}
      ## check if `indicators` are `character` class
  stopifnot("`indicators` should be a `character`" = is.character(indicators))
      ## check if `indicators` are in `data` colnames()
  stopifnot("At least one indicator is not found in `data`" = indicators %in% colnames(data))

    ### `weights` argument
      ## check if `weights` missing argument
  if (missing(weights)) {
    warning("`weights` not found. An equal weighting scheme is assumed between all indicators.")
    weights <- rep(1/length(indicators), length(indicators))
    }
      ## check if `weights` is numeric
  stopifnot("`weights` should be a `numeric`" = is.numeric(weights))
      ## check if `weights` sum up to 1
  stopifnot("`weights` must sum up to 1" = sum(weights) == 1)
      ## check if `weights` has the same length as `indicators`
  stopifnot("`weights` and `indicators` do not have the same length" = length(weights) == length(indicators))

    ### `K` argument (poverty cut-offs)
      ## check if `K` is numeric
  stopifnot("`K` should be a `numeric`" = is.numeric(K))
      ## check if `K` is between 1 and 100
  stopifnot("`K` out of range. Values greater than 100 found" = K <= 100)
  stopifnot("`K` out of range. Values lower than 1 found" = K >= 1)
  if (length(K) > 3) {warning("There are more than 3 poverty cut-off. This can slow down further calculations.")}

    ### `subgroup` argument
  if (!is.null(subgroup)) {
      ## check if `subgroup` is `character`
    stopifnot("`subgroup` should be a `character`" = is.character(subgroup))
      ## check if `indicators` are in `data` colnames()
    stopifnot("At least one subgroup not found in `data`" = subgroup %in% colnames(data))
    subgroup <- c("Total", subgroup)
    # The total observations in the data are interpreted as a subgroup
    } else {subgroup <- c("Total")}

    ### `year` argument
  if (!is.null(year)) {
    ## check if `year` is `character`
    stopifnot("`year` should be a `character`" = is.character(year))
    ## check if `year` is of length 1
    stopifnot("`year` should be one element (the column of out data that contains information about the year)" = length(year) == 1)
    ## check if `year` is in `data` colnames()
    stopifnot("At least one subgroup not found in `data`" = year %in% colnames(data))
  }

    ### `name` argument
      ## check if `year` is `character`
  stopifnot("`name` should be a `character`" = is.character(name))

    ### `description` argument
      ## check if `description` is `character`
  stopifnot("`description` should be a `character`" = is.character(description))


    #.transformation of arguments and variables ####
  K <- change.K.scale(K)

  data[,"Total"] <- "Total"

  # Create the deprivations and weighted deprivations matrix ####
  # G0 <- matrix: Deprivation Matrix
  G0 <- G0.matrix(data, indicators)
  # G0_w <- matrix: Weighted Deprivation Matrix
  G0_w <- weighted.G0.matrix(G0, weights)

  # Calculate the deprivations score ####
  # cens.score <- numeric: Deprivations score
  cens.score <- deprivations.score(G0_w)
  # add deprivations score to survey variables data frame
  data <- update.survey.design(data, c.score = cens.score)

  # Define names and class ####
  set <- list()
  set$data <- data
  # check if name is character
  attr(set, "name") <- name

  # check if description is a character
  attr(set, "description") <- description

  set$year <- unique(year)

  set$indicators <- indicators
  set$K <- K

  set$weights <- weights
  attr(set$weights, "scheme") <- "equal"

  set$subgroup <- subgroup

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
      "Description: ", attr(x,"description"),"\n\n"
  )

  cat("--- Survey design --- \n")
  cat(dim(x$data)[1], "observations\n")
  print(x$data)

  cat("\n--- Parameters ---\n",
      "Cut-offs: ", length(x$K), "(",x$K,")\n",
      "Indicators: ",length(x$indicators),"\n")

  cat("\n--- Number of subgroups ---\n")
  for (i in x$subgroup[-1]){
    cat(i,":", length(levels(x$data$variables[,i])), "levels\n\n\n")
  }

  missings <- apply(X = x$data$variables[,x$indicators],MARGIN = 2,FUN = function(x) sum(is.na(x)))
  if (sum(missings) == 0)
  {cat("No missing values found\n")}
  else{cat( sum(missings), "missing values found\n")}

  invisible(x)
}
