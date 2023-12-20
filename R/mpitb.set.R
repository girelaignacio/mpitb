#' Set the specification of the project
#'
#' @param data Input data, a `survey.design` class object where a complex survey design was previously specified. Can be a matrix but it is coerced to a `survey.design` class assuming equal probabilities.
#' @param indicators A character vector containing the names of the indicators. This character vector should belong to columns names of `data`.
#' @param weights A numerical vector representing the weights of each indicator. Should have the same length as `indicators` vector. Should sum up to 1. Default sets the same weight to each indicator
#' @param K A numerical vector representing the poverty cut-offs. Should be values between 1 and 100. Default is 1, i.e., deprivations are not censored.
#' @param subgroup A character vector containing the names of the populations subgroup of interest. This character vector should belong to columns names of `data`.
#' @param year A character object with the name of the column in `data` containing information about temporal information for analysis of changes over time.
#' @param name A character object in which it can be specified a name for the project.
#' @param description A character object in which it can be specified a brief description for the project.
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
    warning("An equal weighting scheme is assumed between all indicators.")
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
  # Coerce as factor!

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
  #K <- change.K.scale(K)

  data[,"Total"] <- "Total"

  # Create the deprivations and weighted deprivations matrix ####
  # G0 <- matrix: Deprivation Matrix
  G0 <- G0.matrix(data, indicators)
  # G0_w <- matrix: Weighted Deprivation Matrix
  G0_w <- weighted.G0.matrix(G0, weights)

  # Calculate the deprivations score ####
  deprivations.score <- deprivations.score(G0_w)
  # add deprivations score to survey variables data frame
  data <- update.survey.design(data, score = deprivations.score)

  # Define names and class ####
  set <- list()
  set$data <- data

  attr(set, "name") <- name

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


