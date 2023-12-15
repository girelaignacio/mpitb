#' Censored Headcount Ratios
#'
#' Calculate the censored headcount ratios for each indicator or the proportion of people who are multidimensionally poor and deprived in each indicator
#'
#' @param object `mpitb_set` object
#' @param censored logical. If TRUE, it calculates the censored headcounts ratios. It is set FALSE by default.
#' @param ... other arguments
#'
#' @return `mpitb_headcounts` and `mpitb_measure` class
#' @export
#' @rdname mpitb.headcounts
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
#' h_j <- mpitb.headcounts(set)
#' as.data.frame(h_j)
mpitb.headcounts <- function(object, ...) UseMethod("mpitb.headcounts", object)

#' @rdname mpitb.headcounts
#' @export
mpitb.headcounts.mpitb_set <- function(object, censored = FALSE, ...) {
  data <- object$data
  over <- object$over
  indicators <- object$indicators

  if (censored == FALSE) {
    #### Calculate uncensored indicators headcount ratio
    output <- vector("list", length = 1)
    mylist <- survey::svybys(survey::make.formula(indicators), bys = survey::make.formula(over),data,survey::svymean)

    Hj <- lapply(mylist, FUN = function(x) mpitb.measure(x, data))
    names(Hj) <- over
    attr(Hj, "k") <- NA
    output[[1]] <- Hj
    ####
    } else {
    #### Calculate censored indicators headcount ratio
      K <- object$K
      output <- vector("list", length = length(K))
      for (i in 1:length(K)){
        k <- K[i]
          #### Censored deprivation matrix
        cens.data <- object$data
        mpi.poor <- cens.data$variables$c.score >= k
        G0 <- G0.matrix(object$data,indicators)
        cens.G0 <- matrix(NA, nrow = nrow(G0), ncol = ncol(G0))
        colnames(cens.G0) <- indicators
        for (col in 1:length(indicators)){
          cens.G0[, col] <- ifelse(mpi.poor, G0[, col], 0)
        }
        cens.data[, indicators] <- cens.G0
          ####

        mylist <- survey::svybys(survey::make.formula(indicators), bys = survey::make.formula(over), cens.data, survey::svymean)

        Hj <- lapply(mylist, FUN = function(x) mpitb.measure(x, cens.data))
        names(Hj) <- over
        attr(Hj, "k") <- k
        output[[i]] <- Hj
     ####
      }
    }
  class(output) <- c("mpitb_headcounts", "mpitb_measure")
  output
}
