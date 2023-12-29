#' Censored Headcount Ratios
#'
#' Calculate the censored headcount ratios for each indicator or the proportion of people who are multidimensional poor and deprived in each indicator.
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

mpitb.headcounts <- function(object, ...) UseMethod("mpitb.headcounts", object)

#' @rdname mpitb.headcounts
#' @export
mpitb.headcounts.mpitb_set <- function(object, censored = FALSE, ...) {
  data <- object$data
  subgroup <- object$subgroup
  indicators <- object$indicators
  level <- attr(object, "level")
  if (censored == FALSE) {
    #### Calculate uncensored indicators headcount ratio
    output <- vector("list", length = 1)

    by.list <- survey::svybys(survey::make.formula(indicators), bys = survey::make.formula(subgroup),data, survey::svyciprop, vartype = c("se","ci"), level = level)

    Hj <- lapply(by.list, FUN = function(x) mpitb.measurebys(x, data))
    names(Hj) <- subgroup
    attr(Hj, "k") <- NA
    output[[1]] <- Hj
    ####
    } else {
    #### Calculate censored indicators headcount ratio
      K <- object$K
      output <- vector("list", length = length(K))
      for (i in 1:length(K)){
        k <- K[i]/100
          #### Censored deprivation matrix
        cens.data <- object$data
        mpi.poor <- cens.data$variables$score >= k
        G0 <- G0.matrix(object$data,indicators)
        cens.G0 <- matrix(NA, nrow = nrow(G0), ncol = ncol(G0))
        colnames(cens.G0) <- indicators
        for (col in 1:length(indicators)){
          cens.G0[, col] <- ifelse(mpi.poor, G0[, col], 0)
        }
        cens.data[, indicators] <- cens.G0
          ####

        by.list <- survey::svybys(survey::make.formula(indicators), bys = survey::make.formula(subgroup), cens.data, survey::svyciprop, vartype = c("se","ci"), level = level)

        Hj <- lapply(by.list, FUN = function(x) mpitb.measurebys(x, cens.data))
        names(Hj) <- subgroup
        attr(Hj, "k") <- k*100
        output[[i]] <- Hj
     ####
      }
    }
  class(output) <- c("mpitb_headcounts", "mpitb_measure")
  output
}
