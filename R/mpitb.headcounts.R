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
#' library(mpitb)
#' data <- survey::svydesign(id=~PSU, weights = ~Weight, strata = ~Strata,
#'         data = swz_mics14)
#' indicators <- c("Water","Assets","School","Nutrition")
#' weights <- c(1/6,1/6,1/3,1/3)
#' cutoff <- c(25,50)
#' subgroup <- c("Region","Area")
#'
#' set <- mpitb.set(data, indicators, weights, cutoff, subgroup,
#'       name = "Example", desc = "SWZ MICS survey 2014")
#'
#' headcounts.censored <- mpitb.headcounts(set, censored = TRUE)
#'
#' ## to observe the results in a data.frame format
#' as.data.frame(headcounts.censored)

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

    Hj <- NULL
    for (l in 1:length(indicators)) {
      print(l)
      indicator <- indicators[l]
      print(indicator)
      hj <- survey::svybys(survey::make.formula(indicators[l]), bys = survey::make.formula(subgroup), data, survey::svyciprop, vartype = c("se","ci"), level = level)
      hj <- lapply(hj, reduce.svyby, indicator)
      if (is.null(Hj)){Hj <- hj}else{Hj <- mapply(rbind,Hj, hj, SIMPLIFY = FALSE)}
    }

    #Hj <- lapply(by.list, FUN = function(x) mpitb.measurebys(x, data))
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
        cens.G0 <- as.data.frame(matrix(NA, nrow = nrow(G0), ncol = ncol(G0)))
        colnames(cens.G0) <- indicators
        for (col in 1:length(indicators)){
          cens.G0[, col] <- ifelse(mpi.poor, G0[, col], 0)
        }
        cens.data[, indicators] <- cens.G0
          ####

        Hj <- NULL
        for (l in 1:length(indicators)) {
          print(l)
          indicator <- indicators[l]
          print(indicator)
          hj <- survey::svybys(survey::make.formula(indicators[l]), bys = survey::make.formula(subgroup), cens.data, survey::svyciprop, vartype = c("se","ci"), level = level)
          hj <- lapply(hj, reduce.svyby, indicator)
          if (is.null(Hj)){Hj <- hj}else{Hj <- mapply(rbind,Hj, hj, SIMPLIFY = FALSE)}
        }

        #by.list <- survey::svybys(survey::make.formula(indicators), bys = survey::make.formula(subgroup), cens.data, survey::svyciprop, vartype = c("se","ci"), level = level)

        #Hj <- lapply(by.list, FUN = function(x) mpitb.measurebys(x, cens.data))
        names(Hj) <- subgroup
        attr(Hj, "k") <- k*100
        output[[i]] <- Hj
     ####
      }
    }

  if(censored == FALSE){
    class(output) <- c("mpitb_headcounts", "mpitb_hd", "mpitb_measure")
  }else{
    class(output) <- c("mpitb_headcounts", "mpitb_hdk", "mpitb_measure")
  }
  output
}
