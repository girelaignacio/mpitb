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
mpitb.headcounts.mpitb_set <- function(object, censored = FALSE, ...) {
  data <- object$data
  over <- object$over
  indicators <- object$indicators

  if (censored == FALSE) {
  ### Calculate uncensored indicators headcount ratio
  output <- vector("list", length = 1)
  mylist <- survey::svybys(survey::make.formula(indicators), bys = survey::make.formula(over),data,survey::svymean)

  Hj <- lapply(mylist, FUN = function(x) mpitb.measure(x, data))
  names(Hj) <- over
  attr(Hj, "k") <- NA
  output[[1]] <- Hj
  } else {
  ### Calculate censored indicators headcount ratio
  K <- object$K
  output <- vector("list", length = length(K))
  for (i in 1:length(K)){
    k <- K[i]
      # Censored deprivation matrix
    c.data <- object$data
    c.score <- object$data$variables$c.score
    #  G0.k<-data$variables[,indicators]
      for (l in 1:length(c.score)){
        if(c.score[l] <= k){
          c.data$variables[l,indicators] <- rep(0, length(indicators))
        }
      }

      mylist <- survey::svybys(survey::make.formula(indicators), bys = survey::make.formula(over),c.data,survey::svymean)

      Hj <- lapply(mylist, FUN = function(x) mpitb.measure(x, c.data))
      names(Hj) <- over
      attr(Hj, "k") <- k
      output[[i]] <- Hj
    }
  }
  class(output) <- c("mpitb_headcounts", "mpitb_measure")
  output
}
