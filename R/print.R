#' print() method for "mpitb_set" class
#'
#' @param object An object of class "mpitb_set"
#' @param ... Other arguments passed to or from other methods
#'
#' @export
print.mpitb_set <- function(object, ...) {
  cat("--- Specification --- \n",
      "Name: ", attr(object,"name"),"\n",
      "Weighting scheme: ", attr(object$weights,"scheme"),"\n",
      "Description: ", attr(object,"description"),"\n\n"
  )

  cat("--- Survey design --- \n")
  cat(dim(object$data)[1], "observations\n")
  print(object$data)

  cat("\n--- Parameters ---\n",
      "Cut-offs: ", length(object$K), "(",object$K,")\n",
      "Indicators: ",length(object$indicators),"\n")

  cat("\n--- Number of subgroups ---\n")
  for (i in object$subgroup[-1]){
    cat(i,":", length(levels(object$data$variables[,i])), "levels\n\n\n")
  }

  missings <- apply(object = object$data$variables[,object$indicators], MARGIN = 2,FUN = function(x) sum(is.na(x)))
  if (sum(missings) == 0)
  {cat("No missing values found\n")}
  else{cat( sum(missings), "missing values found\n")}

  invisible(object)
}
