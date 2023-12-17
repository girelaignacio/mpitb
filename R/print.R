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

  missings <- apply(x = x$data$variables[,x$indicators], MARGIN = 2,FUN = function(x) sum(is.na(x)))
  if (sum(missings) == 0)
  {cat("No missing values found\n")}
  else{cat( sum(missings), "missing values found\n")}

  invisible(x)
}
