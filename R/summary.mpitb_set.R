summary.mpitb_set <- function(object, ...) {
  cat("--- Specification --- \n")
  cat("Name: ", attr(object,"name"),"\n",
      "Weighting scheme: ", attr(object$weights,"scheme"),"\n",
      "Description: ", attr(object,"desc"),"\n\n"
      )
  cat("--------------------- \n")

  cat("--------------------- \n")

  cat("--- Survey design --- \n")
  print(object, varnames = T ,design.summaries = T)

  cat("\n--- Parameters ---\n",
      "Cut-offs: ", length(object$K), "(",paste(object$K, collapse = ", "),")\n",
      "Indicators: ",length(object$indicators), "(", paste(object$indicators, collapse = ", "),")\n",
      "Weights: ", object$weights)

  cat("\n--- Number of subgroups ---\n")
  for (i in object$over[-1]){
    cat(i,":", length(levels(object$data$variables[,i])), "levels\n\n\n")
  }

  cat("\n--- Missing observations ---\n")
  missings <- apply(X = object$data$variables[,object$indicators],MARGIN = 2, FUN = function(x) sum(is.na(x)))
  if (sum(missings) == 0)
  {cat("No missing values found\n")}
  else{cat( sum(missings), "missing values found\n")}

  #invisible(x)
}
