#' as.data.frame() method for "mpitb_measure" class
#'
#' @param x An object of class "mpitb_measure"
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional logical
#' @param ... further arguments passed to or from other methods
#'
#' @export
as.data.frame.mpitb_measure <- function(x, row.names = NULL, optional = FALSE, ...) {

  # functions
  convert.to.data.frame <- function(X) {
    dataframe.k <- do.call("rbind",X)
    dataframe.k$subgroup <- sub('\\..*',"",rownames(dataframe.k))
    rownames(dataframe.k) <- NULL
    dataframe.k$k <- rep(attr(X,"k"), nrow(dataframe.k))

    if ("indicator" %in% colnames(dataframe.k)){
      order <- c("subgroup","level","k","indicator","coefficient","se","lb","ub")
    }else{order <- c("subgroup","level","k","coefficient","se","lb","ub")}

    dataframe.k <- dataframe.k[,order]

    return(dataframe.k)
  }

  list.k <- lapply(x, FUN = convert.to.data.frame)
  mpitb_measure.df <- do.call("rbind", list.k)
  attr(mpitb_measure.df,"mpitb_measure") <- sub(".*_","",class(x)[1])
  mpitb_measure.df
}
