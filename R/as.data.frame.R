#' as.data.frame() method for "mpitb_measure" class
#'
#' @param x An object of class "mpitb_measure"
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional logical
#' @param ... further arguments passed to or from other methods
#'
#' @export
as.data.frame.mpitb_measure <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (any(class(uncensored.headcounts) == "mpitb_headcounts")){
    list.k <- lapply(x, FUN = convert.to.data.frame_columns)
  } else {list.k <- lapply(x, FUN = convert.to.data.frame_rows)}
  mpitb_measure.df <- do.call("rbind", list.k)
  mpitb_measure.df
}
