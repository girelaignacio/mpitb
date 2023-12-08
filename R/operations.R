
# Deprivations matrix ####
G0.matrix <- function(object, indicators) {

  G0 <- as.matrix(object$variables[, indicators])

  return(G0)
}

# Weighted Deprivations matrix ####
# /* Multiply the deprivation matrix by the weight of each indicator */

weighted.G0.matrix <- function(G0, weights) {

  G0_w <- G0 %*% diag(weights)

  return(G0_w)
}

# Deprivations score ####

#' Calculate deprivations score from the deprivation matrix
#'
#' @param x Numeric Matrix
#'
#' @return A numeric vector
#' @export
#'
#' @examples
deprivations.score <- function(x){
  # x : numeric matrix
  c <- apply(x, MARGIN = 1, FUN = sum)

  return(c)
}

# Censored deprivations score ####

censored.deprivations.score <- function(cvector, cutoff){
  c.k <- sapply(cvector, function(x) ifelse(x >= cutoff,x,0))
  return(c.k)
}

# Censored deprivations matrix ####
censored.G0.matrix <- function(){
  data <- object$data
  c.score <- data$variables$c.score
  G0.k<-data$variables[,indicators]
  for (row in 1:length(c.score)){
    if(c.score[row] <= k){
      data$variables[row,indicators] <- rep(0, length(indicators))
    }
  }
}








