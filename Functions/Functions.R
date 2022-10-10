mlag <- function(X, lag){
  p <- lag
  X <- as.matrix(X)
  Traw <- nrow(X)
  N <- ncol(X)
  Xlag <- matrix(0, Traw, p*N)
  for (ii in 1:p) {
    Xlag[(p+1):Traw, (N*(ii-1)+1):(N*ii)]=X[(p+1-ii):(Traw-ii), (1:N)]
  }
  return(Xlag)
}
