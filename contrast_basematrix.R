# n is number of treatments
contrast.basematrix <- function(n) {
  m <- matrix(nrow = n-1, ncol = n)
  
  for (i in 1:(n-1)) {
    m[i,] <- c(rep(0, i - 1), n-i,rep(-1, n-i))
  }
  
  return(m)
}


