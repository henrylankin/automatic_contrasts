library("pracma")

# n is number of treatments
contrast.basematrix <- function(n) {
  m <- matrix(nrow = n-1, ncol = n)
  
  for (i in 1:(n-1)) {
    m[i,] <- c(rep(0, i - 1), n-i,rep(-1, n-i))
  }
  
  return(m)
}

contrast.orthogonalize <- function(treatment_count, treatments_desired) {
  
  n <- treatment_count
  l <- treatments_desired
  
  numberOfContrasts <- length(l)
  
  
  numberToReduce <- n-1 - numberOfContrasts
  
  contrast.base <- contrast.basematrix(n)

  
  contrast.original <- contrast.base
  for (i in 1:numberOfContrasts) {
    contrast.original[n-i,] <- l[[i]]

  }

  
  temp <- contrast.original
  i <- 0
  while (i <= numberToReduce) {
    index <- numberToReduce - i
    
    print(temp)
    
    temp[index,] <- rep(1, n)
    toReduce <- temp
    
    print(toReduce)
    
    toReduce <- rref(toReduce)
    
    print(toReduce)
    
    temp[index,] <- c(toReduce[,n],-1)
    i <- i + 1
  }
  
  contrast.orthogonalized <- temp
 
  return(contrast.orthogonalized)
}

