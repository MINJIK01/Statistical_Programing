###########################################################
# Question 6

M <- matrix(c(2,-2,4,6,3,5,-2,17,4,4,-1,19), c(3,4), byrow = TRUE)
G_E = function(M){
  # make diagonal as 1
  for(i in 1:2){
    for(j in 2:3){
      if (abs(M[i,i]) < abs(M[j,i])){
        swap <- M[i,]
        M[i,] <- M[j,]
        M[j,] <- swap
      }
    }
    
    for (k in 2:3){
      if (k != i){
        m <- M[k,i] / M[i,i]
        M[k,] <- M[k,] - m * M[i,]
      }
    }
    
    M[i,] <- M[i,]/ M[i,i]
  }

  # back propagation
  for (h in 3:2){
    for (z in 1:2){
      if (h != z){
        M[z,] <- M[z,] - M[h,] * (M[z,h])
      }
    }
  }
  
  return(M[,4])
}

G_E(M)
#######################################################################
# Question 7

M <- matrix(c(2,5,-2,6, 2, -2, 4, 6, 3, 4, -1, 8), c(3,4), byrow = TRUE)
G_E_n = function(M){
  
  n <- nrow(M)
  p <- ncol(M) - 1
  for(i in 1:(n-1)){
    for(j in (n-1):p){
      if (abs(M[i,i]) < abs(M[j,i])){
        swap <- M[i,]
        M[i,] <- M[j,]
        M[j,] <- swap
      }
    }
    
    for (k in (n-1):p){
      if (k != i){
        m <- M[k,i] / M[i,i]
        M[k,] <- M[k,] - m * M[i,]
      }
    }
    
    M[i,] <- M[i,]/ M[i,i]
  }
  
  for (h in p:(n-1)){
    for (z in 1:(n-1)){
      if (h != z){
        M[z,] <- M[z,] - M[h,] * (M[z,h])
      }
    }
  }
  
  return(M[,p+1])
}

G_E_n(M)

#######################################################################
# Question 8

A <- matrix(c(20,1,2,3,20,-1,2,-3,20), c(3,3), byrow = TRUE)
b = c(17,-18,25)
G_S <- function(A, b, eps = 10^(-5), max_iter = 1000) {
  n <- length(b)
  x0 <- rep(0,n)
  x <- rep(0,n)
  
  for (k in 1:max_iter) {
    for (i in 1:n) {
      if (i > 1 & i < n){
        x[i] <- (b[i] - sum(A[i, 1:(i-1)] * x0[(i-1):n]) - sum(A[i,(i+1):n] * x0[(i+1):n])) / A[i, i]
      } else if(i == 1){
        x[i] <- (b[i] - sum(A[i,(i+1):n] * x0[(i+1):n])) / A[i, i]
      } else{
        x[i] <- (b[i] - sum(A[i, 1:(i-1)] * x0[(i-1):n])) / A[i, i]
      }
    }
    
    if (max(abs(x - x0)) < eps) {
      return(x)
    }
    
    x0 <- x # update
  }
}

G_S(A, b)
#######################################################################
# Question 9
A <- matrix(c(-1,3,1,5), c(2,2), byrow = TRUE)

magnitude <- function(v){
  result = sqrt(v %*% v)
  return(as.vector(result))
}

projection <- function(a,b){
  a <- as.vector(a)
  b <- as.vector(b)
  result <- as.vector((a %*% b / as.vector(a %*% a)) %*% a)
  return(result)
}

QR = function(A){
  n <- nrow(A)
  U <- matrix(rep(0,n^2), c(n,n))
  Q <- matrix(rep(0,n^2), c(n,n))

  for (i in 1:n){
    if (i ==1){
      U[,i] <- A[,i]
    } else{
      U[,i] <- A[,i]
      for (j in 1:(i-1)){
        U[,i] <- U[,i] - projection(U[,j], A[,i])
      }
    }
    Q[,i] <- U[,i] / magnitude(U[,i])
  }
  R = solve(Q) %*% A
  
  return(list("Q" = Q, "R" = R))
}

QR(A)

#######################################################################
