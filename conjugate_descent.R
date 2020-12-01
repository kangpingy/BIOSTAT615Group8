conjugate_descent <- function(A,b,x0,tol=1e-20,max_iter=1000){
  convergence = 1
  res <- b - A%*%x0
  d0 <- res
  x <- x0
  for(iter in 1:max_iter){
    alpha <- as.vector(crossprod(res)/(crossprod(d0,A%*%d0)))
    new_x <- x + alpha*d0
    new_r <- res - alpha*(A%*%d0)
    new_beta <- as.vector(crossprod(new_r)/crossprod(res))
    new_d <- new_r + new_beta*d0
    x <- new_x
    res <- new_r
    d0 <- new_d
    if(sum(res^2)<tol){
      convergence = 0
      break
    }
  }
  return(list(root=x,iter=iter,convergence=convergence))
}


install.packages("cPCG")
library(cPCG)

for (i in 1:5000){
  rnum <- sample(10,1)
  A[a[i],b[i]] <- rnum
  A[b[i],a[i]] <- rnum
}
b <- rnorm(1000)