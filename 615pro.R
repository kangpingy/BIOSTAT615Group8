rm(list=ls())
AXB=C
n <- 2
p*n n*m m*q p*q
ncol_A <- n
nrow_A <- p
ncol_X <- m
nrow_X <- n
ncol_B <- q
nrow_B <- m
ncol_C <- q
nrow_C <- p
list_A <- list()
list_X <- list()
list_B <- list()
zero <- matrix(0,p,q)
for (i in 1:(ncol(A)/n)) {
  list_A[i] <- list(A[,((i-1)*n+1):(n*i)])
  list_B[i] <- list(B[,((i-1)*q+1):(q*i)])
  list_X[i] <- list(X[,((i-1)*m+1):(m*i)])
}
for (j in 1:length(list_A)) {
  SumAXB <- SumAXB + list_A[[j]]%*%list_X[[j]]%*%list_B[[j]]
}
Rk <- C-SumAXB
P[j] <- list(t(list_A[[j]])%*%Rk%*%t(list_B[[j]]))
Sum_P <- 0
for (j in 1:length(P)) {
  Sum_P <- Sum_P + sum(P[[j]]^2)
}
while (sum(Rk-zero)<tol) {
  Rk <- C-SumAXB
  for (j in 1:length(list_X)) {
    list_X[[j]] <- list_X[[j]] + (sum(Rk^2)/Sum_P)*P[j]
    SumAPB <- SumAPB + list_A[[j]]%*%P[[j]]%*%list_B[[j]]
  }
  Rk1 <- Rk
  Rk <- Rk-(sum(R^2)/Sum_P)*SumAPB
  P[j] <- list(t(list_A[[j]])%*%Rk%*%t(list_B[[j]]) + (Rk/Rk1)*P[[j]])
}




cov


#test#
list_A <- list(B,B,B)
list_B <- list(B,B,B)
list_X <- list(B,B,B)

A <- matrix(c(rep(1,40)),ncol=8)
B <- matrix(c(1,2,3,4),ncol=2)
