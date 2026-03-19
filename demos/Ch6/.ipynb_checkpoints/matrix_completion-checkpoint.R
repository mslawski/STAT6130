set.seed(97871)
A <- matrix(nrow = 500, ncol = 10, data = runif(5000))
B <- t(matrix(nrow = 200, ncol = 10, data = runif(2000)))
X <- A %*% B

M <- sample(length(X), 10000, replace = FALSE)
X[M] <- NA

### recovery of B given A
Ahat <- matrix(runif(5000 * 10), nrow = 500, ncol = 10)
source("matrix_completion_fct.R")
res <- ALS(X, A_init = Ahat)

sum(abs(res$Xhat - A %*% B))
