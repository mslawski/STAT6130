# load function that implements alternating least squares for 
# low-rank matrix completion
source("matrix_completion_fct.R")

set.seed(97871)
# generate ground truth
A <- matrix(nrow = 500, ncol = 10, data = runif(5000))
B <- t(matrix(nrow = 200, ncol = 10, data = runif(2000)))
X <- A %*% B
# generate 20% missing entries, uniformly at random
M <- sample(length(X), 10000, replace = FALSE)
X[M] <- NA

### recovery of B given A, with A randomly initialized 
Ahat <- matrix(runif(500 * 10), nrow = 500, ncol = 10)
res <- ALS(X, A_init = Ahat)

# compare solution (Xhat) to ground gruth
sum(abs(res$Xhat - A %*% B))


