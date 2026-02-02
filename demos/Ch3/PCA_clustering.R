### Clustering vs. PCA example
n <- 500
N <- 2*n
mu <- c(1,1)
sigma <- 0.1
# generate 500 points from each of the two (well-separated clusters)
X1 <- cbind(rnorm(n, sd = sigma) + mu[1], rnorm(n, sd = sigma) + mu[2])
X2 <- cbind(rnorm(n, sd = sigma) - mu[1], rnorm(n, sd = sigma) - mu[2])
X <- rbind(X1, X2)
plot(X[,1], X[,2])

# here the matrix (X'* X / N) is an approapriate estimator of the covariance matrix
# since the population mean can be shown to be zero
Sigmahat <- crossprod(X)/N

# the eigendecomposition shows one large eigenvalue (signal) and one small eigenvalue (noise)
eigSigmahat <- eigen(Sigmahat)
eigSigmahat$values

# projection of the data on the first eigenvector yields perfect separation of the two clusters 
v1 <- eigSigmahat$vectors[,1]
Z1 <- X %*% v1
plot(Z1)


