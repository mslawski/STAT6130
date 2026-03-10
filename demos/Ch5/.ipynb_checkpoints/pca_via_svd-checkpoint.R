ls()
data(iris)
X <- as.matrix(iris[,c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
y <- iris[,"Species"]

pc <- princomp(X)

svdX <- svd(scale(X, scale = FALSE))

# check loadings -- note that they may differ up to a sign, hence the min(...)
mean(min( abs(svdX$v[,1] - pc$loadings[,1]), abs(svdX$v[,1] + pc$loadings[,1])))
mean(min( abs(svdX$v[,2] - pc$loadings[,2]), abs(svdX$v[,2] + pc$loadings[,2])))
mean(min( abs(svdX$v[,3] - pc$loadings[,3]), abs(svdX$v[,3] + pc$loadings[,3])))
mean(min( abs(svdX$v[,4] - pc$loadings[,4]), abs(svdX$v[,4] + pc$loadings[,4])))

# check principal components -- see comment above
Z <- sweep(svdX$u, MARGIN = 2, STATS = svdX$d, FUN = "*")
#Z2 <- svdX$u %*% diag(svdX$d)
#all.equal(Z, Z2)
mean(min( abs(pc$scores[,1] - Z[,1]),  abs(pc$scores[,1] + Z[,1])))
mean(min( abs(pc$scores[,2] - Z[,2]),  abs(pc$scores[,2] + Z[,2])))
mean(min( abs(pc$scores[,3] - Z[,3]),  abs(pc$scores[,3] + Z[,3])))
mean(min( abs(pc$scores[,4] - Z[,4]),  abs(pc$scores[,4] + Z[,4])))
