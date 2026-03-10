###
ls()
testscores <- read.table("../data/testscores.txt", header = TRUE)

C <- cor(testscores)
image(C)

p <- 5
q <- 3
X <- testscores[,paste("x", 1:p, sep = "")]
Y <- testscores[,paste("y", 1:q, sep = "")]

Sigma_X <- cov(X)
Sigma_XY  <- cov(X, Y)
Sigma_Y <- cov(Y)

eig_SigmaX <- eigen(Sigma_X)
eig_SigmaY <- eigen(Sigma_Y)

Sigma_X_invhalf <- eig_SigmaX$vectors %*% diag(1/sqrt(eig_SigmaX$values)) %*% t(eig_SigmaX$vectors)
Sigma_Y_invhalf <- eig_SigmaY$vectors %*% diag(1/sqrt(eig_SigmaY$values)) %*% t(eig_SigmaY$vectors)

M <- Sigma_X_invhalf %*% Sigma_XY %*% Sigma_Y_invhalf
svdM <- svd(M)
Utilde <- svdM$u
Vtilde <- svdM$v

# canonical directions
U <- Sigma_X_invhalf %*% Utilde
V <- Sigma_Y_invhalf %*% Vtilde

# canonical variates
Z_X <- as.matrix(X) %*% U
Z_Y <- as.matrix(Y) %*% V

zapsmall(cov(Z_X)) # identity
zapsmall(cov(Z_Y)) # identity
zapsmall(cov(Z_X, Z_Y))

# scatterplot of the first pair of canonical variates
pdf("../figs/cca.pdf")
plot(Z_X[,1], Z_Y[,1], pch = "*", cex = 2,
     xlab = "first canonical variate (X)",
     ylab = "first canonical variate (Y)", cex.lab = 1.5,
     cex.axis = 1.3)
dev.off()

# getting the results directly without any matrix algebra:
cca_R <- cancor(X, Y)

# check for equality 1: canonical correlation coefficients
all.equal(cca_R$cor, diag(cov(Z_X, Z_Y)))

n <- nrow(X)
#plot(as.matrix(X) %*% cca_R$xcoef[,1:3] * (-sqrt(n-1)),
#          as.matrix(X) %*% U)
#abline(0,1)

# own computations and R output are equal up to a sign and a scaling factor
                                        # sqrt(n-1)... took a while to figure this out since the R documentation does not make this clear.
all.equal(as.matrix(X) %*% cca_R$xcoef[,1:3] * (-sqrt(n-1)), as.matrix(X) %*% U)
all.equal(as.matrix(Y) %*% cca_R$ycoef[,1:3] * (-sqrt(n-1)), as.matrix(Y) %*% V)


# confirms the canonical correlation coefficients
cor(as.matrix(X) %*% cca_R$xcoef[,1:3],
    as.matrix(Y) %*% cca_R$ycoef)
