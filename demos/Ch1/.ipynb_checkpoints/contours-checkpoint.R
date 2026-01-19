###
library(mvtnorm) # multivariate normal random number generation

mu1 <- c(0,0)
Sigma1 <- matrix(nr = 2, nc = 2, data = c(1, 0, 0, 1), byrow = TRUE)
Omega1 <- solve(Sigma1)

mu2 <- c(3,3)
Sigma2 <- matrix(nr = 2, nc = 2, data = c(1, 0, 0, 3), byrow = TRUE)
Sigma2r <- sqrt(Sigma2)
Omega2 <- solve(Sigma2)

mu3 <- c(-3,3)
Sigma3 <- matrix(nr = 2, nc = 2, data = c(3, 0, 0, 1), byrow = TRUE)
Omega3 <- solve(Sigma3)
Sigma3r <- sqrt(Sigma3)

mu4 <- c(-3,-3)
Sigma4 <- matrix(nr = 2, nc = 2, data = c(1, 0.8, 0.8, 1), byrow = TRUE)
Omega4 <- solve(Sigma4)
eig4 <- eigen(Sigma4)
Sigma4r <- eig4$vectors %*% diag(sqrt(eig4$values))

mu5 <-c(3,-3)
Sigma5 <- matrix(nr = 2, nc = 2, data = c(1, -0.8, -0.8, 1), byrow = TRUE)
Omega5 <- solve(Sigma5)
eig5 <- eigen(Sigma5)
Sigma5r <- eig5$vectors %*% diag(sqrt(eig5$values))


qf <- function(x, mu, Omega) sum((x-mu) * (Omega %*% (x - mu)))

lev <- qchisq(p = c(.95, .8, 2/3, 1/2, 1/4), df  = 2)

###

set.seed(18971)
n <- 50
X1 <- rmvnorm(50, mean = mu1, sigma = Sigma1)
X2 <- rmvnorm(50, mean = mu2, sigma = Sigma2)
X3 <- rmvnorm(50, mean = mu3, sigma = Sigma3)
X4 <- rmvnorm(50, mean = mu4, sigma = Sigma4)
X5 <- rmvnorm(50, mean = mu5, sigma = Sigma5)

pdf("../fig/contours.pdf")
plot(X1[,1], X1[,2], col = "blue", xlim = c(-8,8), ylim = c(-8,8))
points(X2[,1], X2[,2], col = "red")
points(X3[,1], X3[,2], col = "darkgreen")
points(X4[,1], X4[,2], col = "purple")
points(X5[,1], X5[,2], col = "turquoise")

gr <- seq(from = 0, to = 2*pi, length = 200)
for(i in 1:length(lev)){
lines(sqrt(lev[i]) * cos(gr), sqrt(lev[i]) * sin(gr), col = "blue")

t2 <- cbind(cos(gr), sin(gr)) %*% Sigma2r
lines(sqrt(lev[i]) * t2[,1] + mu2[1], sqrt(lev[i]) * t2[,2]+mu2[2], col = "red")

t3 <- cbind(cos(gr), sin(gr)) %*% Sigma3r
lines(sqrt(lev[i]) * t3[,1] + mu3[1], sqrt(lev[i]) * t3[,2]+mu3[2], col = "darkgreen")

t4 <- cbind(cos(gr), sin(gr)) %*% t(Sigma4r)
lines(sqrt(lev[i]) * t4[,1] + mu4[1], sqrt(lev[i]) * t4[,2]+mu4[2], col = "purple")

t5 <- cbind(cos(gr), sin(gr)) %*% t(Sigma5r)
lines(sqrt(lev[i]) * t5[,1] + mu5[1], sqrt(lev[i]) * t5[,2]+mu5[2], col = "turquoise")
}
dev.off()
