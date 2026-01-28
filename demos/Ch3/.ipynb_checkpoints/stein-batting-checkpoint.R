### James-Stein estimator
ls()

d <- 1000
n <- 5

tausq <- 1/4
sigmasq <- 1

mus <- rnorm(d, sd = sqrt(tausq))
Z <- sapply(mus, function(mu) rnorm(n, mean = mu, sd = sqrt(sigmasq)))

zbar <- apply(Z, 2, mean)
mean((zbar - mus)^2)

sigmasq_n <- sigmasq / n
muhat_star <- zbar * (sigmasq_n / (tausq + sigmasq_n))

mean((muhat_star - mus)^2)

# in case sigmasq, tausq are not known:
Zcen <- sweep(Z, 2, STATS = colMeans(Z), FUN = "-")
sigmasq_hat <- sum(Zcen^2)/((n-1) * d) # averages over empirical standard deviations
sigmasq_n_hat <- sigmasq_hat / n

inv_tausq_plus_sigmasq_n_hat <- 1/(sum(zbar^2)/(d-2))

muhat <- zbar * sigmasq_n_hat * inv_tausq_plus_sigmasq_n_hat

mean((muhat - mus)^2)

# error reduction of about 40% compared to sample means

### real data example: batting averages

batting <- read.csv("../data/batting.csv", header = TRUE)

# $y: empirical means early in the season
# $p: true means (rest of the season)

zbar <- mean(batting$y) # grand mean (shrinkage target)
ntrials <- 45
sigmasq_hat <- zbar * (1 - zbar) / ntrials#binomial proportion varianc estimator
S <- sum((batting$y - zbar)^2)
muhat <- zbar + (1 - (nrow(batting) - 3)*sigmasq_hat / S) * (batting$y - zbar)

mean((batting$y - batting$p)^2) # MSE, standard mean
mean((muhat - batting$p)^2) # MSE, J-S


x11()
pdf("../fig/batting.pdf")
plot(batting$p, abs(batting$p - batting$y)/batting$p, ylim = c(0,0.5),
     xlab = "batting avg (rest of the season)", ylab = "RAE", cex.lab = 1.5, cex = 1.5)
points(batting$p, abs(batting$p - muhat)/batting$p, pch = 16, col = "blue", cex = 1.5)
dev.off()
