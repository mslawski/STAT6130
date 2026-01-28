ls()
leuk <- read.csv("../../data/leuk/leukemia_small.csv")
isALL <- numeric(ncol(leuk))
# assign the label 1 to all ALL patients
isALL[grep("ALL", colnames(leuk))] <- 1

# size of subsample (the smaller the better the peformance of the J-S estimator)
n <- 25 #35 #40

# extract subsample from the ALL group
sel <- which(isALL == 1)[1:n]
Z <- t(leuk[,sel])

# empirical mean estimator (baseline)
zbar <- colMeans(Z)

# J-S estimator

# compute estimator of the variance sigma_n^2
Zcen <- sweep(Z, 2, STATS = zbar, FUN = "-")
sigmasq_hat <- sum(Zcen^2)/((nrow(Z)-1) * ncol(Z))
sigmasq_n_hat <- sigmasq_hat / nrow(Z)

# compute lambda_{J-S}
lambda_JS <- sigmasq_n_hat/((sum(zbar^2)/(ncol(Z)-2)))

muhat <- zbar * (1 - lambda_JS)

# means of the full sample -- here treated as "ground truth"
mu <- rowMeans(leuk[,which(isALL == 1)])

# MSE of J-S estimator
mean((muhat - mu)^2)

# MSE of empirical mean baseline
mean((zbar - mu)^2)
