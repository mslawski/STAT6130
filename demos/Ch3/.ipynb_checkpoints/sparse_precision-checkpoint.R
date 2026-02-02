ls()

### Example 1: Math Scores
library(ggm)
data(marks)

### re-fit inverse covariance matrix subject to the following constraints

### Analysis cond. indep. of Mechanics
### Statistics cond. indep. of Mechanics
### Analysis cond. indep of Vectors
### Statistics cond. indep. of Vectors

### 10 possible edges - 4 edges = 6 edges left

# obtain adjacency matrix of this graph
Adj <- matrix(nr = ncol(marks), nc = ncol(marks), data = 1)
diag(Adj) <- 0
Adj[1,c(4,5)] <- 0
Adj[2,c(4,5)] <- 0
Adj[c(4,5),1] <- 0
Adj[c(4,5),2] <- 0
colnames(Adj) <- colnames(marks)
rownames(Adj) <- colnames(marks)

# use function fitConGraph in the package ggm
# this function computes the constrained MLE
# (subject to the constraint that selected off-diagonal entries are zero)
fitPrec <- fitConGraph(Adj, cov(marks), n = nrow(marks), cli = list
(c("vectors", "algebra", "mechanics"),
c("algebra", "analysis", "statistics")))
zapsmall(solve(fitPrec$Shat)*1000)
# note that the selected elements are exactly equal to zero.


### chaingraph and graphical lasso

set.seed(13)
library(glasso)
library(mvtnorm)
d <- 50
n <- 200 # training data size
n_te <- 500 # test data size


rho <- 0.8
Sigma <- matrix(nrow = d, ncol = d, data = 0)
for(j in 1:d){
    for(k in 1:d){
     Sigma[j,k] <- rho^(abs(j-k))
    }
 }

Omega <- solve(Sigma)
image(Omega) # inverse is a band-matrix with bandwidth 1

# training data
X <- rmvnorm(n = n, sigma = Sigma)
S <- crossprod(X) / n

# test data
Xtest <- rmvnorm(n = n_te, sigma = Sigma)
Stest <- crossprod(Xtest)/n_te

# fit the graphical lasso to the training data
lambdagrid <- 10^seq(from = -3, to = 0, by = 0.1)#10^seq(from = -4, to = 0, by = 0.25)

glasso_sol <- glassopath(S, rholist = lambdagrid, penalize.diagonal=FALSE)
image(glasso_sol$wi[,,length(lambdagrid)-20])

# Stein loss (negative log-likelihood) on the test set
loss <- function(Omegahat) -determinant(Omegahat, log = TRUE)$modulus + sum(Omegahat * Stest)

testerrs <- numeric(length(lambdagrid))

for(k in 1:length(testerrs)){
 testerrs[k] <- loss(glasso_sol$wi[,,k])
}

plot(log(lambdagrid), testerrs)
image(glasso_sol$wi[,,which.min(testerrs)])

### real data example: an excerpt from the Climate data

data_all <- read.csv("../data/climate/data_all.csv", header = TRUE)

# This example follows a similar pattern as the example in
# highdim_PCA.R. The main difference is that the variable
# of interest is now the PM2.5 concentration. As before,
# we extract blocks of 24 hours and consider the 24 hours in a day as variables.
# Different rows are treated as different days (treated independently, for simplicity)

dat_Nong <- data_all[data_all$station == "Nongzhanguan",]
rm(data_all)
dates <- paste(dat_Nong$month, dat_Nong$day, dat_Nong$year, sep = "-")
dat_day <- dat_Nong[,colnames(dat_Nong) %in% c("hour", "day", "month", "year", "PM2.5")]

dat_day_c <- split(dat_day["PM2.5"], f = as.factor(dates))
leng <- unlist(lapply(dat_day_c, function(z) length(z[[1]])))


dat_day_f <- matrix(nrow = sum(leng == 24), ncol = 24, data = 0)
counter <- 0
for(i in 1:length(dat_day_c)){
 tmp <-  dat_day_c[[i]]
 if(nrow(tmp) == 24){
     counter <- counter + 1
     dat_day_f[counter,] <- as.matrix(tmp)
 }
}

dat_final <- log(dat_day_f)
Cfull <- cor(dat_final)

### --- sparse inverse covariance matrix seems like a suitable model
image(Cfull)
image(solve(Cfull))

# select only every 10th data point out of the training set
# (note that training set and test set are supposed be
# non-overlapping, here we ignore this issue for simplicity)

train <- seq(from = 1, to = nrow(dat_final), by = 10)
dat_train <- dat_final[train,]

loss_real <- function(Omegahat) -determinant(Omegahat, log = TRUE)$modulus + sum(Omegahat * Cfull)

glasso_sol <- glassopath(cor(dat_train), rholist = lambdagrid, penalize.diagonal=FALSE)
#image(glasso_sol$wi[,,length(lambdagrid)])

# Stein loss (negative log-likelihood) on the test set


testerrs_real <- numeric(length(lambdagrid))

for(k in 1:length(testerrs)){
 testerrs_real[k] <- loss_real(glasso_sol$wi[,,k])
}

image(glasso_sol$wi[,,which.min(testerrs_real)])
# boxplot off-diagonal entries, w/regularization
Omegahat_real <- glasso_sol$wi[,,which.min(testerrs_real)]
boxplot(-Omegahat_real[lower.tri(Omegahat_real)])
# boxplot w/o regularization
Omegahat_plain <- solve(cor(dat_train))
boxplot(-Omegahat_plain[lower.tri(Omegahat_plain)])
