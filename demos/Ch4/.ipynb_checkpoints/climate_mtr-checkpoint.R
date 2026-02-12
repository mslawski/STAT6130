### Multi-Task Regression: Beijing climate data set
ls()
library(glmnet) # lasso and ridge regression
library(grpreg) # group lasso multi-task regression
library(rrr)    # reduced-rank regression

###
ls()
data_all <- read.csv("../data/climate/data_all.csv", header = TRUE)

# coarsening of the categorical variable wind direction
# after merging categories, wind directions are "East", "NorthEast", "Northwest", "SouthEast", "SouthWest", "West"
wd_coarse <- data_all$wd
wd_coarse[wd_coarse %in% c("E", "ENE", "ESE")] <- "E"
wd_coarse[wd_coarse %in% "NNE"] <- "NE"
wd_coarse[wd_coarse %in% "NNW"] <- "NW"
wd_coarse[wd_coarse %in% "SSE"] <- "SE"
wd_coarse[wd_coarse %in% "SSW"] <- "SW"
wd_coarse[wd_coarse %in% c("WNW", "WSW")] <- "W"

data_all$wd_coarse <- as.factor(wd_coarse)

# data preparation (subsetting to Nongzhanguan, extract outcome and predictor variable, set coding of wd_coarse
dat <- data_all[data_all$station == "Nongzhanguan", colnames(data_all) %in% c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "TEMP", "PRES", "RAIN", "WSPM", "wd_coarse")]
# here wind direction is converted corresponding to the sum-to-zero constraint for the regression coefficients
contrasts(dat$wd_coarse) <- contr.sum(nlevels(dat$wd_coarse))

### some simple exploratory testing of different "candidate" models
# plain
mtr0 <- lm(log(cbind(PM2.5, PM10, SO2, NO2, CO, O3)) ~ TEMP + PRES + RAIN + WSPM, data = dat)
# quadratic terms
mtr1 <- lm(log(cbind(PM2.5, PM10, SO2, NO2, CO, O3)) ~ (TEMP + PRES + RAIN + WSPM)^2, data = dat)
# plain +  wind direction
mtr <- lm(log(cbind(PM2.5, PM10, SO2, NO2, CO, O3)) ~ TEMP + PRES + RAIN + WSPM + as.factor(wd_coarse), data = dat)
# final: same as previous, but includes interaction terms
mtr2 <- lm(log(cbind(PM2.5, PM10, SO2, NO2, CO, O3)) ~ TEMP + PRES + RAIN + (WSPM * as.factor(wd_coarse)), data = dat)

# R-squared comparisons (one for each outcome variable and model)
unlist(lapply(summary(mtr0), function(z) z$r.squared))
unlist(lapply(summary(mtr1), function(z) z$r.squared))
unlist(lapply(summary(mtr), function(z) z$r.squared))
unlist(lapply(summary(mtr2), function(z) z$r.squared))

# the model 'mtr2' achieves comparable or even better R^2's than
# the quadratic model (which has significantly more parameters)

### partition "dat" into three chunks
### --- training
### --- validation: model selection
### --- test: model evaluation
trainix <- seq(from = 1, to = nrow(dat), by = 3)
validix <- seq(from = 2, to = nrow(dat), by = 3)
testix <- seq(from = 3, to = nrow(dat), by = 3)
# training
dat_train <- dat[trainix,]
# validation
dat_valid <- dat[validix,]
# test
dat_test <- dat[testix,]

# Here, we generate design matrices and response vector for the three data subsets (training, validation, testing)
# the function model.matrix is quite useful. It extracts/generates the design matrix ("X") that corresponds to a model formula
Xtrain <- model.matrix(formula(mtr2), data = dat_train)
ytrain  <- subset(log(cbind(dat$PM2.5, dat$PM10, dat$SO2, dat$NO2, dat$CO, dat$O3)), subset = (1:nrow(dat) %in% trainix))
colnames(ytrain) <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")
ntrain <- nrow(ytrain)

Xvalid <- model.matrix(formula(mtr2), data = dat_valid)
yvalid  <- subset(log(cbind(dat$PM2.5, dat$PM10, dat$SO2, dat$NO2, dat$CO, dat$O3)), subset = (1:nrow(dat) %in% validix))
colnames(yvalid) <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")
nvalid <- nrow(yvalid)

Xtest <- model.matrix(formula(mtr2), data = dat_test)
ytest  <- subset(log(cbind(dat$PM2.5, dat$PM10, dat$SO2, dat$NO2, dat$CO, dat$O3)), subset = (1:nrow(dat) %in% testix))
colnames(ytest) <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")
ntest <- nrow(ytest)

q <- ncol(ytrain)
d <- ncol(Xtrain)
# Identity matrix of dimension q
Id_q <- diag(q)

# generate "long" design matrices and "long" response vectors (cf. p. 18 in class-04.pdf)
# this is needed to ensure we can run the lasso using the glmnet package (cf. p. 18 in class-04.pdf)
Xtrain_long <- matrix(nrow = q * ntrain, ncol = d  * q)
Xvalid_long <- matrix(nrow = q * nvalid, ncol = d  * q)
Xtest_long <- matrix(nrow = q * ntest, ncol = d  * q)
for(i in 1:nrow(Xtrain)){
    Xtrain_long[((i-1)*q + 1):(i*q),] <- kronecker(Xtrain[i,], Id_q)
    Xvalid_long[((i-1)*q + 1):(i*q),] <- kronecker(Xvalid[i,], Id_q)
    Xtest_long[((i-1)*q + 1):(i*q),] <- kronecker(Xtest[i,], Id_q)
}
ytrain_long <- c(t(ytrain))
yvalid_long <- c(t(yvalid))
ytest_long <- c(t(ytest))


# this line compares the coefficients from the matrix representation of the multivariate linear model
# to the coefficients from the long format -- they need to be identical!
#coef(lm(ytrain ~ Xtrain - 1)) - matrix(coef(lm(ytrain_long ~ Xtrain_long - 1)), nrow = d, ncol = q)


# these are the ordinary least regression again -- note that the intercept is already included in 'Xtrain_long'
beta_ols <- coef(lm(ytrain_long ~ Xtrain_long - 1))

#mse_ols <- mean((yvalid_long - Xvalid_long %*% beta_ols)^2)

# baselines to beat: plain OLS and an intercept model (cf. slide 38 in class-04.pdf)
mse_ols <- mean((ytest_long - Xtest_long %*% beta_ols)^2)
mse_intercept <- mean((apply(ytest, MARGIN = 1, STAT = colMeans(ytrain), FUN = "-"))^2)

### lasso
# we use the intercepts in Xtrain_long, and do not penalize the corresponding columns
# the glmnet function automatically defines a grid of lambda values and obtains the
# corresponding solutions

lasso_train <- glmnet(x = Xtrain_long, y = ytrain_long, intercept = FALSE,
                      penalty_factor = rep(c(0,rep(1, d- 1)), times = q))

# compute the MSEs on the validation set
mses_valid_lasso <- colMeans((yvalid_long - Xvalid_long %*% lasso_train$beta)^2)
# plot(lasso_train$lambda, mses_valid_lasso) -- we see that the optimal lambda is zero

# compute lasso test error for optimal lambda
lasso_lambdaix <- which.min(mses_valid_lasso)
mses_test_lasso <- mean((ytest_long - Xtest_long %*% lasso_train$beta[,lasso_lambdaix])^2)

# print / visualize the lasso coefficients --- there are indeed some entries exactly equal to zero
Beta_lasso <- matrix(lasso_train$beta[,lasso_lambdaix], nrow = d, ncol = q)
rownames(Beta_lasso) <- colnames(Xtrain)
colnames(Beta_lasso) <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")
image(abs(Beta_lasso[2:d,]))

### ridge regression

ridge_train <- glmnet(x = Xtrain_long, y = ytrain_long, intercept = FALSE,
                      penalty_factor = rep(c(0,rep(1, d- 1)), times = q), alpha = 0)

mses_valid_ridge <- colMeans((yvalid_long - Xvalid_long %*% ridge_train$beta)^2)
ridge_lambdaix <- which.min(mses_valid_ridge)
mses_test_ridge <- mean((ytest_long - Xtest_long %*% ridge_train$beta[,ridge_lambdaix])^2)

### group lasso regression

# this may take a few minutes minute or so to run --- this function works with the
# matrix version than with the long version of the data
fit_grpreg <- grpreg(Xtrain[,-1], ytrain, penalty="grLasso",  family= "gaussian",
nlambda=100)

mses_valid_grplasso <- numeric(dim(fit_grpreg$beta)[3])

for(i in 1:dim(fit_grpreg$beta)[3]){
    mses_valid_grplasso[i] <- mean((yvalid - Xvalid %*% t(fit_grpreg$beta[,,i]))^2)
}

# this shows the row sparsity pattern of the solution(s)
t(fit_grpreg$beta[,,10])

# extract test MSE for the value of lamada achieving smallest validation error
grplasso_lambdaix <- which.min(mses_valid_grplasso)
mses_test_grplasso <- mean((ytest - Xtest %*% t(fit_grpreg$beta[,,grplasso_lambdaix]))^2)



### reduced rank regression

# consider the ranks 1 up to 6 for the coeffcient matrix B
rank_grid <- 1:6
errs_valid_rank <- numeric(length(rank_grid))
errs_test_rank <- numeric(length(rank_grid))

# compute validation and test errors for each value of the rank(1 through 6)
for(r in 1:length(rank_grid)){

    Beta_rr <- rrr(Xtrain[,2:d], ytrain, type = "identity", rank = r)
    errs_valid_rank[r] <- mean((sweep(yvalid, MARGIN = 2, STATS = Beta_rr$mean) - Xvalid[,2:d] %*% t(Beta_rr$C))^2)
   errs_test_rank[r] <- mean((sweep(ytest, MARGIN = 2, STATS = Beta_rr$mean) - Xtest[,2:d] %*% t(Beta_rr$C))^2)

}
