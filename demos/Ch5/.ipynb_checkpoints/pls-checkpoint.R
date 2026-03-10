### Multi-Task Regression: Beijing climate data set
### Application of Partial Least Squares Regression

ls()
library(pls) # PLS package

data_all <- read.csv("../data/climate/data_all.csv", header = TRUE)


# coarsening of the categorical variable wind direction
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
### --- validation
### --- test
trainix <- seq(from = 1, to = nrow(dat), by = 3)
validix <- seq(from = 2, to = nrow(dat), by = 3)
testix <- seq(from = 3, to = nrow(dat), by = 3)
dat_train <- dat[trainix,]
dat_valid <- dat[validix,]
dat_test <- dat[testix,]

Xtrain <- model.matrix(formula(mtr2), data = dat_train)
ytrain  <- subset(log(cbind(dat$PM2.5, dat$PM10, dat$SO2, dat$NO2, dat$CO, dat$O3)), subset = (1:nrow(dat) %in% trainix))
colnames(ytrain) <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")
ntrain <- nrow(ytrain)

### Everything above is from climate_mtr.R
### Below are the new additions regarding PLS regression:


### From-scratch computation (subsequently, we will use the 'pls' package)

# compute first few PLS regression components (as a test)

Xtrain_cent <- scale(Xtrain, scale = FALSE)
ytrain_cent <- scale(ytrain, scale = FALSE)

# first component
w1 <- svd(crossprod(Xtrain_cent, ytrain_cent))$u[,1] # optimal weight vector in the PLS optimization problem
t1 <- Xtrain_cent %*% w1 # first PLS component ('X-score')
c1 <- t(coef(lm(ytrain_cent ~ t1 - 1))) # Y-loading
u1 <- ytrain_cent %*% c1 # 'Y-score'
p1 <- t(coef(lm(Xtrain_cent ~ t1 - 1))) # X-loading
b1 <- w1 %*% t(c1) / sum(w1 * p1) # regression coefficents defined by the relatinship t1 %*% t(c1) = Xtrain_cent %*% b1

# deflation --- subtract the current fit of X and Y
Xtrain_cent_1 <- Xtrain_cent - t1 %*% t(p1)
ytrain_cent_1 <- ytrain_cent - u1 %*% t(c1)

# compute next components based on the residuals in the deflation step
w2 <- svd(crossprod(Xtrain_cent_1, ytrain_cent_1))$u[,1]
t2 <- Xtrain_cent_1 %*% w2
c2 <- t(coef(lm(ytrain_cent_1 ~ t2 - 1)))
u2 <- ytrain_cent_1 %*% c2
p2 <- t(coef(lm(Xtrain_cent_1 ~ t2 - 1)))



# visualization --- X-scores  vs. Y scores -- these should correlate well
plot(t1, u1)

cor(t1, u1) # about .5 correlation, which corresponds to an R^2 of .25
cor(t2, u2) # the correlation of the next pair of scores should be less than for the first pair

# here, we verify the results by comparing to the 'pls' package
test_pls <- mvr(ytrain ~ Xtrain, ncomp = 2)
mean(abs(as.matrix(test_pls$scores[,1]) -  t1)) # equal
mean(abs(test_pls$loading.weights[,1] -  w1)) # equal
mean(abs(test_pls$loadings[,1] -  p1)) # equal
mean(abs(test_pls$Yloadings[,1] -  c1)) # equal
mean(abs(test_pls$Yscores[,1] -  u1 )) # proportional

# the latter are not the same, but proportional (up to a scaling factor)
#lm(test_pls$Yscores[,1] ~ u1)
#cor(test_pls$Yscores[,1],u1)

mean(abs(as.matrix(test_pls$scores[,2]) -  t2)) # ~equal
mean(abs(test_pls$loading.weights[,2] -  w2)) # ~equal
mean(abs(test_pls$loadings[,2] -  p2)) # ~equal
mean(abs(test_pls$Yloadings[,2] -  c2)) # ~equal
mean(abs(test_pls$Yscores[,2] -  u2 )) # ~proportional

### last part: evaluate the predictive performance of PLS

Xvalid <- model.matrix(formula(mtr2), data = dat_valid)
yvalid  <- subset(log(cbind(dat$PM2.5, dat$PM10, dat$SO2, dat$NO2, dat$CO, dat$O3)), subset = (1:nrow(dat) %in% validix))
colnames(yvalid) <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")
nvalid <- nrow(yvalid)

Xtest <- model.matrix(formula(mtr2), data = dat_test)
ytest  <- subset(log(cbind(dat$PM2.5, dat$PM10, dat$SO2, dat$NO2, dat$CO, dat$O3)), subset = (1:nrow(dat) %in% testix))
colnames(ytest) <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3")
ntest <- nrow(ytest)


ncomp <- 1:12
mse_valid <- numeric(length(ncomp))

for(i in 1:length(ncomp)){

    test_pls <- mvr(ytrain ~ Xtrain, ncomp = max(ncomp))
    yhat_valid <- drop(predict(test_pls, newdata = Xvalid, ncomp = ncomp[i]))
    mse_valid[i] <- mean((yhat_valid - yvalid)^2)

}

### pick 5 components here, since validation error does not drop dramatically after

pls5 <- mvr(ytrain ~ Xtrain, ncomp = 5)
yhat_test <- drop( predict(pls5, newdata = Xtest, ncomp = 5) )
mse_test <- mean((yhat_test - ytest)^2) # cf. results on class-09.pdf


### *** END *** #######################################################################
