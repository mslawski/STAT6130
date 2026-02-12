### Respiratory illnesss data set: 
### multivariate Regression and MANOVA

resp <- read.csv("../../data/resp/resp.csv", header = TRUE)

### MANOVA with two outcome variables fef50, fef75
### for groups defined by variables
### -- (1) zone: enviromental pollution zone
### -- (2) edu_parents: level of parents' education

## (1) covariate zone

# Here, we use the standard ANOVA encoding of the factor of interest. In the associated
# parameterization, the model parameters are the "grand mean" (over all factor levels)
# and departures from the grand mean, respectively. This is accomplished by  
# specifying contrasts() as 'contr.sum'

resp$zone <- as.factor(resp$zone)
contrasts(resp$zone) = contr.sum(3)
manova.zone <- manova(cbind(log(resp$fef50), log(resp$fef75)) ~ zone, data = resp)
summary(manova.zone, test = "Wilks")
# model.matrix(manova.zone)

# we can compare this to two univariate ANOVA's:
anova.fef50.zone <- anova(lm(log(resp$fef50) ~ zone, data = resp))
print(anova.fef50.zone)

anova.fef75.zone <- anova(lm(log(resp$fef75) ~ zone, data = resp))
print(anova.fef75.zone)

## (2) covariate edu_parents (level of eduction of parents)

resp$edu_parents <- as.factor(resp$edu_parents)
contrasts(resp$edu_parents) = contr.sum(3)
manova.edu_parents <- manova(cbind(log(resp$fef50), log(resp$fef75)) ~ edu_parents, data = resp)
summary(manova.edu_parents, test = "Wilks")

# corresponding univariate ANOVA's:
anova.fef50.edu_parents <- anova(lm(log(resp$fef50) ~ edu_parents, data = resp))
print(anova.fef50.edu_parents)

anova.fef75.edu_parents <- anova(lm(log(resp$fef75) ~ edu_parents, data = resp))
print(anova.fef75.edu_parents)

### ******************************************
### Multivariate Linear Regression Modeling
### ******************************************

### outcome variables: all four lung function measurements [log-transformed]
### log(fvc), log(pef), log(fef50), log(fef75)

### predictor variables:
### "mother_smokes", "freq_cold", "freq_cough", "sex" (all binary)
### "weight" (continuous)

# re-code resp$sex to 0/1 (all other binary variables are already coded as 0/1)
resp$sex <- resp$sex - 1


mlm <- lm(cbind(log(fvc), log(pef), log(fef50), log(fef75))
   ~ mother_smokes + freq_cold + freq_cough +  sex + weight,
   data = resp)

coef(mlm)
Betahat <- coef(mlm)

# estimator of covariance matrix of the error terms
n <- nrow(resp)
d <- ncol(model.matrix(mlm))

Sigma_hat <- crossprod(residuals(mlm))/(n - d)
cov2cor(Sigma_hat)

### linear hypothesis tests

### 1: global null hypothesis -- none of the covariates has any
###    effect on any outcome variable

A <- cbind(0, diag(d-1))
q <- ncol(Betahat)
D <- matrix(data = 0, nrow = d - 1, ncol = q)

XtX <- crossprod(model.matrix(mlm))

Delta <- A %*% Betahat - D

SSP_extra <- crossprod(Delta, solve(A %*% solve(XtX, t(A)), Delta))
SSP_full <- Sigma_hat * (n - d)

Lambda <- det(SSP_full) / (det(SSP_full + SSP_extra))

# using Bartlett approximation to find p-value
pval <- 1 - pchisq(-log(Lambda) * ((n - d) - 0.5 * (q - (d-1) + 1)), df = q * (d-1))

### function for testing general linear hypothesis
lin_hypothesis_test <- function(A, D){

  # determine the rank of A
  rkA <- sum(svd(A)$d > sqrt(.Machine$double.eps))

  Delta <- A %*% Betahat - D

  SSP_extra <- crossprod(Delta, solve(A %*% solve(XtX, t(A)), Delta))
  SSP_full <- Sigma_hat * (n - d)

  Lambda <- det(SSP_full) / (det(SSP_full + SSP_extra))

  pval <- 1 - pchisq(-log(Lambda) * ((n - d) - 0.5 * (q - rkA + 1)), df = q * rkA)

  return(list(statistic = Lambda, pval = pval))

}

# Example: test for the significance of the variable "weight"
#          (all outcome variables simultaneously)

A <- t(rep(0, d))
A[d] <- 1
D <- t(rep(0, q))

lin_hypothesis_test(A, D)
# quicker:
anova(mlm, test = "Wilks") # this performs the Wilks' tests for each of the variables

# we can also use this function to compare two models:
# in this model, we drop the variable "weight"
mlm0 <- update(mlm, formula = ~. -weight)
anova(mlm, mlm0, test = "Wilks") # same result as the previous call to lin_hypothesis_test.

# Example 2:
# test whether freq_cold and freq_cough have the same effect on all the outcome variables
A <- t(c(0, 0, 1, -1, 0, 0))
D <- t(rep(0,q))

lin_hypothesis_test(A, D)
