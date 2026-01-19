rent <- read.csv("../../data/munich_rent/munich_rent.csv", header = TRUE)

### (1) Correlation matrix
Sigma <- cor(cbind(rent = rent$rent, area = rent$area, rooms = rent$rooms))
print(Sigma)

### (2) Partial correlation matrix
Sigmainv <- solve(Sigma)
PC <- -cov2cor(Sigmainv)
diag(PC) <- 1

print(PC)

### (3) Partial residual plot

#pdf("../fig/partial_regression.pdf")
plot(residuals(lm(rent~area, data =rent)), residuals(lm(rooms ~ area, data = rent)), xlab = "rent", ylab = "rooms")
#dev.off()


