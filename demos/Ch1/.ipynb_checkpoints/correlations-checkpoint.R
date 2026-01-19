### Correlations

### sign change: Munich rent data

rent <- read.csv("../data/munich_rent.csv", header = TRUE)

Sigma <- cor(cbind(rent$rent, rent$area, rent$rooms))


Sigmainv <- solve(Sigma)
PC <- -cov2cor(Sigmainv)
diag(PC) <- 1

pdf("../fig/partial_regression.pdf")
plot(residuals(lm(rent~area, data =rent)), residuals(lm(rooms ~ area, data = rent)), xlab = "rent", ylab = "rooms")
dev.off()
