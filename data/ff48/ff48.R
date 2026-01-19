### Fama & French data set
ff48 <- read.csv("ff48_546months.csv")

pdf("../fig/ff48_ind1.pdf")
par(mar = c(4.5, 5.5, 2.1, 2.1))
plot(ff48[,1], lwd = 2, type = "l", xlab = "month", ylab = "return", main = "Industry 1", cex.lab = 2.2, cex.axis = 1.8, cex.main = 2.5)
lines(filter(ff48[,1], rep(1/9, 9)), col = "red", lwd = 1.5)
dev.off()

pdf("../fig/ff48_ind2.pdf")
par(mar = (4.5, 5.5, 2.1, 2.1))
plot(ff48[,2], lwd = 2, type = "l", xlab = "month", ylab = "return", main = "Industry 2", cex.lab = 2.2, cex.axis = 1.8, cex.main = 2.5)
lines(filter(ff48[,2], rep(1/9, 9)), col = "red", lwd = 1.5)
dev.off()


pdf("../fig/ff48_ind3.pdf")
par(mar = c(4.5, 5.5, 2.1, 2.1))
plot(ff48[,3], lwd = 2, type = "l", xlab = "month", main = "Industry 3", ylab = "return", cex.lab = 2.2, cex.axis = 1.8, cex.main = 2.5)
lines(filter(ff48[,3], rep(1/9, 9)), col = "red", lwd = 1.5, cex.lab = 2.2, cex.axis = 1.8)
dev.off()
