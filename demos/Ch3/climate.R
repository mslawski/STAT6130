library(tidyr)

### Beijing climate data set

### data preparation
setwd("../../data/climate/")
filenames <- list.files()
filenames <- filenames[grep("PRSA", filenames)]

stations <- unlist(lapply(sapply(filenames, strsplit, "_"), function(z) z[[3]]))
names(stations) <- rep("", length(stations))

#sapply(filenames, function(z) length(readLines(z)))-1
# each file has 35,064 records

n0 <- 35064

data_combined <- as.data.frame(matrix(nrow = length(filenames)*n0, ncol = 17))
colnames(data_combined) <- colnames(read.csv(filenames[1], nrows = 1, sep = ",")[,-1])
data_combined[,"station"] <- factor(data_combined[,"station"], levels = stations)

for(i in 1:length(filenames)){

    data_combined[((i-1)*n0+1):(i*n0),] <- read.csv(filenames[i], header = TRUE)[,-1]

}

data_all <- data_combined %>% drop_na()
#saveRDS(data_all, file = "data_all.rds")

### consider only one station, extract levels of six pollutants

dat_log <- data_all %>% 
    filter (station == "Nongzhanguan") %>% 
    select(all_of(c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3"))) %>% 
    mutate(across(everything(), log))

# marginal distributions
boxplot(dat_log)

# correlation matrix
cor(dat_log)

# eigenvalues
eigen(cor(dat_log))$values
# percentage of variance explained 
cumsum(eigen(cor(dat_log))$values)/sum(eigen(cor(dat_log))$values)

### compute principal components from hand

# 1 --- center and scale
dat_log_s <- scale(dat_log)
colMeans(dat_log_s) # 0
apply(dat_log_s, 2, sd) #1

#
V <- eigen(cov(dat_log_s))$vectors
Z <- dat_log_s %*% V
#apply(Z, 2, var) 

#pdf("../../fig/PCs_scatter.pdf")

# scatterplot of first two PCs
plot(Z[,1], Z[,2], cex = 1.5, pch = 16)

#dev.off()
cov(Z) # all uncorrelated; diagonal equal to eigen(cor(dat_log))$values
