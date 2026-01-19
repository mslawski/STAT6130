library(dplyr)
library(tidyr)

flights <- read.csv("../../../data/flights.csv")
nrow(flights) ### n = 5.8M records

head(flights, n = 5)

# extract list of 19 airports of interest 
airport_list <- c("ATL", "DFW", "DEN", "ORD", "LAX", "CLT", "MCO", "LAS", "PHX", "MIA", "JFK", "SFO", "SEA", "EWR", "IAH", "BOS", "MSP", "DTW", "FLL")

# recode five-digit airport codes used in the month of October (cf. "INSTRUCTIONS" file)
airport_codes <- c("10397", "11298", "11292", "13930", "12892", "11057", "13204", "12889", "14107", "13303", "12478", "14771", "14747", "11618", "12266", "10721", "13487", "11433", "11697")

names(airport_list) <- airport_codes

flights <- flights %>%
  mutate(ORIGIN_AIRPORT = recode(ORIGIN_AIRPORT, !!!airport_list))

# extract records corresponding to the above 19 airports only (roughly 50% of all records) 
flights_major_ap <- flights %>% 
    filter(ORIGIN_AIRPORT %in% airport_list) 

# compute the daily fractions of cancelled flights for each of the 19 airports
agg <- flights_major_ap %>%
  group_by(ORIGIN_AIRPORT, MONTH, DAY) %>%
  summarize(CANCELLED = mean(CANCELLED, na.rm = TRUE), .groups = "drop")

# re-arrange in a matrix with rows corresponding to days and columns corresponding to airports
dat <- agg %>%
  pivot_wider(
    names_from = ORIGIN_AIRPORT, 
    values_from = CANCELLED
  ) %>% select(-c("MONTH", "DAY"))

head(dat)

### logit transformation for improved normality

datm <- as.matrix(dat)
logit <- function(p) log((p + 0.001)/(1-p + 0.001))

plot(logit(datm[,1]), logit(datm[,2]))

# divide into blocks of size 13
blocklength <- 13
nblocks <- floor(nrow(dat)/blocklength)
blockindex <- numeric(nrow(dat))
blockindex[1:(nblocks * blocklength)] <- rep(1:nblocks, each = blocklength)
blockindex[(nblocks * blocklength):nrow(dat)] <- nblocks
table(blockindex) # 28 blocks of length 13 -- 14 max

# data set consisting of 13-day averages
logit_data_avg <- dat %>%
  mutate(blockindex = blockindex) %>%
  group_by(blockindex) %>%
  summarize(across(everything(), ~ mean(logit(.x), na.rm = TRUE))) %>%
  select(-c("blockindex")) %>%
  as.matrix()

head(logit_data_avg)

boxplot(dat)
boxplot(logit(dat))
boxplot(logit_data_avg)

plot(logit_data_avg[,1], logit_data_avg[,2], pch = 16, cex = 1.5)
plot(logit_data_avg[,5], logit_data_avg[,12], pch = 16, cex = 1.5)
plot(logit_data_avg[,8], logit_data_avg[,15], pch = 16, cex = 1.5)


