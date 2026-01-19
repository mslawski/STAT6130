### Flight delay data set

flights <- read.csv("flights.csv")
nrow(flights) ### n = 5.8M records

airport_list <- c("ATL", "DFW", "DEN", "ORD", "LAX", "CLT", "MCO", "LAS", "PHX", "MIA", "JFK", "SFO", "SEA", "EWR", "IAH", "BOS", "MSP", "DTW", "FLL")
# five digit airport codes (only used in October...)
airport_codes <- c("10397", "11298", "11292", "13930", "12892", "11057", "13204", "12889", "14107", "13303", "12478", "14771", "14747", "11618", "12266", "10721", "13487", "11433", "11697")
to_repl <- flights$ORIGIN_AIRPORT %in% airport_codes
flights[to_repl,"ORIGIN_AIRPORT"] <- airport_list[match(flights$ORIGIN_AIRPORT[to_repl], airport_codes)]

flights_major_ap <- flights[flights$ORIGIN_AIRPORT %in% airport_list,]
                                        # roughly 50% of all records

agg <- aggregate(flights_major_ap$CANCELLED, by = list(flights_major_ap$ORIGIN_AIRPORT, flights_major_ap$MONTH,flights_major_ap$DAY), FUN = mean)

# convert to 365 x 19 data matrix
dat <- matrix(nrow = nrow(agg)/length(airport_list), ncol = length(airport_list))
for(i in 1:(nrow(agg)/length(airport_list))){
 dat[i,] <- agg[((i-1)*length(airport_list) + 1):(i*length(airport_list)),]$x
}
colnames(dat) <- airport_list
# obtain correlation matrix
cor(dat)
