library(dplyr)
library(tidyr)

flights <- read.csv("../data/flights.csv")
nrow(flights) ### n = 5.8M records

head(flights, n = 5)

# extract 31 hub airports 
airport_list <- c("ATL", "DFW", "DEN", "ORD", "LAX", "CLT", "MCO", "LAS", "PHX", "MIA", "JFK", "SFO", 
                  "SEA", "EWR", "IAH", "BOS", "MSP", "DTW", "FLL", "LGA", "PHL", "SLC", "BWI", "IAD", 
                  "SAN", "DCA", "TPA", "BNA", "AUS", "HNL", "MDW")

# recode five-digit airport codes used in the month of October (cf. "INSTRUCTIONS" file)
airport_codes <- c("10397", "11298", "11292", "13930", "12892", "11057", "13204", "12889", "14107", "13303", "12478", "14771", "14747", "11618", "12266", 
                   "10721", "13487", "11433", "11697", "12953", "14100", "14869", "10821", "12264", "14679",
                  "11278", "15304", "10693", "10423", "12173", "13232")

names(airport_list) <- airport_codes

flights <- flights %>%
  mutate(ORIGIN_AIRPORT = recode(ORIGIN_AIRPORT, !!!airport_list))

# extract records corresponding to the above 31 hub airports only (roughly 50% of all records) 
flights_major_ap <- flights %>% 
    filter(ORIGIN_AIRPORT %in% airport_list) 

# compute the daily fractions of cancelled flights for each of the 31 airports, 
# additionally grouped by airlines
agg <- flights_major_ap %>%
  group_by(AIRLINE, ORIGIN_AIRPORT, MONTH, DAY) %>%
  summarize(CANCELLED = mean(CANCELLED, na.rm = TRUE), .groups = "drop")

# re-arrange in a tensor with "rows" corresponding to days, 
# "columns" corresponding to airports, and 3rd dimension corresponding to airlines 
# NOTE: note all (airport, airline) combinations are in the data!
# we are excluding 'US' because it does not offer flights every day  
airlines <- setdiff(unique(flights_major_ap$AIRLINE), "US")  
d1 <- 365 #days
d2 <- 31 # airports
d3 <- length(airlines)
Dat = array(data = 0, dim = c(d1, d2, d3))

for(i in 1:length(airlines)){
dat <- agg %>% filter(AIRLINE == airlines[i]) %>%
  pivot_wider(
    names_from = ORIGIN_AIRPORT, 
    values_from = CANCELLED
  ) %>% select(-c("MONTH", "DAY"))

    dat <- dat[,-1]
    airports_val <- apply(dat, 2, function(z) all(!is.na(z)))
    
    airport_ix <- match(colnames(dat)[airports_val], airport_list)
    Dat[,airport_ix,i] <- as.matrix(dat[,airports_val]) * 100 # scale to make it percentage
}

library(rTensor)

set.seed(0331)
ntf_result <- cp(as.tensor(Dat), num_components = 5, max_iter = 100)
