outcome <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], xlab = "30-Day Mortality Rates for HA")
