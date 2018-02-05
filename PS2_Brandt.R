## Tyler Brandt - Problem Set 2

rm(list = ls())
test_data <- c(1:100)

# Leemis M
ones_digits <- c(1:9)
integer_votes <- NULL
for (i in ones_digits){
  integer_votes <- c(integer_votes, length(test_data[as.numeric(substr(test_data,1,1)) == i]))
}
total_votes <- length(test_data)
percent_votes <- integer_votes/total_votes
logarithm <- log(1+1/ones_digits, base = 10)
leemis_set <- percent_votes - logarithm
leemis_m <- max(leemis_set)
