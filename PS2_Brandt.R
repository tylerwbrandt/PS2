## Tyler Brandt - Problem Set 2

rm(list = ls())
test_data <- c(1:100)

# Leemis M
ones_digits <- c(1:9)
integer_votes <- NULL
for (i in ones_digits){
  integer_votes <- c(integer_votes, length(test_data[as.numeric(substr(test_data,1,1)) == i]))
}

