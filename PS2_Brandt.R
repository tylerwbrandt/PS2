## Tyler Brandt - Problem Set 2

rm(list = ls())
test_data <- c(1:100)

# 1: Make a function to calculate the Leemis m and the Cho gains d statistic.
## benford_tests takes in two arguments, a vector (data) and a choice for which statistics to return (stat).
## The return will be your chosen statistic for your given data, along with the integer distribution
## for the first significant digit.
## The first argument (data) should be a vector containing observed vote totals.
## The second argument (stat) must be one of three options: {"leemis", "cho", or "both"}.
## Inputting "leemis" here will return the leemis m statistic for your observed vote totals
## along with the integer distribution for the first significant digit.
## Inputting "cho" will return the cho-gains d statistic for your observed vote totals
## along with the integer distribution for the first significant digit.
## Inputting "both" will return both the leemis m statistic and the cho-gains d statistic
## for your observed vote totals along with the integer distribution for the first significant digit.
benford_tests <- function(data, stat){
  ones_digits <- c(1:9)
  integer_votes <- NULL
  for (i in ones_digits){
    integer_votes <- c(integer_votes, length(data[as.numeric(substr(data,1,1)) == i]))
  }
  total_votes <- length(data)
  percent_votes <- integer_votes/total_votes
  logarithm <- log(1+1/ones_digits, base = 10)
  leemis_set <- percent_votes - logarithm
  leemis_m <- max(leemis_set)
  cho_set_sq <- leemis_set^2
  sum_cho <- sum(cho_set_sq)
  cho_d <- sqrt(sum_cho)
  leemis_only <- list(paste("leemis m =", leemis_m), paste(c(1:9), "-", integer_votes, sep = ""))
  cho_only <- list(paste("cho-gains d =", cho_d), paste(c(1:9), "-", integer_votes, sep = ""))
  both <- list(paste("leemis m =", leemis_m), paste("cho-gains d =", cho_d), paste(c(1:9), "-", integer_votes, sep = ""))
  if (stat == "leemis"){
    return (leemis_only)
  } else if (stat == "cho"){
    return (cho_only)
  } else if (stat == "both"){
    return (both)
  } else {
    return ("Not an option, please choose one of {'leemis', 'cho', or 'both'}")
  }
}

benford_tests(test_data, "leemis")
benford_tests(test_data, "cho")
benford_tests(test_data, "both")
benford_tests(test_data, "stats")