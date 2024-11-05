# Load necessary library
library(numbers)

# Function to check if a number can be expressed as b + c + c
check_conjecture <- function(n) {
  if (n <= 5 || n %% 2 == 0) {
    return(FALSE)
  }
  
  primes <- Primes(n)
  for (b in primes) {
    for (c in primes) {
      if (b + 2 * c == n) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

# Function to filter positive odd numbers from input values
filter_positive_odd_numbers <- function(values) {
  return(values[values %% 2 != 0 & values > 0])
}

# Test the conjecture for positive odd numbers greater than 5 up to a given range
test_conjecture <- function(input_values) {
  input_values <- filter_positive_odd_numbers(input_values)  # Filter positive odd numbers
  results <- data.frame(Number = integer(), Result = logical())
  for (i in input_values) {
    result <- check_conjecture(i)
    results <- rbind(results, data.frame(Number = i, Result = result))
  }
  return(results)
}

# Set a range of numbers for testing
input_values <- 5000:6000
results <- test_conjecture(input_values)

# Print results
print(results)

# Summary of results
table(results$Result)
