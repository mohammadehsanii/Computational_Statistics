# TRI-Section Method to find the minimum of a given function

# Define the function fx to minimize
fx <- function(x) {
  # The function is f(x) = x^2 + 2 * exp(-x)
  x^2 + 2 * exp(-x)
}

# Define the initial bounds for the search interval
x1 <- 0   # Lower bound
x2 <- 2   # Upper bound

# Initialize the number of iterations counter
inter <- 0 

# Calculate the initial partition size D
D <- (x2 - x1) / 3   # Divide the interval into three equal parts

# Set the tolerance value (epsilon) for the stopping condition
eps <- 1e-6

# Repeat until the interval is sufficiently small (less than eps)
while (D > eps) {
  
  # Increment the iteration counter
  inter <- inter + 1
  
  # Calculate the two internal points x3 and x4
  x3 <- x1 + D        # x3 is 1/3 of the way from x1 to x2
  x4 <- x2 - D        # x4 is 1/3 of the way from x2 to x1
  
  # Evaluate the function at x3 and x4
  f3 <- fx(x3)
  f4 <- fx(x4)
  
  # Compare function values at x3 and x4
  # If f(x3) < f(x4), discard the right part of the interval (x4 to x2)
  if (f3 < f4) {
    x2 <- x4  # Move x2 to x4, effectively narrowing the interval from the right
  } else {
    # Otherwise, discard the left part of the interval (x1 to x3)
    x1 <- x3  # Move x1 to x3, effectively narrowing the interval from the left
  }
  
  # Update D for the new interval size
  D <- (x2 - x1) / 3
}

# Calculate the midpoint of the final interval as the approximate minimum
x0 <- (x1 + x2) / 2 

# Evaluate the function at x0 (the midpoint)
fm <- fx(x0)

# Output the results
cat("x1 (lower bound):", x1, "\n")
cat("x2 (upper bound):", x2, "\n")
cat("x0 (midpoint, approximate minimum):", x0, "\n")
cat("Function value at x0 (fm):", fm, "\n")
cat("Number of iterations (inter):", inter, "\n")
