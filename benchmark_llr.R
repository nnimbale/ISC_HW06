# Load necessary libraries
library(microbenchmark)

source("llr_functions.R")  # file includes llr, compute_f_hat, make_weight_matrix, make_predictor_matrix

# Load the french fries data for testing
data(french_fries, package = "reshape2")
french_fries <- french_fries[complete.cases(french_fries), ]

# Set up input variables
x <- french_fries$potato
y <- french_fries$buttery
z <- seq(0, 15, length.out = 100)
omega <- 2

# Benchmark the llr function
benchmark_results <- microbenchmark(
  llr(z = z, x = x, y = y, omega = omega),
  times = 10  # Number of times to repeat for averaging
)

# Print benchmark results
cat("Benchmark results for llr function:\n")
print(benchmark_results)

# Optional: Print average time
cat("Average runtime:", mean(benchmark_results$time) / 1e6, "milliseconds\n")
