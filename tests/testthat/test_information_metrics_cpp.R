binary_test_cases <- list(
  list(string = c(0, 0, 0, 0, 0),  # All zeros
       shannon_entropy = 0,       # No uncertainty
       hamming_weight = 0,        # No 1s
       run_length_encoding = 1,   # Single run
       depth = 5,                 # Depth of 5
       integer = 0),              # Integer representation

  list(string = c(1, 1, 1, 1, 1),  # All ones
       shannon_entropy = 0,        # No uncertainty
       hamming_weight = 5,         # All 1s
       run_length_encoding = 1,    # Single run
       depth = 5,                  # Depth of 5
       integer = 31),              # Integer representation (11111 in binary)

  list(string = c(0, 1, 0, 1, 0, 1),  # Alternating
       shannon_entropy = 1,           # Maximum entropy for binary
       hamming_weight = 3,            # Half 1s
       run_length_encoding = 6,       # Alternating runs
       depth = 6,                     # Depth of 6
       integer = 21),                 # Integer representation (10101 in binary)

  list(string = c(0, 1, 1, 0, 0, 1, 0, 1),  # Mixed
       shannon_entropy = 1.0,               # Corrected to 1.0
       hamming_weight = 4,                  # Equal 0s and 1s
       run_length_encoding = 6,             # Runs of 0s and 1s
       depth = 8,                           # Depth of 8
       integer = 101)                       # Integer representation (01100101 in binary)
)


# Shannon Entropy
test_that("shannon_entropy_cpp computes correct values", {
  for (case in binary_test_cases) {
    result <- shannon_entropy_cpp(case$string)
    expect_equal(result, case$shannon_entropy, tolerance = 1e-6)
  }
})

# Test for integer conversion
test_that("to_integer_cpp converts bits to integer correctly", {
  for (case in binary_test_cases) {
    result <- as_integer_cpp(case$string)
    expect_equal(result, case$integer)
  }
})

# Hamming Weight
test_that("hamming_weight_cpp computes correct values", {
  for (case in binary_test_cases) {
    result <- hamming_weight_cpp(case$string)
    expect_equal(result, case$hamming_weight, tolerance = 1e-6)
  }
})

# Run-Length Encoding
test_that("run_length_encoding_cpp computes correct values", {
  for (case in binary_test_cases) {
    result <- run_length_encoding_cpp(case$string)
    expect_equal(result, case$run_length_encoding, tolerance = 1e-6)
  }
})

# Depth
test_that("depth_cpp computes correct values", {
  for (case in binary_test_cases) {
    result <- depth_cpp(case$string)
    expect_equal(result, case$depth, tolerance = 1e-6)
  }
})

# Binary String to String Conversion
test_that("as_string_cpp converts bits to string correctly", {
  for (case in binary_test_cases) {
    result <- as_string_cpp(case$string)
    expected_string <- paste(case$string, collapse = "")
    expect_equal(result, expected_string)
  }
})

# Test Shannon Entropy with the entropy package (Expanded)
test_that("shannon_entropy_cpp matches entropy package implementation (expanded)", {
  library(entropy)
  # Define more diverse test cases
  test_cases <- list(
    c(0, 0, 0, 0, 0),                # All zeros
    c(1, 1, 1, 1, 1),                # All ones
    c(0, 1, 0, 1, 0, 1),             # Alternating
    c(1, 0, 1, 0, 1, 0, 1, 0),       # Alternating starting with 1
    c(0, 1, 1, 0, 0, 1, 0, 1),       # Mixed
    c(1, 1, 0, 0, 1, 1, 0, 0),       # Repeating blocks
    c(1, 0, 0, 1, 0, 0, 1, 1),       # Irregular pattern
    c(0),                            # Single bit (0)
    c(1),                            # Single bit (1)
    c(0, 1),                         # Minimal binary pair
    c(rep(0, 100)),                  # All zeros (long sequence)
    c(rep(1, 100)),                  # All ones (long sequence)
    rep(c(0, 1), 50),                # Alternating long sequence
    c(0, 0, 0, 1, 1, 1, 0, 0, 1),    # Pattern with clusters
    c(1, 1, 1, 0, 0, 0, 1, 1, 0),    # Inverse of above
    c(0, 1, 0, 1, 1, 0, 1, 1, 0, 1), # Long mixed pattern
    rep(c(1, 0, 0), 33),             # Repeated triplet
    rep(c(0, 1, 1), 33)              # Repeated triplet (inverted)
  )

  for (test_bits in test_cases) {
    # Calculate using Rcpp function
    cpp_result <- shannon_entropy_cpp(test_bits)

    # Calculate using entropy package
    counts <- table(test_bits)
    entropy_pkg_result <- entropy.empirical(counts, unit = "log2")

    # Compare results
    expect_equal(cpp_result, entropy_pkg_result, tolerance = 1e-6)
  }
})
