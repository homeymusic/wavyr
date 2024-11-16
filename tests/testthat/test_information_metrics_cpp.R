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

# Test for integer conversion
test_that("to_integer_cpp converts bits to integer correctly", {
  for (case in binary_test_cases) {
    result <- as_integer_cpp(case$string)
    expect_equal(result, case$integer)
  }
})

# Shannon Entropy
test_that("shannon_entropy_cpp computes correct values", {
  for (case in binary_test_cases) {
    result <- shannon_entropy_cpp(case$string)
    expect_equal(result, case$shannon_entropy, tolerance = 1e-6)
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
