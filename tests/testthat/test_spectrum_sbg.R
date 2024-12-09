source(testthat::test_path("helper.R"))

create_dense_matrix <- function(sparse_df, size) {
  # Ensure the size is odd
  if (size %% 2 == 0) {
    stop("The size parameter must be an odd number.")
  }

  # Ensure the size is large enough
  max_value <- max(abs(sparse_df$rationalized_x), abs(sparse_df$rationalized_y))
  if (size < 2 * max_value + 1) {
    stop("The size parameter must be greater than or equal to the maximum range of rationalized_x and rationalized_y.")
  }

  # Initialize a dense matrix of zeros
  dense_matrix <- matrix(0, nrow = size, ncol = size)

  # Compute the center index of the matrix
  center <- (size + 1) / 2

  # Map sparse matrix coordinates to the dense matrix
  for (i in seq_len(nrow(sparse_df))) {
    x <- sparse_df$rationalized_x[i]
    y <- sparse_df$rationalized_y[i]
    count <- sparse_df$count[i]

    # Map to dense matrix indices
    row <- center - y
    col <- center + x
    dense_matrix[row, col] <- count
  }

  return(dense_matrix)
}

test_spectrum_makes_sense <- function(s, label) {
  matrix_size = dim(s)[1] %>% sqrt()
  spectrum_sbg_sparse <- s %>%
    dplyr::group_by(rationalized_x, rationalized_y) %>%
    dplyr::summarize(count = dplyr::n(), .groups = "drop")

  spectrum_sbg_dense <- create_dense_matrix(spectrum_sbg_sparse, matrix_size)

  expected_kernel <- matrix(
    c(0, 1, 0, 1, 0,
      1, 2, 2, 2, 1,
      0, 2, 1, 2, 0,
      1, 2, 2, 2, 1,
      0, 1, 0, 1, 0),
    nrow = 5, ncol = 5, byrow = TRUE
  )

  expect_equal(spectrum_sbg_dense, expected_kernel, tolerance=0.01)

  vdiffr::expect_doppelganger(label, function() plot_matrix(spectrum_sbg_dense, fft_shift = F))

  expect_equal(class(s), "data.frame")
  expect_equal(class(spectrum_sbg_dense), c("matrix", "array"))
  expect_named(s, c('x', 'y', 'idealized_x', 'idealized_y',
                        'rationalized_x', 'rationalized_y',
                        'original_value', 'num', 'den', 'approximation', 'error',
                        'uncertainty', 'depth', 'path', 'path_id',
                        'shannon_entropy', 'run_length_encoding', 'hamming_weight'))
  expect_equal(s$error, rep(0,25))
  expect_equal(s %>% dplyr::distinct(x, y) %>%  nrow(), 25)
  expect_equal(s %>% dplyr::distinct(idealized_x, idealized_y) %>%  nrow(), 25)
  expect_equal(s %>% dplyr::distinct(rationalized_x, rationalized_y) %>%  nrow(), 17)

  # Expected idealized spatial frequency map
  expected_frequencies <- matrix(list(
    c(0, 0),  c(1, 0),  c(2, 0),  c(-2, 0),  c(-1, 0),
    c(0, 1),  c(1, 1),  c(2, 1),  c(-2, 1),  c(-1, 1),
    c(0, 2),  c(1, 2),  c(2, 2),  c(-2, 2),  c(-1, 2),
    c(0, -2), c(1, -2), c(2, -2), c(-2, -2), c(-1, -2),
    c(0, -1), c(1, -1), c(2, -1), c(-2, -1), c(-1, -1)
  ), nrow = 5, byrow = TRUE)

  # Check if each entry matches expected values
  for (i in seq_len(nrow(expected_frequencies))) {
    for (j in seq_len(ncol(expected_frequencies))) {
      # Convert to 1-based indices for R
      x_r <- j
      y_r <- i

      # Extract the corresponding row from s
      Q_cell <- s[s$x == x_r & s$y == y_r, ]

      # Ensure exactly one match
      expect_equal(nrow(Q_cell), 1)

      # Validate idealized_x and idealized_y
      expected <- expected_frequencies[[i, j]]
      expect_equal(unname(Q_cell$idealized_x), expected[1], label = paste("Mismatch at (", x_r, ",", y_r, ") for idealized_x"))
      expect_equal(unname(Q_cell$idealized_y), expected[2], label = paste("Mismatch at (", x_r, ",", y_r, ") for idealized_y"))
    }
  }

}

test_that("a 5x5 rationalized matrix from cpp makes sense", {
  s = spectrum_sbg_cpp(5, 5, GABOR_UNCERTAINTY ^ 2)
  test_spectrum_makes_sense(
    s, 'sbg spectrum from cpp directly 5 x 5'
  )
})

test_that("a 5x5 rationalized matrix from data file makes sense", {
  s = spectrum_sbg(5, 5, GABOR_UNCERTAINTY ^ 2)
  test_spectrum_makes_sense(
    s, 'sbg spectrum from data file 5 x 5'
  )
})

# Helper function for a single test
test_error_histogram <- function(length) {
  uncertainty = GABOR_UNCERTAINTY ^ 2
  test_that(paste0("a ", length, "x", length, " map from a uniform 2D spectrum makes sense"), {
    s <- spectrum_sbg(length, length)
    vdiffr::expect_doppelganger(
      paste0("Error Histogram ", sprintf("%.4f", uncertainty), " ", length, "x", length),
      function() plot_error_histogram(s$error)
    )
  })
}

# Wrapper function to run multiple tests
test_error_histograms <- function(lengths) {
  lapply(lengths, test_error_histogram)
}

# Call the wrapper function with the desired lengths
test_error_histograms(2^(1:7) + 1)
