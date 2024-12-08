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

test_that("a 5x5 rationalized matrix makes sense", {
  matrix_size = 5
  Q_map <- spectrum_sbg_cpp(matrix_size, matrix_size, GABOR_UNCERTAINTY ^ 2)

  spectrum_sbg_sparse <- Q_map %>%
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

  expect_equal(class(Q_map), "data.frame")
  expect_equal(class(spectrum_sbg_dense), c("matrix", "array"))
  expect_named(Q_map, c('x', 'y', 'idealized_x', 'idealized_y',
                        'rationalized_x', 'rationalized_y',
                        'original_value', 'num', 'den', 'approximation', 'error',
                        'uncertainty', 'depth', 'path', 'path_id',
                        'shannon_entropy', 'run_length_encoding', 'hamming_weight'))
  expect_equal(Q_map$error, rep(0,25))
  expect_equal(Q_map %>% dplyr::distinct(x, y) %>%  nrow(), 25)
  expect_equal(Q_map %>% dplyr::distinct(idealized_x, idealized_y) %>%  nrow(), 25)
  expect_equal(Q_map %>% dplyr::distinct(rationalized_x, rationalized_y) %>%  nrow(), 17)

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

      # Extract the corresponding row from Q_map
      Q_cell <- Q_map[Q_map$x == x_r & Q_map$y == y_r, ]

      # Ensure exactly one match
      expect_equal(nrow(Q_cell), 1)

      # Validate idealized_x and idealized_y
      expected <- expected_frequencies[[i, j]]
      expect_equal(unname(Q_cell$idealized_x), expected[1], label = paste("Mismatch at (", x_r, ",", y_r, ") for idealized_x"))
      expect_equal(unname(Q_cell$idealized_y), expected[2], label = paste("Mismatch at (", x_r, ",", y_r, ") for idealized_y"))
    }
  }

})

# Helper function for a single test
# test_rationalized_matrix <- function(length) {
#   test_that(paste0("a ", length, "x", length, " map from a uniform 2D spectrum makes sense"), {
#     uniform_matrix <- matrix(1 + 0i, nrow = length, ncol = length)
#     rationalized_matrix <- rationalized_spectrum_cpp(uniform_matrix)
#     vdiffr::expect_doppelganger(
#       paste0("Rationalized Matrix ", length, "x", length),
#       plot_matrix(rationalized_matrix)
#     )
#   })
# }
#
# # Wrapper function to run multiple tests
# test_rationalized_matrices <- function(lengths) {
#   lapply(lengths, test_rationalized_matrix)
# }
#
# # Call the wrapper function with the desired lengths
# test_rationalized_matrices(c(5, 35, 63, 127, 511))


# Helper function for a single test
test_error_histogram <- function(length) {
  uncertainty = GABOR_UNCERTAINTY ^ 2
  test_that(paste0("a ", length, "x", length, " map from a uniform 2D spectrum makes sense"), {
    Q_map <- fourier_transform_2D_map(length, length)
    vdiffr::expect_doppelganger(
      paste0("Error Histogram ", sprintf("%.4f", uncertainty), " ", length, "x", length),
      plot_error_histogram(Q_map$error)
    )
  })
}

# Wrapper function to run multiple tests
test_error_histograms <- function(lengths) {
  lapply(lengths, test_error_histogram)
}

# Call the wrapper function with the desired lengths
test_error_histograms(c(5, 35, 63, 127, 511))
