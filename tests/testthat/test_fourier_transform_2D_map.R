source(testthat::test_path("helper.R"))

test_that("a 5x5 rationalized matrix makes sense", {
  # Generate the rationalized spatial frequency map
  Q_map <- fourier_transform_2D_map_cpp(5, 5, GABOR_UNCERTAINTY ^ 2)
  expect_equal(class(Q_map), "data.frame")
  expect_named(Q_map, c('x', 'y', 'idealized_x', 'idealized_y',
                        'rationalized_x', 'rationalized_y',
                        'original_value', 'num', 'den', 'approximation', 'error',
                        'uncertainty', 'depth', 'path', 'path_id',
                        'shannon_entropy', 'run_length_encoding', 'hamming_weight'))
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

test_that("a 5x5 map from a uniform 2D spectrum make sense", {
  uniform_matrix <- matrix(1 + 0i, nrow = 5, ncol = 5)

  rationalized_matrix = rationalized_spectrum_cpp(uniform_matrix)

  expected_rationalized_spectrum_cpp <- matrix(
    c(
      1 + 0i, 2 + 0i, 0 + 0i, 0 + 0i, 2 + 0i,
      2 + 0i, 2 + 0i, 1 + 0i, 1 + 0i, 2 + 0i,
      0 + 0i, 1 + 0i, 0 + 0i, 0 + 0i, 1 + 0i,
      0 + 0i, 1 + 0i, 0 + 0i, 0 + 0i, 1 + 0i,
      2 + 0i, 2 + 0i, 1 + 0i, 1 + 0i, 2 + 0i
    ),
    nrow = 5,
    byrow = TRUE
  )

  expect_equal(rationalized_matrix, expected_rationalized_spectrum_cpp, tolerance=0.01)

})

# Helper function for a single test
test_rationalized_matrix <- function(length) {
  test_that(paste0("a ", length, "x", length, " map from a uniform 2D spectrum makes sense"), {
    uniform_matrix <- matrix(1 + 0i, nrow = length, ncol = length)
    rationalized_matrix <- rationalized_spectrum_cpp(uniform_matrix)
    vdiffr::expect_doppelganger(
      paste0("Rationalized Matrix ", length, "x", length),
      plot_matrix(rationalized_matrix)
    )
  })
}

# Wrapper function to run multiple tests
test_rationalized_matrices <- function(lengths) {
  lapply(lengths, test_rationalized_matrix)
}

# Call the wrapper function with the desired lengths
test_rationalized_matrices(c(5, 31, 63))
