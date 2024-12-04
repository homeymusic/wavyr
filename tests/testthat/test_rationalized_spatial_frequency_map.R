source(testthat::test_path("helper.R"))

test_that("a 5x5 rationalized matrix makes sense", {
  # Generate the rationalized spatial frequency map
  time_taken <- system.time({
    Q_map <- rationalized_spatial_frequency_map(5, 5)
  })["elapsed"]
  expect_lt(time_taken, 1.0)

  # Ensure Q_map is a data frame
  expect_equal(class(Q_map), "data.frame")
  expect_equal(Q_map %>% dplyr::distinct(x, y) %>%  nrow(), 25)
  expect_equal(Q_map %>% dplyr::distinct(k_x, k_y) %>%  nrow(), 25)
  expect_equal(Q_map %>% dplyr::distinct(k_Q_x, k_Q_y) %>%  nrow(), 17)

  # Expected idealized spatial frequency map
  expected_frequencies <- matrix(list(
    c(0, 0),  c(1, 0),  c(2, 0),  c(-2, 0),  c(-1, 0),
    c(0, 1),  c(1, 1),  c(2, 1),  c(-2, 1),  c(-1, 1),
    c(0, 2),  c(1, 2),  c(2, 2),  c(-2, 2),  c(-1, 2),
    c(0, -2), c(1, -2), c(2, -2), c(-2, -2), c(-1, -2),
    c(0, -1), c(1, -1), c(2, -1), c(-2, -1), c(-1, -1)
  ), nrow = 5, byrow = TRUE)

  # Ensure Q_map has all required columns
  expect_named(Q_map, c("x", "y", "k_x", "k_y", "k_Q_x", "k_Q_y"))

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

      # Validate k_x and k_y
      expected <- expected_frequencies[[i, j]]
      expect_equal(unname(Q_cell$k_x), expected[1], label = paste("Mismatch at (", x_r, ",", y_r, ") for k_x"))
      expect_equal(unname(Q_cell$k_y), expected[2], label = paste("Mismatch at (", x_r, ",", y_r, ") for k_y"))
    }
  }
})

test_that("a 512x512 rationalized matrix make sense", {

  # Generate the rationalized spatial frequency map
  time_taken <- system.time({
    Q_map <- rationalized_spatial_frequency_map(512, 512)
  })["elapsed"]
  expect_lt(time_taken, 1.0)
  expect_equal(class(Q_map), c('data.frame'))

  expect_equal(Q_map %>% dplyr::distinct(x, y) %>%  nrow(), 262144)
  expect_equal(Q_map %>% dplyr::distinct(k_x, k_y) %>%  nrow(), 262144)
  expect_equal(Q_map %>% dplyr::distinct(k_Q_x, k_Q_y) %>%  nrow(), 14545)
})

rationalized_spectrum <- function(source) {

  rationalized_map = rationalized_matrix(rows=nrow(source),columns=ncol(source))$matrix

  # Loop through the rows and columns
  for (i in seq_len(nrow(source))) {
    for (j in seq_len(ncol(source))) {
      browser()
      x_y_prime = rationalized_map[[i,j]]
      i_prime = x_y_prime['x']
      j_prime = x_y_prime['y']
      rationalized_matrix[[i, j]] = rationalized_matrix[[i, j]] + source[[i_prime, j_prime]]
    }
  }
  rationalized_matrix
}

test_that("a 5x5 map from a uniform 2D spectrum make sense", {
  uniform_matrix <- matrix(1 + 0i, nrow = 5, ncol = 5)

  rationalized_matrix = rationalized_spectrum(uniform_matrix)

  vdiffr::expect_doppelganger(
    "Uniform Matrix 5x5",
    plot_matrix(uniform_matrix)
  )

  expected_rationalized_spectrum <- matrix(
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

  expect_equal(rationalized_matrix, expected_rationalized_spectrum, tolerance=0.01)

  vdiffr::expect_doppelganger(
    "Rationalized Matrix 5x5",
    plot_matrix(rationalized_matrix)
  )
})

test_that("a 32x32 map from a uniform 2D spectrum make sense", {
  uniform_matrix <- matrix(1 + 0i, nrow = 32, ncol = 32)

  rationalized_matrix = rationalized_spectrum_cpp(uniform_matrix,
                                                  rationalized_matrix(32,32)$matrix)

  vdiffr::expect_doppelganger(
    "Rationalized Matrix 32x32",
    plot_matrix(rationalized_matrix)
  )
})

test_that("a 64x64 map from a uniform 2D spectrum make sense", {
  uniform_matrix <- matrix(1 + 0i, nrow = 64, ncol = 64)

  rationalized_matrix = rationalized_spectrum_cpp(uniform_matrix,
                                                  rationalized_matrix(64,64)$matrix)

  vdiffr::expect_doppelganger(
    "Rationalized Matrix 64x64",
    plot_matrix(rationalized_matrix)
  )
})

test_that("a 128x128 map from a uniform 2D spectrum make sense", {
  uniform_matrix <- matrix(1 + 0i, nrow = 128, ncol = 128)

  rationalized_matrix = rationalized_spectrum_cpp(uniform_matrix,
                                                  rationalized_matrix(128,128)$matrix)

  vdiffr::expect_doppelganger(
    "Rationalized Matrix 128x128",
    plot_matrix(rationalized_matrix)
  )
})

test_that("a 512x512 map from a uniform 2D spectrum make sense", {
  uniform_matrix <- matrix(1 + 0i, nrow = 512, ncol = 512)
  expect_true(uniform_matrix %>% as.vector() %>% `==`(1+0i) %>% all())

  rationalized_matrix = rationalized_spectrum_cpp(uniform_matrix,
                                                  rationalized_matrix(512,512)$matrix)

  expect_equal(rationalized_matrix %>% as.vector() %>% Mod() %>% min(), 0)
  expect_equal(rationalized_matrix %>% as.vector() %>% Mod() %>% max(), 453)

  vdiffr::expect_doppelganger(
    "Rationalized Matrix 512x512",
    plot_matrix(rationalized_matrix)
  )
})
test_that("a 1024x1024 map from a uniform 2D spectrum make sense", {
  uniform_matrix <- matrix(1 + 0i, nrow = 1024, ncol = 1024)

  rationalized_matrix = rationalized_spectrum_cpp(uniform_matrix,
                                                  rationalized_matrix(1024,1024)$matrix)

  vdiffr::expect_doppelganger(
    "Rationalized Matrix 1024x1024",
    plot_matrix(rationalized_matrix)
  )
})
