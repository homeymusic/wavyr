source(testthat::test_path("helper.R"))

test_that("a 5x5 rationalized matrix make sense", {

  k_Q = rationalized_matrix(rows=5,columns=5)

  expect_equal(class(k_Q), c('rationalized_matrix'))

  # Expected idealized spatial frequency map
  expected_frequencies <- matrix(list(
    c(x = 0, y = 0),  c(x = 1, y = 0),  c(x = 1, y = 0),  c(x = -1, y = 0),  c(x = -1, y = 0),
    c(x = 0, y = 1),  c(x = 1, y = 1),  c(x = 2, y = 1),  c(x = -2, y = 1),  c(x = -1, y = 1),
    c(x = 0, y = 1),  c(x = 1, y = 2),  c(x = 1, y = 1),  c(x = -1, y = 1),  c(x = -1, y = 2),
    c(x = 0, y = -1), c(x = 1, y = -2), c(x = 1, y = -1), c(x = -1, y = -1), c(x = -1, y = -2),
    c(x = 0, y = -1), c(x = 1, y = -1), c(x = 2, y = -1), c(x = -2, y = -1), c(x = -1, y = -1)
  ), nrow = 5, byrow = TRUE)

  # Compare each element of the matrices
  for (i in seq_len(nrow(expected_frequencies))) {
    for (j in seq_len(ncol(expected_frequencies))) {
      expect_equal(k_Q$matrix[[i, j]], expected_frequencies[[i, j]], info=paste("i: ", i, "j: ", j))
    }
  }

})

test_that("a 512x512 rationalized matrix make sense", {

  time_taken <- system.time({
    k_Q = rationalized_matrix(rows=512,columns=512)
  })["elapsed"]
  expect_lt(time_taken, 1.0)
  expect_equal(class(k_Q), c('rationalized_matrix'))

  v = k_Q$matrix %>% as.vector() %>% unlist()

  expect_equal(v %>% min(), -255)
  expect_equal(v %>% max(), 256)
  expect_equal(sum(v==0), 1024)
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
