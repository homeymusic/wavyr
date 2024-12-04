test_that("the various maps for a 5x5 matrix make sense", {

  k_idealized = idealized_2D_frequency_matrix(rows=5,columns=5)
  expect_equal(class(k_idealized), c('idealized_2D_frequency_map'))

  # Expected spatial frequency map
  expected_frequencies <- matrix(list(
    c(x = 0, y = 0),  c(x = 1, y = 0),  c(x = 2, y = 0),  c(x = -2, y = 0),  c(x = -1, y = 0),
    c(x = 0, y = 1),  c(x = 1, y = 1),  c(x = 2, y = 1),  c(x = -2, y = 1),  c(x = -1, y = 1),
    c(x = 0, y = 2),  c(x = 1, y = 2),  c(x = 2, y = 2),  c(x = -2, y = 2),  c(x = -1, y = 2),
    c(x = 0, y = -2), c(x = 1, y = -2), c(x = 2, y = -2), c(x = -2, y = -2), c(x = -1, y = -2),
    c(x = 0, y = -1), c(x = 1, y = -1), c(x = 2, y = -1), c(x = -2, y = -1), c(x = -1, y = -1)
  ), nrow = 5, byrow = TRUE)

  # Compare each element of the matrices
  for (i in seq_len(nrow(expected_frequencies))) {
    for (j in seq_len(ncol(expected_frequencies))) {
      expect_equal(k_idealized$matrix[[i, j]], expected_frequencies[[i, j]])
    }
  }

  k_Q = rationalized_2D_frequency_map(rows=5,columns=5)

  expect_equal(class(k_Q), c('rationalized_2D_frequency_map'))

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
      expect_equal(k_Q[[i, j]], expected_frequencies[[i, j]], info=paste("i: ", i, "j: ", j))
    }
  }

})
