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
