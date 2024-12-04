test_that("a 5x5 idealized matrix make sense", {

  k_idealized = idealized_matrix(rows=5,columns=5)
  expect_equal(class(k_idealized), c('idealized_matrix'))

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

})

test_that("a 512x512 idealized matrix make sense", {

  k_idealized = idealized_matrix(rows=512,columns=512)
  expect_equal(class(k_idealized), c('idealized_matrix'))

  v = k_idealized$matrix %>% as.vector() %>% unlist()

  expect_equal(v %>% min(), -255)
  expect_equal(v %>% max(), 256)
  expect_equal(sum(v==0), 1024)
})
