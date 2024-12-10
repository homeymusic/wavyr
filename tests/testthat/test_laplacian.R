source(testthat::test_path("helper.R"))

laplacian <- matrix(
  c(0, 1, 1, 2, 2, 2, 1, 1, 0,
    1, 2, 4, 5, 5, 5, 4, 2, 1,
    1, 4, 5, 3, 0, 3, 5, 4, 1,
    2, 5, 3, -12, -24, -12, 3, 5, 2,
    2, 5, 0, -24, -40, -24, 0, 5, 2,
    2, 5, 3, -12, -24, -12, 3, 5, 2,
    1, 4, 5, 3, 0, 3, 5, 4, 1,
    1, 2, 4, 5, 5, 5, 4, 2, 1,
    0, 1, 1, 2, 2, 2, 1, 1, 0),
  nrow = 9, byrow = TRUE
)

lenna <- (load_and_preprocess_image(test_path("images", "Lenna.png")))$grayscale_matrix

test_that('the laplacian convolution of Lenna is as expected', {
  expect_equal(sum(laplacian), 0)
  expect_false(is.complex(laplacian))
  convolved_image <- imager::convolve(imager::as.cimg(lenna), imager::as.cimg(laplacian), dirichlet = F)
  vdiffr::expect_doppelganger(
    'Lenna Laplacian',
    function() plot(convolved_image, axes = FALSE)
  )
})
