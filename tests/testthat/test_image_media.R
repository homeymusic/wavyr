test_that("Image_media object has correct class, stores the original media content, computes idealized_spectrum, idealized_signal, and verifies dimensions", {

  image_file_path <- test_path("images",
                               "Maureen Ma Dukes Yancy with Axes of J Dilla from Smithsonian.png")

  expected_original <- imager::load.image(image_file_path)
  expected_original_dim <- dim(expected_original)
  expected_grayscale_image = imager::grayscale(expected_original)
  expected_grayscale_image_matrix = as.matrix(expected_grayscale_image)
  expected_idealized_spectrum <- fftwtools::fftw2d(expected_grayscale_image_matrix)
  expect_equal(class(expected_idealized_spectrum), c('matrix','array'))
  element <- expected_idealized_spectrum[100,100]
  expect_equal(class(element), c('complex'))
  expect_equal(Re(element), 97.228, tolerance=0.1)
  expect_equal(Im(element), 49.62, tolerance=0.1)
  expect_equal(Mod(element), 109.162, tolerance=0.1)
  expect_equal(Arg(element), 0.4719509, tolerance=0.1)
  expect_equal(Conj(element), 97.22871-49.62774i , tolerance=0.1)
  expected_idealized_dim <- expected_original_dim
  expected_idealized_dim[length(expected_idealized_dim)] <- 1
  expected_idealized_signal <- fftwtools::fftw2d(expected_idealized_spectrum, inverse = 1)
  expected_idealized_image <- imager::as.cimg(Re(expected_idealized_signal),
                                              dim = expected_idealized_dim)



  image_media_obj <- image_media(image_file_path)

  expect_s3_class(image_media_obj, "image_media")
  expect_equal(image_media_obj$original_image, expected_original)
  expect_equal(image_media_obj$original_dimensions, expected_original_dim)
  expect_equal(image_media_obj$idealized_spectrum, expected_idealized_spectrum)
  expect_equal(image_media_obj$idealized_dimensions, expected_idealized_dim)
  expect_equal(image_media_obj$idealized_signal, expected_idealized_signal)
  expect_equal(image_media_obj$idealized_image, expected_idealized_image)
  label = 'idealized image'
  vdiffr::expect_doppelganger(label, function() plot(image_media_obj$idealized_image,
                                                     axes = F))

})
