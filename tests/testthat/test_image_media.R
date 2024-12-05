source(testthat::test_path("helper.R"))

test_that("Image_media object has correct class, stores the original media content, computes idealized_spectrum, idealized_signal, and verifies dimensions", {

  image_file_path <- test_path("images", "MaDukes.png")

  expected_original <- imager::load.image(image_file_path)
  expected_original_dim <- dim(expected_original)
  expected_grayscale_image = imager::grayscale(expected_original)
  expected_grayscale_image_matrix = as.matrix(expected_grayscale_image)
  expected_idealized_spectrum <- fftwtools::fftw2d(expected_grayscale_image_matrix)
  expect_equal(class(expected_idealized_spectrum), c('matrix','array'))
  element <- expected_idealized_spectrum[100,100]
  expect_equal(class(element), c('complex'))
  expect_equal(Re(element), 3.45, tolerance=0.1)
  expect_equal(Im(element), -7.32, tolerance=0.1)
  expect_equal(Mod(element), 8.10, tolerance=0.1)
  expect_equal(Arg(element), -1.13, tolerance=0.1)
  expect_equal(Conj(element), 3.454608+7.328465i, tolerance=0.1)
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

  label = 'MaDukes Idealized Spectrum'
  vdiffr::expect_doppelganger(label, plot_matrix(image_media_obj$idealized_spectrum))
  label = 'MaDukes Rationalized Spectrum'
  vdiffr::expect_doppelganger(label, plot_matrix(image_media_obj$rationalized_spectrum))

  expect_equal(image_media_obj$idealized_dimensions, expected_idealized_dim)
  expect_equal(image_media_obj$idealized_signal, expected_idealized_signal)
  expect_equal(image_media_obj$idealized_image, expected_idealized_image)

  label = 'MaDukes_Idealized'
  vdiffr::expect_doppelganger(label, function() plot(image_media_obj$idealized_image,
                                                     axes = F))

  label = 'MaDukes_Rationalized'
  vdiffr::expect_doppelganger(label, function() plot(image_media_obj$rationalized_image,
                                                     axes = F))

})

test_that("Image_media object has correct class, stores the original media content, computes idealized_spectrum, idealized_signal, and verifies dimensions", {

  image_file_path <- test_path("images", "Lenna.png")

  expected_original <- imager::load.image(image_file_path)
  expected_original_dim <- dim(expected_original)
  expected_grayscale_image = imager::grayscale(expected_original)
  expected_grayscale_image_matrix = as.matrix(expected_grayscale_image)
  expected_idealized_spectrum <- fftwtools::fftw2d(expected_grayscale_image_matrix)
  expect_equal(class(expected_idealized_spectrum), c('matrix','array'))
  element <- expected_idealized_spectrum[100,100]
  expect_equal(class(element), c('complex'))
  expect_equal(Re(element), -32.30, tolerance=0.1)
  expect_equal(Im(element), -2.33, tolerance=0.1)
  expect_equal(Mod(element), 32.39, tolerance=0.1)
  expect_equal(Arg(element), -3.06, tolerance=0.1)
  expect_equal(Conj(element), -32.30888+2.33377i, tolerance=0.1)
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

  label = 'Lenna Idealized Spectrum'
  vdiffr::expect_doppelganger(label, plot_matrix(image_media_obj$idealized_spectrum))
  label = 'Lenna Rationalized Spectrum'
  vdiffr::expect_doppelganger(label, plot_matrix(image_media_obj$rationalized_spectrum))

  expect_equal(image_media_obj$idealized_dimensions, expected_idealized_dim)
  expect_equal(image_media_obj$idealized_signal, expected_idealized_signal)
  expect_equal(image_media_obj$idealized_image, expected_idealized_image)

  label = 'Lenna_Idealized'
  vdiffr::expect_doppelganger(label, function() plot(image_media_obj$idealized_image,
                                                     axes = F))

  label = 'Lenna_Rationalized'
  vdiffr::expect_doppelganger(label, function() plot(image_media_obj$rationalized_image,
                                                     axes = F))
})

test_that("Image_media object creates Gabor-filtered images with verified parameters", {

  image_filenames = c('Lenna', 'MaDukes')
  orientations <- c(0, pi/4, pi/2, 3*pi/4)  # 0°, 45°, 90°, 135° in radians

  for (image_filename in image_filenames) {
    image_file_path <- test_path("images", paste0(image_filename, ".png"))
    image_media_obj <- image_media(image_file_path)
    # Test each orientation
    for (orientation in orientations) {
      label <- paste0(image_filename, "-Gabor-Filters-", orientation * 180 / pi)
      vdiffr::expect_doppelganger(label, function() {
        plot(image_media_obj$gabor_filtered_image(orientation), axes = FALSE)
      })
    }
  }

})

test_that("the various maps for a 5x5 matrix make sense", {
  image_filename = "MaDukesRightEye"
  image_file_path <- test_path("images", paste0(image_filename, ".png"))
  image_media_obj <- image_media(image_file_path)

  expected_idealized_spectrum <- matrix(
    c(
      6.1361961 + 0.0000000i, 0.1477849 + 1.0480768i, -0.08568688 + 0.05459868i, -0.08568688 - 0.05459868i, 0.1477849 - 1.0480768i,
      0.7944703 + 0.5699178i, 0.5525445 - 0.2506504i,  0.12778788 - 0.01742626i,  0.29475756 + 0.21481316i, 0.0622026 - 0.4496432i,
      0.1696866 + 0.0538504i, 0.3160660 + 0.0995171i, -0.01142689 - 0.11058409i,  0.06430717 + 0.17928514i, 0.1871533 - 0.2141558i,
      0.1696866 - 0.0538504i, 0.1871533 + 0.2141558i,  0.06430717 - 0.17928514i, -0.01142689 + 0.11058409i, 0.3160660 - 0.0995171i,
      0.7944703 - 0.5699178i, 0.0622026 + 0.4496432i,  0.29475756 - 0.21481316i,  0.12778788 + 0.01742626i, 0.5525445 + 0.2506504i
    ),
    nrow = 5,
    byrow = TRUE
  )

  expect_equal(image_media_obj$idealized_spectrum, expected_idealized_spectrum, tolerance=0.01)

  label = 'MaDukesRightEye Idealized Spectrum'
  vdiffr::expect_doppelganger(label, plot_matrix(image_media_obj$idealized_spectrum))

  expected_rationalized_spectrum <- matrix(
    c(
                               6.1361961 + 0.0000000i, 0.1477849 + 1.0480768i + -0.08568688 + 0.05459868i,                    0 + 0i,                    0 + 0i, 0.1477849 - 1.0480768i + -0.08568688 - 0.05459868i,
      0.7944703 + 0.5699178i + 0.1696866 + 0.0538504i, 0.5525445 - 0.2506504i + -0.01142689 - 0.11058409i,  0.12778788 - 0.01742626i,  0.29475756 + 0.21481316i, 0.0622026 - 0.4496432i +  0.06430717 + 0.17928514i,
                                               0 + 0i,                             0.3160660 + 0.0995171i,                    0 + 0i,                    0 + 0i,                           0.1871533  - 0.2141558i,
                                               0 + 0i,                             0.1871533 + 0.2141558i,                    0 + 0i,                    0 + 0i,                           0.3160660  - 0.0995171i,
      0.7944703 - 0.5699178i + 0.1696866 - 0.0538504i, 0.0622026 + 0.4496432i +  0.06430717 - 0.17928514i,  0.29475756 - 0.21481316i,  0.12778788 + 0.01742626i, 0.5525445 + 0.2506504i + -0.01142689 + 0.11058409i
    ),
    nrow = 5,
    byrow = TRUE
  )
  expect_equal(image_media_obj$rationalized_spectrum, expected_rationalized_spectrum,
               tolerance=0.01)

  label = 'MaDukesRightEye Rationalized Spectrum'
  vdiffr::expect_doppelganger(label, plot_matrix(image_media_obj$rationalized_spectrum))

  vdiffr::expect_doppelganger(paste(image_filename, 'idealized'), function() {
    plot(image_media_obj$idealized_image, axes = FALSE)
  })

  vdiffr::expect_doppelganger(paste(image_filename, 'rationalized'), function() {
    plot(image_media_obj$rationalized_image, axes = FALSE)
  })

})
