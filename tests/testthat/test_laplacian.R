source(testthat::test_path("helper.R"))

lenna <- (load_and_preprocess_image(test_path("images", "Lenna.png")))$grayscale_matrix

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

test_that('3D plot of laplacian', {
  vdiffr::expect_doppelganger(
    'Laplacian 3D',
    function() persp(z=laplacian, theta = 30, phi = 30, ticktype = "detailed")
  )
})

test_that('the laplacian convolution of Lenna is as expected', {
  expect_equal(sum(laplacian), 0)
  expect_false(is.complex(laplacian))
  expect_equal(min(laplacian),-40)
  expect_equal(max(laplacian),5)
  convolved_image <- imager::convolve(imager::as.cimg(lenna), imager::as.cimg(laplacian), dirichlet = F)
  vdiffr::expect_doppelganger(
    'Lenna Laplacian',
    function() plot(convolved_image, axes = FALSE)
  )
})

size = dim(laplacian)[1]
sbg_kernel = kernel_sbg(size, signal_or_spectrum = SIGNAL_OR_SPECTRUM$signal) %>% Mod() %>% fft_shift()
sbg_kernel = sum(sbg_kernel) / size^2 - sbg_kernel

test_that('3D plot of SBG', {
  vdiffr::expect_doppelganger(
    'SBG 3D',
    function() persp(z=sbg_kernel, theta = 30, phi = 30, ticktype = "detailed")
  )
})

test_that('the SBG convolution of Lenna is as expected', {
  expect_equal(sum(sbg_kernel), 0)
  expect_false(is.complex(sbg_kernel))
  expect_equal(min(sbg_kernel),-72.57, tolerance = 0.1)
  expect_equal(max(sbg_kernel),7.242813, tolerance = 0.1)
  convolved_image <- imager::convolve(imager::as.cimg(lenna), imager::as.cimg(sbg_kernel), dirichlet = F)
  vdiffr::expect_doppelganger(
    'Lenna SBG',
    function() plot(convolved_image, axes = FALSE)
  )
})

test_that('the diff of the images from Laplacian convolution and SBG convolution of Lenna', {
  laplacian_image <- imager::convolve(imager::as.cimg(lenna), imager::as.cimg(laplacian), dirichlet = F)
  sbg_image       <- imager::convolve(imager::as.cimg(lenna), imager::as.cimg(sbg_kernel), dirichlet = F)
  vdiffr::expect_doppelganger(
    'Lenna diff betwen SBG and Laplacian Images',
    function() plot(abs(sbg_image - laplacian_image), axes = FALSE)
  )
})

test_that('the diff of the Laplacian the SBG kernel convolved with Lenna', {
  blended_image <- imager::convolve(imager::as.cimg(lenna), imager::as.cimg(sbg_kernel - laplacian), dirichlet = F)
  vdiffr::expect_doppelganger(
    'Lenna diff betwen SBG and Laplacian Kernels',
    function() plot(blended_image, axes = FALSE)
  )
})

size = 35
sbg_kernel = kernel_sbg(size, signal_or_spectrum = SIGNAL_OR_SPECTRUM$signal) %>% Mod() %>% fft_shift()
sbg_kernel = sum(sbg_kernel) / size^2 - sbg_kernel

test_that('3D plot of 35x35 SBG Kernel Signal', {
  expect_equal(sum(sbg_kernel), 0)
  expect_false(is.complex(sbg_kernel))
  vdiffr::expect_doppelganger(
    'SBG 2D Signal',
    function() plot_matrix(sbg_kernel, fft_shift = F, magnitude = F, log_scaling = F)
  )
  vdiffr::expect_doppelganger(
    'SBG 3D Signal from Above',
    function() persp(z=sbg_kernel, theta = 30, phi = 30, ticktype = "detailed")
  )
  vdiffr::expect_doppelganger(
    'SBG 3D Signal from Below',
    function() persp(z=sbg_kernel, theta = 30, phi = -30, ticktype = "detailed")
  )
})

size = 35
sbg_kernel = kernel_sbg(size, signal_or_spectrum = SIGNAL_OR_SPECTRUM$spectrum) %>% Mod()

test_that('3D plot of 35x35 SBG Kernel Spectrum', {
  expect_false(is.complex(sbg_kernel))
  vdiffr::expect_doppelganger(
    'SBG 2D Spectrum',
    function() plot_matrix(sbg_kernel, fft_shift = F, magnitude = F, log_scaling = F)
  )
  vdiffr::expect_doppelganger(
    'SBG 3D Spectrum from Above',
    function() persp(z=sbg_kernel, theta = 30, phi = 30, ticktype = "detailed")
  )
  vdiffr::expect_doppelganger(
    'SBG 3D Spectrum from Below',
    function() persp(z=sbg_kernel, theta = 30, phi = -30, ticktype = "detailed")
  )
})
