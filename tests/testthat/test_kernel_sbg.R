source(testthat::test_path("helper.R"))

test_dense_spectrum_makes_sense <- function(d, label) {
  expected_kernel <- matrix(
    c(0, 1, 0, 1, 0,
      1, 2, 2, 2, 1,
      0, 2, 1, 2, 0,
      1, 2, 2, 2, 1,
      0, 1, 0, 1, 0),
    nrow = 5, ncol = 5, byrow = TRUE
  )

  expect_equal(d, expected_kernel, tolerance=0.01)
  expect_equal(class(d), c("matrix", "array"))
}

test_sparse_spectrum_makes_sense <- function(s, label) {
  expect_equal(class(s), "data.frame")
  expect_named(s, c('x', 'y', 'idealized_x', 'idealized_y', 'idealized_angle',
                    'rationalized_x', 'rationalized_y', 'rationalized_angle',
                    'original_value', 'num', 'den', 'approximation', 'error',
                    'uncertainty', 'depth', 'path', 'path_id',
                    'shannon_entropy', 'run_length_encoding', 'hamming_weight'))
  expect_equal(s$error, rep(0,25))
  expect_equal(s %>% dplyr::distinct(x, y) %>%  nrow(), 25)
  expect_equal(s %>% dplyr::distinct(idealized_x, idealized_y) %>%  nrow(), 25)
  expect_equal(s %>% dplyr::distinct(rationalized_x, rationalized_y) %>%  nrow(), 17)

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

      # Extract the corresponding row from s
      Q_cell <- s[s$x == x_r & s$y == y_r, ]

      # Ensure exactly one match
      expect_equal(nrow(Q_cell), 1)

      # Validate idealized_x and idealized_y
      expected <- expected_frequencies[[i, j]]
      expect_equal(unname(Q_cell$idealized_x), expected[1], label = paste("Mismatch at (", x_r, ",", y_r, ") for idealized_x"))
      expect_equal(unname(Q_cell$idealized_y), expected[2], label = paste("Mismatch at (", x_r, ",", y_r, ") for idealized_y"))
    }
  }

}

test_that("a 5x5 rationalized matrix from cpp makes sense", {
  s = sparse_spectrum_sbg_cpp(5, 5, GABOR_UNCERTAINTY ^ 2)
  test_sparse_spectrum_makes_sense(
    s, 'sbg sparse spectrum from cpp directly 5 x 5'
  )
})

test_that("a 5x5 rationalized matrix from data file makes sense", {
  s = sparse_spectrum_sbg(5, GABOR_UNCERTAINTY ^ 2)
  test_sparse_spectrum_makes_sense(
    s, 'sbg sparse spectrum from data file 5 x 5'
  )
})

test_that("a 5x5 kernel matrix from data file makes sense", {
  d = kernel_sbg(5, GABOR_UNCERTAINTY ^ 2)
  test_dense_spectrum_makes_sense(
    d, 'sbg dense spectrum from data file 5 x 5'
  )
})


test_image_convolution <- function(label, grayscale_matrix, kernel) {
  response <- convolution_with_kernel(grayscale_matrix, kernel)
  filtered_image <- imager::as.cimg(Mod(response), dim = dim(grayscale_matrix))
  vdiffr::expect_doppelganger(
    label,
    function() plot(filtered_image, axes = FALSE)
  )
  thresholded_img <- imager::threshold(filtered_image, thr = "80%")
  vdiffr::expect_doppelganger(
    paste('thresholded', label),
    function() plot(thresholded_img, axes = FALSE)
  )

}

negate_matrix_except_center <- function(mat) {
  # Ensure the matrix is square and has odd dimensions
  if (nrow(mat) != ncol(mat)) {
    stop("The matrix must be square.")
  }
  if (nrow(mat) %% 2 == 0) {
    stop("The matrix dimensions must be odd.")
  }

  # Find the center index
  center_index <- (nrow(mat) + 1) / 2

  # Negate the entire matrix
  negated_mat <- -mat

  # Restore the center cell
  negated_mat[center_index, center_index] <- mat[center_index, center_index]

  return(negated_mat)
}

apply_angle_filter <- function(fourier_matrix, angle, uncertainty=(GABOR_UNCERTAINTY^2)) {

  # Get matrix dimensions and ensure it is square
  rows <- nrow(fourier_matrix)
  cols <- ncol(fourier_matrix)
  if (rows != cols) {
    stop("The Fourier matrix must be square.")
  }
  if (rows %% 2 == 0) {
    stop("The Fourier matrix must have odd dimensions.")
  }

  # Center index
  center_index <- (rows + 1) / 2

  # Special cases: angles where tangent is undefined
  if (abs(angle - pi / 2) < 1e-6) {
    # Vertical line (upward)
    rationalized_x <- 0
    rationalized_y <- 1
  } else if (abs(angle + pi / 2) < 1e-6) {
    # Vertical line (downward)
    rationalized_x <- 0
    rationalized_y <- -1
  } else {
    # General case: compute slope from the angle
    slope <- tan(angle)

    # Use Stern-Brocot tree to find a rational fraction approximation

    if (slope == 0) {
      num <- 0
      den <- 1
    } else {
      sb_result <- stern_brocot_cpp(abs(slope), uncertainty)
      num <- sb_result$num[1]  # Numerator
      den <- sb_result$den[1]  # Denominator
    }


    # Reapply signs based on the angle's quadrant
    if (angle >= 0 && angle < pi / 2) {
      # First quadrant: x > 0, y > 0
      rationalized_x <- den
      rationalized_y <- num
    } else if (angle >= pi / 2 && angle < pi) {
      # Second quadrant: x < 0, y > 0
      rationalized_x <- -den
      rationalized_y <- num
    } else if (angle >= -pi && angle < -pi / 2) {
      # Third quadrant: x < 0, y < 0
      rationalized_x <- -den
      rationalized_y <- -num
    } else if (angle >= -pi / 2 && angle < 0) {
      # Fourth quadrant: x > 0, y < 0
      rationalized_x <- den
      rationalized_y <- -num
    } else {
      stop("Angle is out of bounds.")
    }
  }

  # Map the rational fraction to matrix indices
  selected_row <- center_index + rationalized_y
  selected_col <- center_index + rationalized_x

  # Ensure indices are within bounds
  if (selected_row < 1 || selected_row > rows || selected_col < 1 || selected_col > cols) {
    stop("Selected rational fraction is out of bounds for the matrix size.")
  }

  # Create a modified Fourier spectrum
  result_matrix <- matrix(0, nrow = rows, ncol = cols)  # Start with all zeros
  result_matrix[center_index, center_index] <- fourier_matrix[center_index, center_index]  # Center stays as is
  result_matrix[selected_row, selected_col] <- -fourier_matrix[selected_row, selected_col]  # Selected cell negated

  return(result_matrix)
}

lenna    <- (load_and_preprocess_image(test_path("images", "Lenna.png")))$grayscale_matrix
ma_dukes <- (load_and_preprocess_image(test_path("images", "MPC3000JDilla.png")))$grayscale_matrix

test_kernel_spectrum_plot <- function(length) {
  uncertainty = GABOR_UNCERTAINTY ^ 2
  label = paste0("Spectrum ", length, "x", length)
  test_that(paste0("a ", length, "x", length, " plot 2D spectrum"), {
    d <- kernel_sbg(length, uncertainty, SIGNAL_OR_SPECTRUM$spectrum)
    vdiffr::expect_doppelganger(paste("Kernel", label), function() plot_matrix(d, fft_shift = F))
    test_image_convolution(paste("Lenna", label), lenna, d)
    test_image_convolution(paste("MPC3000JDilla", label), ma_dukes, d)
  })
}

test_kernel_signal_plot <- function(length) {
  uncertainty = GABOR_UNCERTAINTY ^ 2
  label = paste0("Signal ", length, "x", length)
  test_that(paste0("a ", length, "x", length, " plot 2D signal"), {
    d <- kernel_sbg(length, uncertainty, SIGNAL_OR_SPECTRUM$signal) %>% negate_matrix_except_center()
    vdiffr::expect_doppelganger(paste("Kernel", label), function() plot_matrix(d, fft_shift = T))
    test_image_convolution(paste("Lenna", label), lenna, d)
    test_image_convolution(paste("MPC3000JDilla", label), ma_dukes, d)
  })
}

test_angles_kernel_signal_plot <- function(angle) {
  length = 51
  uncertainty = GABOR_UNCERTAINTY ^ 2
  label = paste0("Angle Signal ", angle/pi)
  test_that(paste0("angle ", angle/ pi), {
    spectrum <- kernel_sbg(length, uncertainty, SIGNAL_OR_SPECTRUM$spectrum) %>% apply_angle_filter(angle)
    d <-  fftwtools::fftw2d(spectrum, inverse = 1)
    vdiffr::expect_doppelganger(paste("Kernel", label), function() plot_matrix(d, fft_shift = T))
    test_image_convolution(paste("Lenna", label), lenna, d)
    test_image_convolution(paste("MPC3000JDilla", label), ma_dukes, d)
  })
}

test_sizes = seq(5, 11, by = 2)
lapply(test_sizes, test_kernel_spectrum_plot)
lapply(test_sizes, test_kernel_signal_plot)

test_angles = c(0,1/4,1/2,3/4) * pi
lapply(test_angles, test_angles_kernel_signal_plot)

test_error_histogram <- function(length) {
  uncertainty = GABOR_UNCERTAINTY ^ 2
  test_that(paste0("a ", length, "x", length, " error histogram makes sense"), {
    s <- sparse_spectrum_sbg(length, uncertainty)
    vdiffr::expect_doppelganger(
      paste0("Error Histogram ", length, "x", length),
      function() plot_error_histogram(s$error)
    )
  })
}
lapply(test_sizes, test_error_histogram)

test_error_uncertainty_histogram <- function(uncertainty) {
  length = 129
  test_that(paste0("a ", length, "x", length, "uncertainty ", sprintf("%.4f", uncertainty),
                   " error histogram makes sense"), {
    s <- sparse_spectrum_sbg(length, uncertainty)
    vdiffr::expect_doppelganger(
      paste0("Error Uncertainty Histogram ", sprintf("%.5f", uncertainty)),
      function() plot_error_histogram(s$error)
    )
  })
}
test_uncertainties = c(GABOR_UNCERTAINTY, GABOR_UNCERTAINTY ^ 2, 2 * 10^seq(-5, 3, by = 1)) %>% sort()
lapply(test_uncertainties, test_error_uncertainty_histogram)


test_that('angles make sense', {
  uncertainty = GABOR_UNCERTAINTY ^ 2

  s <- sparse_spectrum_sbg(5, uncertainty)
  expect_equal(s$idealized_angle, s$rationalized_angle)

  s <- sparse_spectrum_sbg(27, uncertainty)
  expect_equal(s$idealized_angle, s$rationalized_angle)
  rationalized <- s %>%
    dplyr::group_by(rationalized_x, rationalized_y) %>%
    dplyr::summarize(count = dplyr::n(), .groups = "drop")
  idealized <- s %>%
    dplyr::group_by(idealized_x, idealized_y) %>%
    dplyr::summarize(count = dplyr::n(), .groups = "drop")
  expect_lt(nrow(rationalized), nrow(idealized))

  s <- sparse_spectrum_sbg(29, uncertainty)
  s_diff = (abs(s$idealized_angle -s$rationalized_angle))
  s_diff = (round(s_diff[s_diff != 0] * 1e6)/1e6) %>% unique() %>% sort()
  expect_equal(s_diff, c(0.003, 0.005), tolerance = 0.01)
  rationalized <- s %>%
    dplyr::group_by(rationalized_x, rationalized_y) %>%
    dplyr::summarize(count = dplyr::n(), .groups = "drop")

  idealized <- s %>%
    dplyr::group_by(idealized_x, idealized_y) %>%
    dplyr::summarize(count = dplyr::n(), .groups = "drop")

  expect_lt(nrow(rationalized), nrow(idealized))

})

normalize_matrix_to_zero_corners <- function(mat) {
  s = mat %>% fft_shift()
  round((s[1,1] - s) * 1e10) / 1e10
}

test_that("a 9x9 kernel relates to a Laplacian", {
  k = kernel_sbg(9, GABOR_UNCERTAINTY ^ 2, SIGNAL_OR_SPECTRUM$signal) %>%
    Mod() %>% normalize_matrix_to_zero_corners()

  vdiffr::expect_doppelganger(
    paste('Laplacian Like 9x9'),
    function() persp(z=k, theta = 30, phi = 30, ticktype = "detailed")
  )

})

test_that("a 51x51 kernel relates to a Laplacian", {
  k = kernel_sbg(51, GABOR_UNCERTAINTY ^ 2, SIGNAL_OR_SPECTRUM$signal) %>%
    Mod() %>% normalize_matrix_to_zero_corners()

  vdiffr::expect_doppelganger(
    paste('Laplacian Like 51x51'),
    function() persp(
      z=k,
      theta = 30,
      phi = 30,
      ticktype = "detailed"
    )
  )

})
