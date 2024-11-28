test_that("Media object has correct class and stores the original media content", {
  # Path to the test image file using testthat::test_path
  image_file_path <- test_path("images",
                               "Maureen Ma Dukes Yancy with Axes of J Dilla from Smithsonian.png")

  # Load the expected media content
  expected_signal <- imager::load.image(image_file_path)

  # Create a media object
  media_obj <- media(image_file_path)

  # Check that the object is of class "media"
  expect_s3_class(media_obj, "media")

  # Check that the original_signal contains the actual media content
  expect_equal(media_obj$original_signal, expected_signal)
})
