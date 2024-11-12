source(testthat::test_path("test_utils.R"))
test_that("cohenerce and modulation metrics for ionian and phrygian triads", {
  ionian   = waveform_for(framed_triads$ionian, num_harmonics = 2)
  phrygian = waveform_for(framed_triads$phrygian, num_harmonics = 2)

  label = "Ionian Triad"
  vdiffr::expect_doppelganger(label, function() plot(ionian, label = label))
  label = "Phrygian Triad"
  vdiffr::expect_doppelganger(label, function() plot(phrygian, label = label))

})
