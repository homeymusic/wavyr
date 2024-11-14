source(testthat::test_path("helper.R"))
test_that("cohenerce and modulation metrics for ionian and phrygian triads", {
  ionian   = wave_for(framed_triads$ionian, num_harmonics = 2)
  phrygian = wave_for(framed_triads$phrygian, num_harmonics = 2)

  label = "Framed Ionian Triad"
  vdiffr::expect_doppelganger(label, function() plot(ionian, label = label))
  label = "Framed Phrygian Triad"
  vdiffr::expect_doppelganger(label, function() plot(phrygian, label = label))

})
