test_that('time harmonics plot',{
  label = '11 Time Harmonics Beats'
  s = 60 %>% hrep::sparse_fr_spectrum(num_harmonics = 11)
  w = frequency_spectrum(
    frequency = s$x,
    amplitude = s$y
  ) %>% superposed_wave()
  vdiffr::expect_doppelganger(label, function() plot(w, label = label))
})

test_that('time harmonics plot',{
  label = '11 Time Harmonics'
  s = 60 %>% hrep::sparse_fr_spectrum(num_harmonics = 11)
  w = frequency_spectrum(
    frequency = s$x,
    amplitude = s$y
  ) %>% wave()
  vdiffr::expect_doppelganger(label, function() plot(w, label = label))
})

test_that('space harmonics plot',{
  label = '11 Space Harmonics Beats'
  f = SPEED_OF_SOUND / 1:11 # meters
  a = (60 %>% hrep::sparse_fr_spectrum(num_harmonics = 11))$y # just to get the same amplitude roll off
  w = frequency_spectrum(
    frequency = f,
    amplitude = a
  ) %>% superposed_wave()
  vdiffr::expect_doppelganger(label, function() plot(w, label = label))
})

test_that('space harmonics plot',{
  label = '11 Space Harmonics'
  f = SPEED_OF_SOUND / 1:11 # meters
  a = (60 %>% hrep::sparse_fr_spectrum(num_harmonics = 11))$y # just to get the same amplitude roll off
  w = frequency_spectrum(
    frequency = f,
    amplitude = a
  ) %>% wave()
  vdiffr::expect_doppelganger(label, function() plot(w, label = label))
})
