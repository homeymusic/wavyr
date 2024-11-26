num_harmonics = 3

test_that('time harmonics plot',{
  label = paste(num_harmonics, 'Time Harmonics Beats')

  f = 1:num_harmonics * 60 %>% midi_to_freq()
  a = 1 / (1:num_harmonics)
  w = frequency_spectrum(
    idealized_frequency = f,
    amplitude = a
  ) %>% superposed_wave()
  vdiffr::expect_doppelganger(label, function() plot(w, label = label))
})

test_that('time harmonics plot',{
  label = paste(num_harmonics, 'Time Harmonics')
  f = 1:num_harmonics * 60 %>% midi_to_freq()
  a = 1/1:num_harmonics
  w = frequency_spectrum(
    idealized_frequency = f,
    amplitude = a
  ) %>% wave()
  vdiffr::expect_doppelganger(label, function() plot(w, label = label))
})

test_that('space harmonics plot',{
  label = paste(num_harmonics, 'Space Harmonics Beats')
  f = SPEED_OF_SOUND / 1:num_harmonics
  a = 1/1:num_harmonics
  w = frequency_spectrum(
    idealized_frequency = f,
    amplitude = a
  ) %>% superposed_wave()
  vdiffr::expect_doppelganger(label, function() plot(w, label = label))
})

test_that('space harmonics plot',{
  label = paste(num_harmonics, 'Space Harmonics')
  f = SPEED_OF_SOUND / 1:num_harmonics
  a = 1/1:num_harmonics
  w = frequency_spectrum(
    idealized_frequency = f,
    amplitude = a
  ) %>% wave()
  vdiffr::expect_doppelganger(label, function() plot(w, label = label))
})
