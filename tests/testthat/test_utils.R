intervals <- list(
  # consonant intervals
  Major3 = 64,
  minor6 = 68,
  Major6 = 69,
  minor3 = 63,
  # perfect intervals
  Perfect5 = 67,
  Perfect4 = 65,
  Perfect1 = 60,
  Perfect8 = 72,
  # dissonant intervals
  tritone = 66,
  Major7 = 71,
  minor2 = 61,
  Major2 = 62,
  minor7 = 70
)
framed_intervals <- purrr::map(intervals, ~c(60, .x, 72))

waveform_for <- function(x, num_harmonics = 1) {
  sparse = x %>% hrep::sparse_fr_spectrum(num_harmonics=num_harmonics)
  frequency_spectrum(
    frequency = sparse$x,
    amplitude = sparse$y
  ) %>% waveform()
}

