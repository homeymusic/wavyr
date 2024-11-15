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

triads <- list(
  # consonant intervals
  ionian   =  60 + c(4,7),
  phrygian =  72 - c(4,7)
)
framed_triads <- purrr::map(triads, ~c(60, .x, 72) %>% sort())

spectrum_for <- function(x, num_harmonics = 1, roll_off_dB = 1) {

  purrr::map2(midi_to_freq(x), rep(1, length(x)),
              function(freq, amp) {
                n  <- seq_len(num_harmonics)
                df <- data.frame(
                  frequency = freq * n,
                  amplitude = 1 * 10 ^ ( -roll_off_dB * log2(n) / 20)
                )
                df
              }) %>%
    dplyr::bind_rows() %>%
    frequency_spectrum()
}

wave_for <- function(x, num_harmonics = 1, roll_off_dB = 1) {
    spectrum_for(x, num_harmonics = 1, roll_off_dB = 1) %>%
    wave()
}

