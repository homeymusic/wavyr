test_that('an n increases the probablity fns from SB make sense',{
  resolution = 100000
  max_quantum_number = 30
  locations = seq(from=1/resolution, to=2-1/resolution, by=1/resolution)
  for (n in 0:max_quantum_number) {
    # TODO: because the std dev represents a ratio of
    # x_coh / x_amp we should scale it back up along
    # the x-axis when we plot these
    delta_x_coherence = 1
    amplitude = 2 * (n+1) + 1
    uncertainty = delta_x_coherence / amplitude
    fractions <- data_rds({
      locations %>%
        purrr::map(\(l) {
          approximate_rational_fractions_cpp(l, uncertainty, uncertainty+0.1)
        }, .progress=F) %>% purrr::list_rbind()
    }, filename = paste0("sb_errors_quantum_number_", n, ".rds"))

    vdiffr::expect_doppelganger(
      paste0("Quantum Number ", n, " Uncertainty ", sprintf("%.4f", uncertainty)),
      function() plot_error_histogram(fractions$error,
                                      ceiling(2*length(fractions$error)^(1/3)))
    )
  }
})
