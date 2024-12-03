#' Least Common Multiple of Integer Vector
#'
#' Computes the least common multiple of a vector of integers.
#'
#' @param x A numeric vector of integers.
#' @return A single numeric value representing the least common multiple.
#' @export
lcm_integers <- function(x) {
  Reduce(gmp::lcm.bigz, x) %>% as.numeric()
}

#' @export
midi_to_freq <- function(midi) {
  440 * 2^((midi - 69) / 12)
}

# we want a speed of sound such that one of the MIDI note's fundamental frequency
# will give us a wavelength of 1 m.
# The frequency of F4 (65) is closest to 343 Hz.
# 343 m/s is commonly used as the speed of sound.
#' @export
SPEED_OF_SOUND <- midi_to_freq(65)

#' @export
DEFAULT_SPEED_OF_MEDIUM <- SPEED_OF_SOUND

#' @export
FLOATING_POINT_TOLERANCE <- 1e-6

#' @export
GABOR_UNCERTAINTY <- 1 / (4 * pi)

# Define colors and helper functions
colors_homey <- list(
  'background'        = '#664433',
  'highlight'         = '#C18160',
  'foreground'        = '#291B14',
  'subtle_foreground' = '#7F745A',
  'minor'             = '#8AC5FF',
  'minor_dark'        = '#6894BF',
  'neutral'           = '#F3DDAB',
  'major'             = '#FFB000',
  'major_dark'        = '#BF8400',
  'light_neutral'     = '#FFF6E2',
  'fundamental'       = '#FF5500',
  'beat'              = '#77B255',
  'gray'              = '#C0C0C0'
)

saturation_colors_homey <- list(
  major = list(
    hi = '#FF9700',
    lo = '#FFE0B2'
  ),
  neutral = list(
    hi = '#FF5500',
    lo = '#FFCCB2'
  ),
  minor = list(
    hi = '#0084EC',
    lo = '#A5CDEC'
  )
)

color_factor_homey <- function(x, column_name) {
  cut(x[[column_name]], c(-Inf, -1e-6, 1e-6, Inf), labels = c("minor", "neutral", "major"))
}

color_values_homey <- function() {
  c("minor" = colors_homey$minor,
    "neutral" = colors_homey$fundamental,
    "major" = colors_homey$major,
    "behavioral" = colors_homey$neutral)
}

space_time_colors <- function() {
  c("space" = colors_homey$minor,
    "time" = colors_homey$major,
    "behavioral" = colors_homey$neutral)
}

theme_homey <- function(aspect.ratio = NULL, font = "Helvetica", panel_background_fill = colors_homey$background) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = colors_homey$foreground, face = "bold", size = ggplot2::rel(0.8)),
      plot.subtitle = ggplot2::element_text(color = colors_homey$foreground, size = ggplot2::rel(0.75)),
      axis.title = ggplot2::element_text(color = colors_homey$foreground, size = ggplot2::rel(0.7)),
      axis.text = ggplot2::element_text(color = colors_homey$foreground, size = ggplot2::rel(0.6)),
      axis.ticks = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = colors_homey$neutral, color = colors_homey$foreground),
      panel.background = ggplot2::element_rect(fill = panel_background_fill),
      panel.grid.major = ggplot2::element_line(color = colors_homey$foreground, linewidth = 0.2),
      panel.grid.minor = ggplot2::element_line(color = colors_homey$foreground, linewidth = 0.05, linetype = "dashed"),
      legend.background = ggplot2::element_rect(fill = colors_homey$light_neutral),
      legend.key = ggplot2::element_rect(fill = colors_homey$background, color = NA),
      legend.position = "right",  # Position the legend to the right of the plot
      legend.direction = "vertical",  # Set legend orientation to vertical
      legend.title = ggplot2::element_text(color = colors_homey$foreground, size = ggplot2::rel(0.7)),
      legend.text = ggplot2::element_text(color = colors_homey$foreground, size = ggplot2::rel(0.6)),
      aspect.ratio = aspect.ratio
    )
}
