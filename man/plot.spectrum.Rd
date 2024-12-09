% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectrum.R
\name{plot.spectrum}
\alias{plot.spectrum}
\title{Core plotting function for spectrum objects}
\usage{
\method{plot}{spectrum}(
  x,
  x_label,
  segment_color,
  rectangles = numeric(0),
  title = NULL,
  overlay_spectrum = NULL,
  overlay_spectrum_color = NULL
)
}
\arguments{
\item{x}{An object of class "spectrum".}

\item{x_label}{A string label for the x-axis, provided by the subclass.}

\item{segment_color}{A color for the spectrum segments, provided by the subclass.}

\item{rectangles}{Optional: A numeric vector specifying positions for additional rectangles.}

\item{title}{An optional character string for the plot title.}

\item{overlay_spectrum}{An optional overlay spectrum object of class "spectrum".}

\item{overlay_spectrum_color}{A color for the overlay spectrum segments. Required if overlay_spectrum is provided.}
}
\description{
Creates a spike plot based on subclass-specific labels and colors.
Allows for an optional overlay spectrum with a separate color.
}
