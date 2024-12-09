% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/signal.R
\name{plot.signal}
\alias{plot.signal}
\title{Plot method for signal objects}
\usage{
\method{plot}{signal}(
  x,
  title = "",
  coordinate_range = NULL,
  number_of_cycles = NULL,
  resolution = 99
)
}
\arguments{
\item{x}{A signal object.}

\item{coordinate_range}{A numeric vector of length 2 specifying the start and stop values for the coordinate range.}

\item{number_of_cycles}{A numeric value specifying the number of cycles to plot. Defaults to 3 if not provided.}

\item{resolution}{Number of points to sample within the range.}

\item{label}{An optional label for the plot.}
}
\description{
Creates a plot of the signal's amplitude over a specified coordinate range or a number of cycles.
}
