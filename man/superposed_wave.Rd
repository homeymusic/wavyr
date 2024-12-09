% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/superposed_wave.R
\name{superposed_wave}
\alias{superposed_wave}
\title{Create a Linearwave for a linear medium}
\usage{
superposed_wave(frequency_spectrum, wavelength_spectrum = NULL, phase = 0)
}
\arguments{
\item{frequency_spectrum}{An object of class "frequency_spectrum" containing frequencies and amplitudes.}

\item{wavelength_spectrum}{Optional: An object of class "wavelength_spectrum" for custom wavelength components.}

\item{phase}{Optional: A numeric value representing the phase of the wave.}
}
\value{
An object of class "superposed_wave" with combined wavelength and frequency spectra, and beat spectrum.
}
\description{
Create a Linearwave for a linear medium
}
