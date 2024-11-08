% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waveform.R
\name{waveform}
\alias{waveform}
\title{Create a general waveform}
\usage{
waveform(frequency_spectrum, wavelength_spectrum = NULL, phase = 0)
}
\arguments{
\item{frequency_spectrum}{An object of class "frequency_spectrum" containing frequencies and amplitudes.}

\item{wavelength_spectrum}{Optional: An object of class "wavelength_spectrum" for custom wavelength components.}

\item{phase}{Optional: A numeric value representing the phase of the waveform.}
}
\value{
An object of class "waveform" containing the frequency spectrum, wavelength spectrum, phase, and indexed_spectra.
}
\description{
Create a general waveform
}
