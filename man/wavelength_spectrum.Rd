% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wavelength_spectrum.R
\name{wavelength_spectrum}
\alias{wavelength_spectrum}
\title{Create a Wavelength Spectrum}
\usage{
wavelength_spectrum(wavelength, amplitude = NULL)
}
\arguments{
\item{wavelength}{Either a numeric vector of wavelengths or a list with named \code{wavelength} and \code{amplitude} vectors.}

\item{amplitude}{A numeric vector of amplitudes, if \code{wavelength} is a numeric vector.}
}
\value{
An object of class \code{wavelength_spectrum} that inherits from \code{spectrum}.
}
\description{
Extends the \code{spectrum} class specifically for wavelength spectra.
Accepts either direct numeric vectors or a list containing \code{wavelength} and \code{amplitude}.
}
