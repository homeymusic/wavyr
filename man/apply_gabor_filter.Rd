% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gabor_kernel.R
\name{apply_gabor_filter}
\alias{apply_gabor_filter}
\title{Apply Gabor Filter}
\usage{
apply_gabor_filter(grayscale_matrix, orientation, f, kernel_size)
}
\arguments{
\item{grayscale_matrix}{A matrix representation of the grayscale image.}

\item{orientation}{Orientation of the Gabor filter in radians.}

\item{f}{Frequency of the sinusoidal wave.}

\item{kernel_size}{Size of the Gabor kernel.}
}
\value{
A filtered image as a \code{cimg} object.
}
\description{
Applies a Gabor filter to a grayscale image matrix.
}
