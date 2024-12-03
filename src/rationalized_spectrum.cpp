#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
ComplexMatrix rationalized_spectrum_cpp(
    const ComplexMatrix& idealized_spectrum,
    const List& rationalized_spatial_frequencies
) {
  int nrows = idealized_spectrum.nrow();
  int ncols = idealized_spectrum.ncol();

  // Initialize the rationalized spectrum with zeros
  ComplexMatrix rationalized_spectrum(nrows, ncols);

  // Loop through the spectrum
  for (int i = 0; i < nrows; ++i) {
    for (int j = 0; j < ncols; ++j) {
      // Get the rationalized coordinates for this cell
      NumericVector coords = rationalized_spatial_frequencies(i, j);
      int x_prime = coords["x"];
      int y_prime = coords["y"];

      // Map the rationalized coordinates back to matrix indices
      int target_row = (y_prime + nrows) % nrows;
      int target_col = (x_prime + ncols) % ncols;

      // Aggregate the value from the idealized spectrum
      rationalized_spectrum(target_row, target_col) =
        rationalized_spectrum(target_row, target_col) + idealized_spectrum(i, j);
    }
  }

  return rationalized_spectrum;
}
