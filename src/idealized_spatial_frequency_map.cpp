#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
ListMatrix idealized_spatial_frequency_map_cpp(int nrows, int ncols) {
  // Create a matrix to hold spatial frequencies
  ListMatrix spatial_frequencies(nrows, ncols);

  // Loop over rows and columns to calculate spatial frequencies
  for (int i = 0; i < nrows; ++i) {
    int y_value = (i > nrows / 2) ? i - nrows : i; // Compute y value
    for (int j = 0; j < ncols; ++j) {
      int x_value = (j > ncols / 2) ? j - ncols : j; // Compute x value

      // Assign a named vector to each cell
      spatial_frequencies(i, j) = NumericVector::create(
        Named("x") = x_value,
        Named("y") = y_value
      );
    }
  }

  return spatial_frequencies;
}
