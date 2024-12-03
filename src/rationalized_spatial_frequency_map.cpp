#include <Rcpp.h>
#include "stern_brocot.h"  // Include the header file for stern_brocot_cpp
using namespace Rcpp;

// [[Rcpp::export]]
ListMatrix rationalized_spatial_frequency_map_cpp(const ListMatrix& spatial_frequencies, double uncertainty) {
  int nrows = spatial_frequencies.nrow();
  int ncols = spatial_frequencies.ncol();

  // Initialize the output ListMatrix
  ListMatrix rationalized_frequencies(nrows, ncols);

  for (int i = 0; i < nrows; ++i) {
    for (int j = 0; j < ncols; ++j) {
      // Extract x and y
      NumericVector k = spatial_frequencies(i, j);
      double x = k["x"];
      double y = k["y"];

      if (x == 0 && y == 0) {
        // Handle zero values: copy original spatial frequencies
        rationalized_frequencies(i, j) = k;
      } else if (x == 0) {
        // Set x = 0 and y = +1 or -1 based on sign of y
        NumericVector rationalized = NumericVector::create(
          Named("x") = 0,
          Named("y") = std::copysign(1, y)
        );
        rationalized_frequencies(i, j) = rationalized;
      } else if (y == 0) {
        // Set y = 0 and x = +1 or -1 based on sign of x
        NumericVector rationalized = NumericVector::create(
          Named("x") = std::copysign(1, x),
          Named("y") = 0
        );
        rationalized_frequencies(i, j) = rationalized;
      } else {
        // Compute the absolute ratio
        double abs_ratio = std::abs(y / x);

        // Call Stern-Brocot function
        DataFrame sb_result = stern_brocot_cpp(abs_ratio, uncertainty);

        // Extract numerator and denominator from DataFrame
        NumericVector num_vec = sb_result["num"];
        NumericVector den_vec = sb_result["den"];
        int num = num_vec[0];
        int den = den_vec[0];

        // Reapply signs
        NumericVector rationalized = NumericVector::create(
          Named("x") = std::copysign(den, x),
          Named("y") = std::copysign(num, y)
        );

        // Assign to output matrix
        rationalized_frequencies(i, j) = rationalized;
      }
    }
  }

  return rationalized_frequencies;
}
