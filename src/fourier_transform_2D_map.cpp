#include <Rcpp.h>
#include "stern_brocot.h"  // Include the header file for stern_brocot_cpp
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame fourier_transform_2D_map_cpp(int nrows, int ncols, double uncertainty) {
  // Initialize vectors for tibble columns
  std::vector<int> x_values, y_values;
  std::vector<int> idealized_x_values, idealized_y_values;
  std::vector<int> rationalized_x_values, rationalized_y_values;

  // Loop through all rows and columns
  for (int i = 0; i < ncols; ++i) {
    for (int j = 0; j < nrows; ++j) {
      // Calculate spatial frequencies idealized_x and idealized_y
      int idealized_x = (i > ncols / 2) ? i - ncols : i;
      int idealized_y = (j > nrows / 2) ? j - nrows : j;

      // Initialize rationalized frequencies
      int rationalized_x = 0;
      int rationalized_y = 0;

      // Rationalize idealized_x and idealized_y using Stern-Brocot
      if (idealized_x == 0 && idealized_y == 0) {
        // Handle the DC component (0 frequency)
        rationalized_x = 0;
        rationalized_y = 0;
      } else if (idealized_x == 0) {
        // Special case for idealized_x = 0
        rationalized_x = 0;
        rationalized_y = std::copysign(1, idealized_y);
      } else if (idealized_y == 0) {
        // Special case for idealized_y = 0
        rationalized_x = std::copysign(1, idealized_x);
        rationalized_y = 0;
      } else {
        // Compute the absolute ratio and rationalize it
        double abs_ratio = std::abs(static_cast<double>(idealized_y) / idealized_x);
        DataFrame sb_result = stern_brocot_cpp(abs_ratio, uncertainty);

        // Extract numerator and denominator from Stern-Brocot result
        NumericVector num_vec = sb_result["num"];
        NumericVector den_vec = sb_result["den"];
        int num = num_vec[0];
        int den = den_vec[0];

        // Reapply signs
        rationalized_x = std::copysign(den, idealized_x);
        rationalized_y = std::copysign(num, idealized_y);
      }

      // Store values in vectors
      x_values.push_back(i + 1); // Convert to R's 1-based indexing
      y_values.push_back(j + 1); // Convert to R's 1-based indexing
      idealized_x_values.push_back(idealized_x);
      idealized_y_values.push_back(idealized_y);
      rationalized_x_values.push_back(rationalized_x);
      rationalized_y_values.push_back(rationalized_y);
    }
  }

  // Create and return the tibble
  return DataFrame::create(
    _["x"] = x_values,
    _["y"] = y_values,
    _["idealized_x"] = idealized_x_values,
    _["idealized_y"] = idealized_y_values,
    _["rationalized_x"] = rationalized_x_values,
    _["rationalized_y"] = rationalized_y_values
  );
}
