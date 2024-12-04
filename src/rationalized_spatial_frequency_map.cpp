#include <Rcpp.h>
#include "stern_brocot.h"  // Include the header file for stern_brocot_cpp
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame rationalized_spatial_frequency_map_cpp(int nrows, int ncols, double uncertainty) {
  // Initialize vectors for tibble columns
  std::vector<int> x_values, y_values;
  std::vector<int> k_x_values, k_y_values;
  std::vector<int> k_Q_x_values, k_Q_y_values;

  // Loop through all rows and columns
  for (int i = 0; i < nrows; ++i) {
    for (int j = 0; j < ncols; ++j) {
      // Calculate spatial frequencies k_x and k_y
      int k_y = (i > nrows / 2) ? i - nrows : i;
      int k_x = (j > ncols / 2) ? j - ncols : j;

      // Initialize rationalized frequencies
      int k_Q_x = 0;
      int k_Q_y = 0;

      // Rationalize k_x and k_y using Stern-Brocot
      if (k_x == 0 && k_y == 0) {
        // Handle the DC component (0 frequency)
        k_Q_x = 0;
        k_Q_y = 0;
      } else if (k_x == 0) {
        // Special case for k_x = 0
        k_Q_x = 0;
        k_Q_y = std::copysign(1, k_y);
      } else if (k_y == 0) {
        // Special case for k_y = 0
        k_Q_x = std::copysign(1, k_x);
        k_Q_y = 0;
      } else {
        // Compute the absolute ratio and rationalize it
        double abs_ratio = std::abs(static_cast<double>(k_y) / k_x);
        DataFrame sb_result = stern_brocot_cpp(abs_ratio, uncertainty);

        // Extract numerator and denominator from Stern-Brocot result
        NumericVector num_vec = sb_result["num"];
        NumericVector den_vec = sb_result["den"];
        int num = num_vec[0];
        int den = den_vec[0];

        // Reapply signs
        k_Q_x = std::copysign(den, k_x);
        k_Q_y = std::copysign(num, k_y);
      }

      // Store values in vectors
      x_values.push_back(j + 1); // Convert to R's 1-based indexing
      y_values.push_back(i + 1); // Convert to R's 1-based indexing
      k_x_values.push_back(k_x);
      k_y_values.push_back(k_y);
      k_Q_x_values.push_back(k_Q_x);
      k_Q_y_values.push_back(k_Q_y);
    }
  }

  // Create and return the tibble
  return DataFrame::create(
    _["x"] = x_values,
    _["y"] = y_values,
    _["k_x"] = k_x_values,
    _["k_y"] = k_y_values,
    _["k_Q_x"] = k_Q_x_values,
    _["k_Q_y"] = k_Q_y_values
  );
}
