#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
ComplexMatrix rationalized_spectrum_cpp(const ComplexMatrix& source) {
  // Generate Q_map using a helper function
  Function fourier_transform_2D_map("fourier_transform_2D_map");
  DataFrame Q_map = fourier_transform_2D_map(source.nrow(), source.ncol());

  // Extract columns from Q_map
  IntegerVector x = Q_map["x"];
  IntegerVector y = Q_map["y"];
  IntegerVector rationalized_x = Q_map["rationalized_x"];
  IntegerVector rationalized_y = Q_map["rationalized_y"];
  IntegerVector idealized_x = Q_map["idealized_x"];
  IntegerVector idealized_y = Q_map["idealized_y"];

  // Initialize the rationalized matrix
  ComplexMatrix rationalized_matrix(source.nrow(), source.ncol());

  // Iterate over each row of Q_map
  for (int i = 0; i < x.size(); i++) {
    int source_x = x[i];
    int source_y = y[i];

    // Fetch the source value
    Rcomplex source_value = source(source_y - 1, source_x - 1);

    // Get rationalized_x and rationalized_y
    int target_rationalized_x = rationalized_x[i];
    int target_rationalized_y = rationalized_y[i];

    // Find the target row in Q_map
    int target_row_index = -1;
    for (int j = 0; j < idealized_x.size(); j++) {
      if (idealized_x[j] == target_rationalized_x &&
          idealized_y[j] == target_rationalized_y) {
        target_row_index = j;
        break;
      }
    }

    // Verify that the target row exists and is unique
    if (target_row_index == -1) {
      stop("Error: Target row not uniquely identified.");
    }

    // Get the target cell in the rationalized_matrix
    int target_x = x[target_row_index];
    int target_y = y[target_row_index];

    // Add the source value to the rationalized_matrix at the target cell
    rationalized_matrix(target_y - 1, target_x - 1).r += source_value.r; // Add real part
    rationalized_matrix(target_y - 1, target_x - 1).i += source_value.i; // Add imaginary part
  }

  return rationalized_matrix;
}
