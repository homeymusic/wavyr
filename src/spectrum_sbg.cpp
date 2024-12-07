#include <Rcpp.h>
#include "stern_brocot.h"  // Include the header file for stern_brocot_cpp
using namespace Rcpp;

// Helper function to add placeholders for Stern-Brocot outputs
void add_placeholders_sbg(
    std::vector<double>& original_values,
    std::vector<int>& nums,
    std::vector<int>& dens,
    std::vector<double>& approximations,
    std::vector<double>& errors,
    std::vector<double>& uncertainties,
    std::vector<int>& depths,
    std::vector<std::string>& paths,
    std::vector<int>& path_ids,
    std::vector<double>& shannon_entropies,
    std::vector<double>& run_length_encodings,
    std::vector<int>& hamming_weights
) {
  double placeholder_double = 0.0;
  int placeholder_int = 0;
  const std::string& placeholder_string = "";

  original_values.push_back(placeholder_double);
  nums.push_back(placeholder_int);
  dens.push_back(placeholder_int);
  approximations.push_back(placeholder_double);
  errors.push_back(placeholder_double);
  uncertainties.push_back(placeholder_double);
  depths.push_back(placeholder_int);
  paths.push_back(placeholder_string);
  path_ids.push_back(placeholder_int);
  shannon_entropies.push_back(placeholder_double);
  run_length_encodings.push_back(placeholder_double);
  hamming_weights.push_back(placeholder_int);
}

// Helper function to process Stern-Brocot results
void process_stern_brocot_results_sbg(
    const DataFrame& sb_result,
    double abs_ratio,
    int& rationalized_x,
    int& rationalized_y,
    std::vector<double>& original_values,
    std::vector<int>& nums,
    std::vector<int>& dens,
    std::vector<double>& approximations,
    std::vector<double>& errors,
    std::vector<double>& uncertainties,
    std::vector<int>& depths,
    std::vector<std::string>& paths,
    std::vector<int>& path_ids,
    std::vector<double>& shannon_entropies,
    std::vector<double>& run_length_encodings,
    std::vector<int>& hamming_weights,
    int idealized_x,
    int idealized_y
) {
  NumericVector num_vec = sb_result["num"];
  NumericVector den_vec = sb_result["den"];
  NumericVector approx_vec = sb_result["approximation"];
  NumericVector error_vec = sb_result["error"];
  NumericVector unc_vec = sb_result["uncertainty"];
  IntegerVector depth_vec = sb_result["depth"];
  CharacterVector path_vec = sb_result["path"];
  IntegerVector path_id_vec = sb_result["path_id"];
  NumericVector shannon_vec = sb_result["shannon_entropy"];
  NumericVector rle_vec = sb_result["run_length_encoding"];
  IntegerVector hamming_vec = sb_result["hamming_weight"];

  int num = num_vec[0];
  int den = den_vec[0];

  // Reapply signs
  rationalized_x = std::copysign(den, idealized_x);
  rationalized_y = std::copysign(num, idealized_y);

  // Store Stern-Brocot outputs
  original_values.push_back(abs_ratio);
  nums.push_back(num);
  dens.push_back(den);
  approximations.push_back(approx_vec[0]);
  errors.push_back(error_vec[0]);
  uncertainties.push_back(unc_vec[0]);
  depths.push_back(depth_vec[0]);
  paths.push_back(as<std::string>(path_vec[0]));
  path_ids.push_back(path_id_vec[0]);
  shannon_entropies.push_back(shannon_vec[0]);
  run_length_encodings.push_back(rle_vec[0]);
  hamming_weights.push_back(hamming_vec[0]);
}

// [[Rcpp::export]]
DataFrame spectrum_sbg_cpp(int nrows, int ncols, double uncertainty) {
  // Initialize vectors for tibble columns
  std::vector<int> x_values, y_values;
  std::vector<int> idealized_x_values, idealized_y_values;
  std::vector<int> rationalized_x_values, rationalized_y_values;

  // Initialize vectors for Stern-Brocot outputs
  std::vector<double> original_values, approximations, errors, uncertainties;
  std::vector<int> nums, dens, depths, path_ids;
  std::vector<std::string> paths;
  std::vector<double> shannon_entropies, run_length_encodings;
  std::vector<int> hamming_weights;

  // Loop through all rows and columns
  for (int i = 0; i < ncols; ++i) {
    for (int j = 0; j < nrows; ++j) {
      // Calculate spatial frequencies idealized_x and idealized_y
      int idealized_x = (i > ncols / 2) ? i - ncols : i;
      int idealized_y = (j > nrows / 2) ? j - nrows : j;

      // Initialize rationalized frequencies
      int rationalized_x = 0;
      int rationalized_y = 0;

      if (idealized_x == 0 && idealized_y == 0) {
        // Handle the DC component (0 frequency)
        rationalized_x = 0;
        rationalized_y = 0;

        add_placeholders_sbg(
          original_values, nums, dens, approximations, errors, uncertainties,
          depths, paths, path_ids, shannon_entropies, run_length_encodings,
          hamming_weights
        );
      } else if (idealized_x == 0) {
        rationalized_x = 0;
        rationalized_y = std::copysign(1, idealized_y);
        add_placeholders_sbg(
          original_values, nums, dens, approximations, errors, uncertainties,
          depths, paths, path_ids, shannon_entropies, run_length_encodings,
          hamming_weights
        );
      } else if (idealized_y == 0) {
        rationalized_x = std::copysign(1, idealized_x);
        rationalized_y = 0;
        add_placeholders_sbg(
          original_values, nums, dens, approximations, errors, uncertainties,
          depths, paths, path_ids, shannon_entropies, run_length_encodings,
          hamming_weights
        );
      } else {

        // Rcpp::Rcout
        // << "idealized_y: "
        // << idealized_y
        // << ", idealized_x: "
        // << idealized_x << std::endl;
        // Compute the absolute ratio and rationalize it
        double abs_ratio = std::abs(static_cast<double>(idealized_y) / idealized_x);
        DataFrame sb_result = stern_brocot_cpp(abs_ratio, uncertainty);
        process_stern_brocot_results_sbg(
          sb_result, abs_ratio, rationalized_x, rationalized_y, original_values,
          nums, dens, approximations, errors, uncertainties, depths, paths,
          path_ids, shannon_entropies, run_length_encodings, hamming_weights,
          idealized_x, idealized_y
        );
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
    _["rationalized_y"] = rationalized_y_values,
    _["original_value"] = original_values,
    _["num"] = nums,
    _["den"] = dens,
    _["approximation"] = approximations,
    _["error"] = errors,
    _["uncertainty"] = uncertainties,
    _["depth"] = depths,
    _["path"] = paths,
    _["path_id"] = path_ids,
    _["shannon_entropy"] = shannon_entropies,
    _["run_length_encoding"] = run_length_encodings,
    _["hamming_weight"] = hamming_weights
  );
}
