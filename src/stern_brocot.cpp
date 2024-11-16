#include <Rcpp.h>
#include <tuple>
#include "stern_brocot.h"
#include "information_metrics.h"

using namespace Rcpp;

// Helper function to create the DataFrame with all columns and calculated metrics
DataFrame create_stern_brocot_df(
    const double x,
    const int num,
    const int den,
    const double uncertainty,
    const std::vector<int>& path
) {
  // Calculate metrics inside the DataFrame creation
  double shannon_entropy = shannon_entropy_cpp(path);  // Calculate Shannon entropy
  int hamming_weight = hamming_weight_cpp(path);    // Calculate Hamming weight
  double run_length_encoding = run_length_encoding_cpp(path);  // Calculate run length encoding
  int path_id = as_integer_cpp(path);  // Derive path_id from path

  return DataFrame::create(
    _["original_value"] = x,
    _["num"] = num,
    _["den"] = den,
    _["uncertainty"] = uncertainty,
    _["depth"] = path.size(),  // Depth corresponds to the length of the path
    _["path"] = as_string_cpp(path),  // Convert path vector to string
    _["path_id"] = path_id,
    _["shannon_entropy"] = shannon_entropy,
    _["hamming_weight"] = hamming_weight,
    _["run_length_encoding"] = run_length_encoding
  );
}

 //' stern_brocot_cpp
 //'
 //' Approximates a floating-point number to arbitrary uncertainty.
 //'
 //' @param x Number to convert to rational fraction
 //' @param uncertainty Binary search stops once the desired uncertainty is reached
 //'
 //' @return A data frame with columns: `original_value`, `num`, `den`, and `uncertainty`
 //' @export
 // [[Rcpp::export]]
 DataFrame stern_brocot_cpp(const double x, const double uncertainty) {
   if (x <= 0) stop("STOP: x must be greater than 0");
   if (uncertainty <= 0) stop("STOP: uncertainty must be greater than 0");

   int cycles = 0;

   double approximation;

   // Create the path as a vector of integers, starting with 0
   std::vector<int> path = {0};  // Path starts with 0

   const double valid_min = x - uncertainty;
   const double valid_max = x + uncertainty;

   int left_num = floor(x), left_den = 1;
   int mediant_num = round(x), mediant_den = 1;
   int right_num = floor(x) + 1, right_den = 1;

   approximation = (double) mediant_num / mediant_den;
   const int insane = 1000;

   // Special case: x <= uncertainty
   if (x <= uncertainty) {
     cycles = 1;  // Set depth as cycles

     int num = 1;
     int den = 1;

     // Handle cases depending on the values of x and uncertainty
     if (uncertainty < 1) {
       num = 1;  // Approximation for num
       den = static_cast<int>(1 / uncertainty);  // Denominator based on uncertainty
     } else if (uncertainty > 1 && x <= 1) {
       num = static_cast<int>(uncertainty);  // Approximation for num
       den = 1;  // Denominator is 1
     } else {
       num = static_cast<int>(x);  // Approximation for num
       den = 1;  // Denominator is 1
     }

     // Set path
     path.push_back(0);  // Path ID starts at 0

     // Return the DataFrame with the estimated values, metrics calculated inside the function
     return create_stern_brocot_df(
       x, num, den, uncertainty, path
     );
   }

   // Main computation loop for Stern-Brocot
   while (((approximation < valid_min) || (valid_max < approximation)) && cycles < insane) {
     double x0 = 2 * x - approximation;

     if (approximation < valid_min) {
       left_num = mediant_num; left_den = mediant_den;
       int k = floor((right_num - x0 * right_den) / (x0 * left_den - left_num));
       right_num += k * left_num; right_den += k * left_den;

     } else if (valid_max < approximation) {
       right_num = mediant_num; right_den = mediant_den;
       int k = floor((x0 * left_den - left_num) / (right_num - x0 * right_den));
       left_num += k * right_num; left_den += k * right_den;
     }

     mediant_num = left_num + right_num;
     mediant_den = left_den + right_den;
     approximation = (double) mediant_num / mediant_den;
     cycles++;
   }

   if (mediant_num <= 0) stop("STOP: mediant_num is less than or equal to zero");
   if (mediant_den <= 0) stop("STOP: mediant_den is less than or equal to zero");

   // Return the DataFrame with the estimated values and metrics calculated
   return create_stern_brocot_df(
     x, mediant_num, mediant_den, uncertainty, path
   );
 }
