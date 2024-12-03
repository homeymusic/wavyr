#include <Rcpp.h>
#include <tuple>
#include "stern_brocot.h"

using namespace Rcpp;

 inline bool is_close(double a, double b, double tol = 1e-9) {
   return std::abs(a - b) <= tol;
 }

 //' _approximate_rational_fractions_cpp
 //'
 //' Approximates floating-point numbers to arbitrary uncertainty.
 //'
 //' @param x Vector of floating point numbers to approximate
 //' @param uncertainty Precision for finding rational fractions
 //' @param deviation Deviation for estimating least common multiples
 //' @param metadata Optional data frame with metadata (must have the same number of rows as x)
 //'
 //' @return A data frame of rational numbers and metadata
 //'
 //' @export
 // [[Rcpp::export]]
 DataFrame approximate_rational_fractions_cpp(NumericVector x,
                                              const double uncertainty,
                                              const double deviation,
                                              Rcpp::Nullable<DataFrame> metadata = R_NilValue) {
   if (deviation <= uncertainty) {
     stop("Deviation must be greater than uncertainty.");
   }

   const int n = x.size();

   // Validate input values for x
   for (int i = 0; i < n; ++i) {
     if (x[i] <= 0.0) {
       stop("Input vector x contains non-positive values, which are invalid for logarithms.");
     }
   }

   // Handle optional metadata and perform early validation
   DataFrame meta;
   if (metadata.isNotNull()) {
     meta = metadata.get();

     // Check that metadata has the same number of rows as x
     if (meta.nrows() != n) {
       stop("Metadata must have the same number of rows as the input vector x.");
     }
   }

   // Vectors to store the results
   NumericVector approximations(n);
   NumericVector errors(n);
   NumericVector nums(n);
   NumericVector dens(n);

   for (int i = 0; i < n; ++i) {
     DataFrame sb_result = stern_brocot_cpp(x[i], uncertainty);

     // Extract numerator and denominator
     nums[i] = (int)sb_result["num"];
     dens[i] = (int)sb_result["den"];

     if (dens[i] == 0) {
       stop("Denominator cannot be zero in rational fraction approximation.");
     }

     const double sb_x = (double)sb_result["original_value"];
     if (!is_close(sb_x, x[i])) {
       stop("Original values disagree. SB x: " + std::to_string(sb_x) + " x: " + std::to_string(x[i]));
     }

     approximations[i] = (double)sb_result["approximation"];
     errors[i] = (double)sb_result["error"];
   }

   // Create the main result DataFrame
   DataFrame result = DataFrame::create(
     _("idealized_x")    = x,
     _("rationalized_x") = approximations,
     _("num")            = nums,
     _("den")            = dens,
     _("error")          = errors,
     _("uncertainty")    = uncertainty
   );

   // Append metadata if provided
   if (metadata.isNotNull()) {
     CharacterVector meta_names = meta.names(); // Extract column names from metadata
     for (int i = 0; i < meta.size(); ++i) {
       result.push_back(meta[i], std::string(meta_names[i])); // Explicitly convert meta_names[i] to std::string
     }
   }

   return result;
 }
