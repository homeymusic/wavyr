#include <Rcpp.h>
#include <tuple>
#include "stern_brocot.h"

using namespace Rcpp;

 //' compute_pseudo_octave
 //'
 //' Find the highest fundamental freq
 //'
 //' @param fn freq to eval
 //' @param f0 fundamental freq
 //' @param n  harmonic number
 //'
 //' @return Calculated pseudo octave
 const double compute_pseudo_octave(const double fn, const double f0, const int n) {
   if (n==1) {
     return 1.0;
   } else {
     const int r = 1000000;
     return std::round(r * pow(2, log(fn / f0) / log(n))) / r;
   }
 }

 //' approximate_harmonics
 //'
 //' Determine pseudo octave of all frequencies relative to lowest frequency
 //'
 //' @param x Chord frequencies
 //' @param deviation Deviation for estimating least common multiples
 //'
 //' @return A double of the best guess of the pseudo octave
 DataFrame approximate_harmonics(const NumericVector x,
                                 const double deviation) {
   const int x_size   = x.size();
   const double f_max = max(x);
   NumericVector harmonic_number(x_size * x_size * x_size);
   NumericVector evaluation_freq(x_size * x_size * x_size);
   NumericVector reference_freq(x_size * x_size * x_size);
   NumericVector reference_amp(x_size * x_size * x_size);
   NumericVector pseudo_octave(x_size * x_size * x_size);
   NumericVector highest_freq(x_size * x_size * x_size);


   const DataFrame default_pseudo_octave = DataFrame::create(
     _("harmonic_number") = 1,
     _("evaluation_freq") = f_max,
     _("reference_freq")  = f_max,
     _("pseudo_octave")   = 2.0,
     _("highest_freq")    = f_max
   );

   if (x_size <= 2) {
     return default_pseudo_octave;
   }

   int num_matches=0;

   for (int eval_freq_index = 0; eval_freq_index < x_size; ++eval_freq_index) {
     for (int ref_freq_index = 0; ref_freq_index < x_size; ++ref_freq_index) {
       for (int harmonic_num = 2; harmonic_num <= x_size; ++harmonic_num) {
         const double p_octave = compute_pseudo_octave(x[eval_freq_index], x[ref_freq_index], harmonic_num);
         if (2.0 - deviation < p_octave && p_octave < 2.0 + deviation) {
           harmonic_number[num_matches] = harmonic_num;
           evaluation_freq[num_matches] = x[eval_freq_index];
           reference_freq[num_matches]  = x[ref_freq_index];
           highest_freq[num_matches]    = f_max;
           pseudo_octave[num_matches]   = p_octave;
           num_matches++;
         }
       }
     }
   }

   if (num_matches == 0) {
     return default_pseudo_octave;
   } else {
     return DataFrame::create(
       _("harmonic_number") = harmonic_number[Rcpp::Range(0, num_matches-1)],
                                             _("evaluation_freq") = evaluation_freq[Rcpp::Range(0, num_matches-1)],
                                                                                   _("reference_freq")  = reference_freq[Rcpp::Range(0, num_matches-1)],
                                                                                                                        _("pseudo_octave")   = pseudo_octave[Rcpp::Range(0, num_matches-1)],
                                                                                                                                                            _("highest_freq")    = highest_freq[Rcpp::Range(0, num_matches-1)]
     );
   }
 }

 //' pseudo_octave
 //'
 //' Finds the pseudo octave from approximate harmonics.
 //'
 //' @param approximate_harmonics List of candidate pseudo octaves
 //'
 //' @return A data frame of rational numbers and metadata
 const double pseudo_octave(NumericVector approximate_harmonics) {
   const IntegerVector counts = table(approximate_harmonics);
   IntegerVector idx = seq_along(counts) - 1;
   std::sort(idx.begin(), idx.end(), [&](int i, int j){return counts[i] > counts[j];});
   CharacterVector names_of_count = counts.names();
   names_of_count = names_of_count[idx];
   return std::stod(std::string(names_of_count[0]));
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

   // Handle optional metadata and perform early validation
   DataFrame meta;
   if (metadata.isNotNull()) {
     meta = metadata.get();

     // Check that metadata has the same number of rows as x
     if (meta.nrows() != n) {
       stop("Metadata must have the same number of rows as the input vector x.");
     }
   }

   // Precompute pseudo octave and ensure it's valid
   const DataFrame approximate_harmonics_df = approximate_harmonics(x, deviation);
   const double pseudo_octave_double = pseudo_octave(approximate_harmonics_df["pseudo_octave"]);

   if (pseudo_octave_double <= 1) {
     stop("Pseudo octave must be greater than 1. The deviation value is likely too large.");
   }

   // Vectors to store the results
   NumericVector pseudo_x(n);
   NumericVector approximations(n);
   NumericVector errors(n);
   NumericVector nums(n);
   NumericVector dens(n);

   for (int i = 0; i < n; ++i) {
     pseudo_x[i] = pow(2.0, log(x[i]) / log(pseudo_octave_double));

     // Call updated stern_brocot_cpp to get DataFrame output
     DataFrame sb_result = stern_brocot_cpp(pseudo_x[i], uncertainty);

     // Extract numerator and denominator
     nums[i] = sb_result["num"];
     dens[i] = sb_result["den"];

     approximations[i] = nums[i] / dens[i];
     errors[i] = approximations[i] - pseudo_x[i];
   }

   // Create the main result DataFrame
   DataFrame result = DataFrame::create(
     _("x")             = x,
     _("rationalized_x")    = approximations,
     _("pseudo_x")      = pseudo_x,
     _("pseudo_octave") = pseudo_octave_double,
     _("num")           = nums,
     _("den")           = dens,
     _("error")         = errors,
     _("uncertainty")   = uncertainty
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
