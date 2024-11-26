#include <Rcpp.h>
#include <tuple>

using namespace Rcpp;

 //' Calculate Beats from Components
 //'
 //' Generate beats from two sets of components (extent or rate) and return their results.
 //'
 //' @param component NumericVector of components (e.g., wavelength, frequency)
 //' @param amplitude NumericVector of amplitudes
 //' @param extent_rate Character string, either "extent" or "rate", specifying the component type
 //' @param tolerance Numeric, the minimum difference to consider for beats
 //'
 //' @return A DataFrame containing the spectrum, components, and amplitudes of the beats.
 //' @export
 // [[Rcpp::export]]
 DataFrame compute_beats_cpp(NumericVector component,
                             NumericVector amplitude,
                             std::string extent_rate,
                             double tolerance = 1e-6) {

   const int n = component.size();

   if (n < 2) {
     return DataFrame::create(
       _("component") = NumericVector::create(),
       _("amplitude") = NumericVector::create()
     );
   }

   // Validate the extent_rate parameter
   if (extent_rate != "extent" && extent_rate != "rate") {
     stop("Invalid extent_rate. Must be 'extent' or 'rate'.");
   }

   // Vectors to hold the results
   NumericVector beat_component(n * (n - 1) / 2); // Max number of pairs
   NumericVector beat_amplitude(n * (n - 1) / 2);

   int count = 0;

   // Calculate the beats
   for (int i = 0; i < n; i++) {
     for (int j = i + 1; j < n; j++) {
       if (std::abs(component[i] - component[j]) > tolerance) {
         // Compute the beat component based on extent_rate
         double computed_component;
         if (extent_rate == "extent") {
           computed_component = (component[i] * component[j]) / std::abs(component[i] - component[j]);
         } else { // extent_rate == "rate"
           computed_component = std::abs(component[j] - component[i]);
         }

         beat_component[count] = computed_component;

         // Compute the beat amplitude
         beat_amplitude[count] = amplitude[i] + amplitude[j];
         count++;
       }
     }
   }

   if (count < 1) {
     return DataFrame::create(
       _("component") = NumericVector::create(),
       _("amplitude") = NumericVector::create()
     );
   } else {
     // Create the resulting DataFrame
     return DataFrame::create(
       _("component") = beat_component[Rcpp::Range(0, count - 1)],
                                      _("amplitude") = beat_amplitude[Rcpp::Range(0, count - 1)]
     );
   }
 }
