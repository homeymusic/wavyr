#include <Rcpp.h>
#include <tuple>

using namespace Rcpp;

 //' Calculate Beats from Frequencies
 //'
 //' Generate beats from two sets of frequencies and return their wavelength and amplitude.
 //'
 //' @param wavelength NumericVector of wavelengths
 //' @param amplitude NumericVector of amplitudes
 //'
 //' @return A DataFrame containing the spectrum, wavelengths, and amplitudes of the beats.
 //' @export
 // [[Rcpp::export]]
 DataFrame compute_beats_cpp(NumericVector wavelength,
                             NumericVector amplitude,
                             double tolerance = 1e-6) {

   const int n = wavelength.size();

   if (n < 2) {
     return DataFrame::create(
       _("wavelength") = NumericVector::create(),
       _("amplitude")  = NumericVector::create()
     );
   }

   // Vectors to hold the results
   NumericVector beat_wavelength(n * (n - 1) / 2); // Max number of pairs
   NumericVector beat_amplitude(n * (n - 1) / 2);

   int count = 0;

   // Calculate the beats
   for (int i = 0; i < n; i++) {
     for (int j = i + 1; j < n; j++) {
       if (std::abs(wavelength[i] - wavelength[j]) > tolerance) {
         // Compute the raw beat wavelength
         double computed_wavelength = (wavelength[i] * wavelength[j]) / std::abs(wavelength[i] - wavelength[j]);
         beat_wavelength[count] = computed_wavelength;
         // Compute the beat amplitude
         beat_amplitude[count] = amplitude[i] + amplitude[j];
         count++;
       }
     }
   }

   if (count < 1) {
     return DataFrame::create(
       _("wavelength") = NumericVector::create(),
       _("amplitude")  = NumericVector::create()
     );
   } else {
     // Create the resulting DataFrame
     return DataFrame::create(
       _("wavelength") = beat_wavelength[Rcpp::Range(0, count - 1)],
       _("amplitude")  = beat_amplitude[Rcpp::Range(0, count - 1)]
     );
   }
 }
