#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]
List combine_spectra_cpp(NumericVector component, NumericVector amplitude, const double tolerance) {
  // Convert Rcpp NumericVectors to std::vectors for manipulation
  std::vector<double> sorted_component(component.begin(), component.end());
  std::vector<double> sorted_amplitude(amplitude.begin(), amplitude.end());

  // Sort the components and amplitudes based on the component values
  std::vector<size_t> order(sorted_component.size());
  std::iota(order.begin(), order.end(), 0);  // Fill with 0, 1, 2, ...

  std::sort(order.begin(), order.end(), [&](size_t i, size_t j) {
    return sorted_component[i] < sorted_component[j];
  });

  std::vector<double> combined_component(sorted_component.size());
  std::vector<double> combined_amplitude(sorted_amplitude.size());

  for (size_t i = 0; i < order.size(); i++) {
    combined_component[i] = sorted_component[order[i]];
    combined_amplitude[i] = sorted_amplitude[order[i]];
  }

  // Step 2: Combine components within the specified tolerance
  std::vector<double> aggregated_components;
  std::vector<double> aggregated_amplitudes;

  double current_component = combined_component[0];
  double current_amplitude = combined_amplitude[0];

  for (size_t i = 1; i < combined_component.size(); i++) {
    if (std::abs(combined_component[i] - current_component) <= tolerance) {
      // If within tolerance, aggregate the amplitude
      current_amplitude += combined_amplitude[i];
    } else {
      // If not within tolerance, push current to results and reset
      aggregated_components.push_back(current_component);
      aggregated_amplitudes.push_back(current_amplitude);

      current_component = combined_component[i];
      current_amplitude = combined_amplitude[i];
    }
  }

  // Add the final component and amplitude
  aggregated_components.push_back(current_component);
  aggregated_amplitudes.push_back(current_amplitude);

  // Step 3: Convert back to Rcpp::NumericVectors for the return List
  return List::create(
    _["component"] = NumericVector(aggregated_components.begin(), aggregated_components.end()),
    _["amplitude"] = NumericVector(aggregated_amplitudes.begin(), aggregated_amplitudes.end())
  );
}
