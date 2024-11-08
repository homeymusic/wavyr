#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]
List combine_spectra(NumericVector component1, NumericVector amplitude1,
                     NumericVector component2, NumericVector amplitude2,
                     double tolerance = 1e-6) {

  // Step 1: Convert NumericVectors to std::vectors for easier manipulation
  std::vector<double> combined_component(component1.begin(), component1.end());
  combined_component.insert(combined_component.end(), component2.begin(), component2.end());

  std::vector<double> combined_amplitude(amplitude1.begin(), amplitude1.end());
  combined_amplitude.insert(combined_amplitude.end(), amplitude2.begin(), amplitude2.end());

  // Step 2: Create an index vector to sort components and amplitudes together
  std::vector<size_t> order(combined_component.size());
  std::iota(order.begin(), order.end(), 0);  // Fill with 0, 1, 2, ...

  // Sort indices based on the combined_component values
  std::sort(order.begin(), order.end(), [&](size_t i, size_t j) {
    return combined_component[i] < combined_component[j];
  });

  // Step 3: Sort combined_component and combined_amplitude based on sorted indices
  std::vector<double> sorted_component(combined_component.size());
  std::vector<double> sorted_amplitude(combined_amplitude.size());

  for (size_t i = 0; i < order.size(); i++) {
    sorted_component[i] = combined_component[order[i]];
    sorted_amplitude[i] = combined_amplitude[order[i]];
  }

  // Step 4: Aggregating components within the tolerance
  std::vector<double> aggregated_components;
  std::vector<double> aggregated_amplitudes;

  double current_component = sorted_component[0];
  double current_amplitude = sorted_amplitude[0];

  for (size_t i = 1; i < sorted_component.size(); i++) {
    if (std::abs(sorted_component[i] - current_component) <= tolerance) {
      // If within tolerance, aggregate the amplitude
      current_amplitude += sorted_amplitude[i];
    } else {
      // If not within tolerance, push current to results and reset
      aggregated_components.push_back(current_component);
      aggregated_amplitudes.push_back(current_amplitude);

      current_component = sorted_component[i];
      current_amplitude = sorted_amplitude[i];
    }
  }

  // Add the final component and amplitude
  aggregated_components.push_back(current_component);
  aggregated_amplitudes.push_back(current_amplitude);

  // Step 5: Convert back to Rcpp::NumericVectors for the return List
  return List::create(
    _["component"] = NumericVector(aggregated_components.begin(), aggregated_components.end()),
    _["amplitude"] = NumericVector(aggregated_amplitudes.begin(), aggregated_amplitudes.end())
  );
}
