#include "information_metrics.h"
#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <numeric>
#include <algorithm>
#include <set>

using namespace Rcpp;

// Shannon Entropy (with 1e-6 precision)
// [[Rcpp::export]]
double shannon_entropy_cpp(const std::vector<int>& bits) {
  if (bits.empty()) return 0.0;

  // Count occurrences of 0 and 1
  size_t count_0 = std::count(bits.begin(), bits.end(), 0);
  size_t count_1 = bits.size() - count_0;

  // Calculate probabilities
  double p0 = static_cast<double>(count_0) / bits.size();
  double p1 = static_cast<double>(count_1) / bits.size();

  // Initialize entropy
  double entropy = 0.0;

  // Compute Shannon entropy
  if (p0 > 0) entropy -= p0 * std::log2(p0);
  if (p1 > 0) entropy -= p1 * std::log2(p1);

  return entropy; // No rounding, rely on test tolerance
}

// Hamming Weight
// [[Rcpp::export]]
double hamming_weight_cpp(const std::vector<int>& bits) {
  return std::count(bits.begin(), bits.end(), 1);
}

// Run-Length Encoding
// [[Rcpp::export]]
double run_length_encoding_cpp(const std::vector<int>& bits) {
  if (bits.empty()) return 0.0;

  int runs = 1;

  for (size_t i = 1; i < bits.size(); ++i) {
    if (bits[i] != bits[i - 1]) {
      runs++;
    }
  }

  return runs;
}

// Depth Metric
// [[Rcpp::export]]
int depth_cpp(const std::vector<int>& bits) {
  return static_cast<int>(bits.size()); // The depth corresponds to the length of the binary string
}

// Convert Bits to String (Tidyverse-style naming)
// [[Rcpp::export]]
std::string as_string_cpp(const std::vector<int>& bits) {
  std::ostringstream oss;
  for (int bit : bits) {
    oss << bit;
  }
  return oss.str();
}

// Convert binary string to integer
// [[Rcpp::export]]
int as_integer_cpp(const std::vector<int>& bits) {
  int result = 0;
  for (size_t i = 0; i < bits.size(); ++i) {
    result = (result << 1) | bits[i];  // Shift left and add current bit
  }
  return result;
}
