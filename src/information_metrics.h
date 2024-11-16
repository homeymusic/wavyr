#ifndef INFORMATION_METRICS_H
#define INFORMATION_METRICS_H

#include <vector>
#include <string>

// Declare all metrics functions
double shannon_entropy_cpp(const std::vector<int>& bits);
double hamming_weight_cpp(const std::vector<int>& bits);
double run_length_encoding_cpp(const std::vector<int>& bits);
int depth_cpp(const std::vector<int>& bits);
std::string as_string_cpp(const std::vector<int>& bits);
int as_integer_cpp(const std::vector<int>& bits);

#endif // INFORMATION_METRICS_H
