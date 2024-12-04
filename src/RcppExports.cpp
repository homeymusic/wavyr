// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// approximate_rational_fractions_cpp
DataFrame approximate_rational_fractions_cpp(NumericVector x, const double uncertainty, const double deviation, Rcpp::Nullable<DataFrame> metadata);
RcppExport SEXP _wavyr_approximate_rational_fractions_cpp(SEXP xSEXP, SEXP uncertaintySEXP, SEXP deviationSEXP, SEXP metadataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const double >::type uncertainty(uncertaintySEXP);
    Rcpp::traits::input_parameter< const double >::type deviation(deviationSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<DataFrame> >::type metadata(metadataSEXP);
    rcpp_result_gen = Rcpp::wrap(approximate_rational_fractions_cpp(x, uncertainty, deviation, metadata));
    return rcpp_result_gen;
END_RCPP
}
// combine_spectra_cpp
List combine_spectra_cpp(NumericVector component, NumericVector amplitude, const double tolerance);
RcppExport SEXP _wavyr_combine_spectra_cpp(SEXP componentSEXP, SEXP amplitudeSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type component(componentSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type amplitude(amplitudeSEXP);
    Rcpp::traits::input_parameter< const double >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(combine_spectra_cpp(component, amplitude, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// compute_beats_cpp
DataFrame compute_beats_cpp(NumericVector component, NumericVector amplitude, std::string extent_rate, double tolerance);
RcppExport SEXP _wavyr_compute_beats_cpp(SEXP componentSEXP, SEXP amplitudeSEXP, SEXP extent_rateSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type component(componentSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type amplitude(amplitudeSEXP);
    Rcpp::traits::input_parameter< std::string >::type extent_rate(extent_rateSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_beats_cpp(component, amplitude, extent_rate, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// idealized_spatial_frequency_map_cpp
ListMatrix idealized_spatial_frequency_map_cpp(int nrows, int ncols);
RcppExport SEXP _wavyr_idealized_spatial_frequency_map_cpp(SEXP nrowsSEXP, SEXP ncolsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nrows(nrowsSEXP);
    Rcpp::traits::input_parameter< int >::type ncols(ncolsSEXP);
    rcpp_result_gen = Rcpp::wrap(idealized_spatial_frequency_map_cpp(nrows, ncols));
    return rcpp_result_gen;
END_RCPP
}
// shannon_entropy_cpp
double shannon_entropy_cpp(const std::vector<int>& bits);
RcppExport SEXP _wavyr_shannon_entropy_cpp(SEXP bitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<int>& >::type bits(bitsSEXP);
    rcpp_result_gen = Rcpp::wrap(shannon_entropy_cpp(bits));
    return rcpp_result_gen;
END_RCPP
}
// hamming_weight_cpp
double hamming_weight_cpp(const std::vector<int>& bits);
RcppExport SEXP _wavyr_hamming_weight_cpp(SEXP bitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<int>& >::type bits(bitsSEXP);
    rcpp_result_gen = Rcpp::wrap(hamming_weight_cpp(bits));
    return rcpp_result_gen;
END_RCPP
}
// run_length_encoding_cpp
double run_length_encoding_cpp(const std::vector<int>& bits);
RcppExport SEXP _wavyr_run_length_encoding_cpp(SEXP bitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<int>& >::type bits(bitsSEXP);
    rcpp_result_gen = Rcpp::wrap(run_length_encoding_cpp(bits));
    return rcpp_result_gen;
END_RCPP
}
// depth_cpp
int depth_cpp(const std::vector<int>& bits);
RcppExport SEXP _wavyr_depth_cpp(SEXP bitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<int>& >::type bits(bitsSEXP);
    rcpp_result_gen = Rcpp::wrap(depth_cpp(bits));
    return rcpp_result_gen;
END_RCPP
}
// as_string_cpp
std::string as_string_cpp(const std::vector<int>& bits);
RcppExport SEXP _wavyr_as_string_cpp(SEXP bitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<int>& >::type bits(bitsSEXP);
    rcpp_result_gen = Rcpp::wrap(as_string_cpp(bits));
    return rcpp_result_gen;
END_RCPP
}
// as_integer_cpp
int as_integer_cpp(const std::vector<int>& bits);
RcppExport SEXP _wavyr_as_integer_cpp(SEXP bitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<int>& >::type bits(bitsSEXP);
    rcpp_result_gen = Rcpp::wrap(as_integer_cpp(bits));
    return rcpp_result_gen;
END_RCPP
}
// rationalized_spatial_frequency_map_cpp
DataFrame rationalized_spatial_frequency_map_cpp(int nrows, int ncols, double uncertainty);
RcppExport SEXP _wavyr_rationalized_spatial_frequency_map_cpp(SEXP nrowsSEXP, SEXP ncolsSEXP, SEXP uncertaintySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nrows(nrowsSEXP);
    Rcpp::traits::input_parameter< int >::type ncols(ncolsSEXP);
    Rcpp::traits::input_parameter< double >::type uncertainty(uncertaintySEXP);
    rcpp_result_gen = Rcpp::wrap(rationalized_spatial_frequency_map_cpp(nrows, ncols, uncertainty));
    return rcpp_result_gen;
END_RCPP
}
// rationalized_spectrum_cpp
ComplexMatrix rationalized_spectrum_cpp(const ComplexMatrix& idealized_spectrum, const List& rationalized_spatial_frequencies);
RcppExport SEXP _wavyr_rationalized_spectrum_cpp(SEXP idealized_spectrumSEXP, SEXP rationalized_spatial_frequenciesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const ComplexMatrix& >::type idealized_spectrum(idealized_spectrumSEXP);
    Rcpp::traits::input_parameter< const List& >::type rationalized_spatial_frequencies(rationalized_spatial_frequenciesSEXP);
    rcpp_result_gen = Rcpp::wrap(rationalized_spectrum_cpp(idealized_spectrum, rationalized_spatial_frequencies));
    return rcpp_result_gen;
END_RCPP
}
// stern_brocot_cpp
DataFrame stern_brocot_cpp(const double x, const double uncertainty);
RcppExport SEXP _wavyr_stern_brocot_cpp(SEXP xSEXP, SEXP uncertaintySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type x(xSEXP);
    Rcpp::traits::input_parameter< const double >::type uncertainty(uncertaintySEXP);
    rcpp_result_gen = Rcpp::wrap(stern_brocot_cpp(x, uncertainty));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_wavyr_approximate_rational_fractions_cpp", (DL_FUNC) &_wavyr_approximate_rational_fractions_cpp, 4},
    {"_wavyr_combine_spectra_cpp", (DL_FUNC) &_wavyr_combine_spectra_cpp, 3},
    {"_wavyr_compute_beats_cpp", (DL_FUNC) &_wavyr_compute_beats_cpp, 4},
    {"_wavyr_idealized_spatial_frequency_map_cpp", (DL_FUNC) &_wavyr_idealized_spatial_frequency_map_cpp, 2},
    {"_wavyr_shannon_entropy_cpp", (DL_FUNC) &_wavyr_shannon_entropy_cpp, 1},
    {"_wavyr_hamming_weight_cpp", (DL_FUNC) &_wavyr_hamming_weight_cpp, 1},
    {"_wavyr_run_length_encoding_cpp", (DL_FUNC) &_wavyr_run_length_encoding_cpp, 1},
    {"_wavyr_depth_cpp", (DL_FUNC) &_wavyr_depth_cpp, 1},
    {"_wavyr_as_string_cpp", (DL_FUNC) &_wavyr_as_string_cpp, 1},
    {"_wavyr_as_integer_cpp", (DL_FUNC) &_wavyr_as_integer_cpp, 1},
    {"_wavyr_rationalized_spatial_frequency_map_cpp", (DL_FUNC) &_wavyr_rationalized_spatial_frequency_map_cpp, 3},
    {"_wavyr_rationalized_spectrum_cpp", (DL_FUNC) &_wavyr_rationalized_spectrum_cpp, 2},
    {"_wavyr_stern_brocot_cpp", (DL_FUNC) &_wavyr_stern_brocot_cpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_wavyr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
