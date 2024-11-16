#ifndef stern_brocot_H
#define stern_brocot_H

#include <Rcpp.h>  // Required for Rcpp types like DataFrame

// Function declaration for stern_brocot_cpp
Rcpp::DataFrame stern_brocot_cpp(const double x, const double uncertainty);

#endif // stern_brocot_H
