// stern_brocot.h

#ifndef STERN_BROCOT_H
#define STERN_BROCOT_H

#include <Rcpp.h>  // Required for Rcpp types like NumericVector

// Function declaration for stern_brocot
Rcpp::NumericVector stern_brocot(const double x, const double uncertainty);

#endif // STERN_BROCOT_H
