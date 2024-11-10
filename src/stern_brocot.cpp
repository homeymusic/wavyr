#include <Rcpp.h>
#include <tuple>
#include "stern_brocot.h"

using namespace Rcpp;

 //' stern_brocot_cpp
 //'
 //' Approximates a floating-point number to arbitrary uncertainty.
 //'
 //' @param x Number to convert to rational fraction
 //' @param uncertainty Binary search stops once the desired uncertainty is reached
 //'
 //' @return A ratio of num / den
 //' @export
 // [[Rcpp::export]]
 NumericVector stern_brocot_cpp(const double x, const double uncertainty) {
   if (x <= 0) stop("STOP: x must be greater than 0");
   if (uncertainty <= 0) stop("STOP: uncertainty must be greater than 0");

   int cycles = 0;



   if (x <= uncertainty) {
     cycles = 1;
     if (uncertainty < 1) {
       return NumericVector::create(1, static_cast<int>(1 / uncertainty));
     } else {
       return NumericVector::create(1, static_cast<int>(uncertainty));
     }
   }

   double approximation;
   const double valid_min = x - uncertainty;
   const double valid_max = x + uncertainty;

   int left_num = floor(x), left_den = 1;
   int mediant_num = round(x), mediant_den = 1;
   int right_num = floor(x) + 1, right_den = 1;

   approximation = (double) mediant_num / mediant_den;
   const int insane = 1000;

   while (((approximation < valid_min) || (valid_max < approximation)) && cycles < insane) {
     double x0 = 2 * x - approximation;

     if (approximation < valid_min) {
       left_num = mediant_num; left_den = mediant_den;
       int k = floor((right_num - x0 * right_den) / (x0 * left_den - left_num));
       right_num += k * left_num; right_den += k * left_den;

     } else if (valid_max < approximation) {
       right_num = mediant_num; right_den = mediant_den;
       int k = floor((x0 * left_den - left_num) / (right_num - x0 * right_den));
       left_num += k * right_num; left_den += k * right_den;
     }

     mediant_num = left_num + right_num;
     mediant_den = left_den + right_den;
     approximation = (double) mediant_num / mediant_den;
     cycles++;
   }

   if (mediant_num <= 0) stop("STOP: mediant_num is less than or equal to zero");
   if (mediant_den <= 0) stop("STOP: mediant_den is less than or equal to zero");

   return NumericVector::create(mediant_num, mediant_den);
 }
