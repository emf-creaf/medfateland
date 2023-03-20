#include <Rcpp.h>

#ifndef ASPWB_H
#define ASPWB_H
#endif
using namespace Rcpp;

List aspwbInput(double crop_factor, List control, List soil);

List aspwb_day_internal(List x, NumericVector meteovec, 
              double elevation, double slope, double aspect, 
              double runon=0.0, bool verbose=false);

List aspwb_day(List x, 
               CharacterVector date, 
               double tmin, double tmax, double rhmin, double rhmax, double rad, double wind, 
               double latitude, double elevation, double slope, double aspect,
               double prec, double runon=0.0, bool modifyInput = true);