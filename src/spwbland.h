#include <Rcpp.h>

#ifndef SPWBLAND_H
#define SPWBLAND_H
#endif
using namespace Rcpp;

List wswbDay(CharacterVector lct, List xList, List soilList,
             IntegerVector waterO, List queenNeigh, List waterQ,
             DataFrame bedrock, NumericVector aquifer,
             List correctionFactors,
             CharacterVector date,
             DataFrame gridMeteo,
             NumericVector latitude, NumericVector elevation, NumericVector slope, NumericVector aspect,
             double patchsize);
