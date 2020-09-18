#include <Rcpp.h>

#ifndef SPWBLAND_H
#define SPWBLAND_H
#endif
using namespace Rcpp;

List wswbDay(CharacterVector lct, List xList, List soilList,
             IntegerVector waterO, List queenNeigh, List waterQ,
             List correctionFactors,
             CharacterVector date,
             NumericVector tminVec, NumericVector tmaxVec, NumericVector rhminVec, NumericVector rhmaxVec,
             NumericVector precVec, NumericVector radVec, NumericVector wsVec,
             NumericVector latitude, NumericVector elevation, NumericVector slope, NumericVector aspect,
             double patchsize);
