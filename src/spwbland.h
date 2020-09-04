#include <Rcpp.h>

#ifndef SPWBLAND_H
#define SPWBLAND_H
#endif
using namespace Rcpp;

List spwbgridDay(CharacterVector lct, List xList, List soilList,
                 IntegerVector waterO, List queenNeigh, List waterQ,
                 CharacterVector date,
                 NumericVector tminVec, NumericVector tmaxVec, NumericVector rhminVec, NumericVector rhmaxVec,
                 NumericVector precVec, NumericVector radVec, NumericVector wsVec,
                 NumericVector latitude, NumericVector elevation, NumericVector slope, NumericVector aspect,
                 double patchsize);
