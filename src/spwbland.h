#include <Rcpp.h>

#ifndef SPWBLAND_H
#define SPWBLAND_H
#endif
using namespace Rcpp;

List spwbgridDay(CharacterVector lct, List xList, List soilList,
                 IntegerVector waterO, List queenNeigh, List waterQ,
                 NumericVector tdayVec, NumericVector petVec, NumericVector rainVec,
                 NumericVector erVec, NumericVector radVec, NumericVector elevation,
                 NumericVector trackSpecies, double patchsize);
