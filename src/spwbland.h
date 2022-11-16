#include <Rcpp.h>

#ifndef SPWBLAND_H
#define SPWBLAND_H
#endif
using namespace Rcpp;

List watershedDay(String localModel,
                  CharacterVector lct, List xList, List soilList,
                  IntegerVector waterO, List queenNeigh, List waterQ,
                  NumericVector depthtobedrock, NumericVector bedrockconductivity, NumericVector bedrockporosity,
                  NumericVector aquifer, NumericVector snowpack,
                  List correctionFactors,
                  CharacterVector date,
                  DataFrame gridMeteo,
                  NumericVector latitude, NumericVector elevation, NumericVector slope, NumericVector aspect,
                  double patchsize);
