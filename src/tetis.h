#include <Rcpp.h>

#ifndef TETIS_H
#define TETIS_H
#endif
using namespace Rcpp;


DataFrame tetisWatershedFlows(List y,
                              IntegerVector waterO, List queenNeigh, List waterQ,
                              List watershed_control,
                              double patchsize);
NumericVector tetisApplyBaseflowChangesToAquifer(List y,
                                                 NumericVector baseflowBalance,
                                                 double patchsize);
void tetisApplyLocalFlowsToAquifer(List y,
                                   NumericVector CapillarityRise,
                                   NumericVector DeepDrainage);
NumericVector tetisOverlandFlows( NumericVector Runoff, NumericVector AquiferDischarge,
                                  NumericVector waterO, List queenNeigh, List waterQ);
