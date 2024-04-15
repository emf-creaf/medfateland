#include <Rcpp.h>

#ifndef SPWBLAND_H
#define SPWBLAND_H
#endif
using namespace Rcpp;


DataFrame tetisWatershedFlows(List y,
                              IntegerVector waterO, List queenNeigh, List waterQ,
                              List watershed_control,
                              double patchsize);
NumericVector tetisApplyBaseflowChangesToAquifer(List y,
                                                 NumericVector BaseflowInput, NumericVector BaseflowOutput,
                                                 double patchsize);
void tetisApplyLocalFlowsToAquifer(List y,
                                   NumericVector CapillarityRise,
                                   NumericVector DeepDrainage);
void copySnowpackToSoil(List y);
void copySnowpackFromSoil(List y);

List initSerghei(NumericVector limits, int nrow, int ncol,
                 IntegerVector sf2cell, List xList,
                 String input_dir, String output_dir);

void callSergheiDay(CharacterVector lct, List xList,
                    DataFrame gridMeteo, List localResults,
                    IntegerVector sf2cell, List serghei_interface);