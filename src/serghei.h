#include <Rcpp.h>

#ifndef SERGHEI_H
#define SERGHEI_H
#endif
using namespace Rcpp;


List initSerghei(NumericVector limits, int nrow, int ncol,
                 IntegerVector sf2cell, List xList,
                 String input_dir, String output_dir);

void callSergheiDay(CharacterVector lct, List xList,
                    DataFrame gridMeteo, List localResults,
                    IntegerVector sf2cell, List serghei_interface);