#include <Rcpp.h>

#ifndef SPWBLAND_H
#define SPWBLAND_H
#endif
using namespace Rcpp;


List watershedDayTetis(String localModel,
                  CharacterVector lct, List xList,
                  IntegerVector waterO, List queenNeigh, List waterQ,
                  NumericVector depth_to_bedrock, NumericVector bedrock_conductivity, NumericVector bedrock_porosity,
                  NumericVector aquifer, NumericVector snowpack,
                  List watershed_control,
                  CharacterVector date,
                  DataFrame gridMeteo,
                  NumericVector latitude, NumericVector elevation, NumericVector slope, NumericVector aspect,
                  double patchsize, bool progress = true);

List initSerghei(List xList,
                 String input_dir, String output_dir);

List watershedDaySerghei(String localModel,
                       CharacterVector lct, List xList,
                       NumericVector snowpack,
                       List serghei_interface,
                       List watershed_control,
                       CharacterVector date,
                       DataFrame gridMeteo,
                       NumericVector latitude, NumericVector elevation, NumericVector slope, NumericVector aspect,
                       double patchsize, bool progress = true);