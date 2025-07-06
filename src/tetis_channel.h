#include <Rcpp.h>

#ifndef TETIS_CHANNEL_H
#define TETIS_CHANNEL_H
#endif
using namespace Rcpp;

void tetisChannelRouting(NumericVector ChannelExport, NumericVector WatershedExport,
                         NumericVector elevation, NumericVector slope, 
                         LogicalVector isChannel, LogicalVector isOutlet, 
                         IntegerVector target_outlet, IntegerVector distance_to_outlet, NumericVector outlet_backlog,
                         List watershed_control, double patchsize);
