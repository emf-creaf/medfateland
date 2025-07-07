#include <Rcpp.h>
#include <vector>
#include <iostream>
#include <cmath>
#include <map>
#include <algorithm>
using namespace Rcpp;

//Calculates the total volume of water in the channel network according to a given
//average water stream elevation (m)
double compute_volume(NumericVector dtm,
                      LogicalVector mask,
                      double patchsize,
                      double wse)
{
  double vol = 0.0;
  for(int i = 0; i < dtm.size(); i++) {
    if(mask[i]) vol += std::max(0.0, wse - dtm[i]) * patchsize;
  }
  return(vol);
}

//Finds the average water stream elevation (m) for the whole channel network
//corresponding to a target_volume
double find_wse(NumericVector dtm,
                LogicalVector mask,
                double patchsize,
                double target_volume,
                double wse_min,
                double wse_max,
                double tol = 0.001) {
  while (wse_max - wse_min > tol) {
    double mid = 0.5 * (wse_min + wse_max);
    if (compute_volume(dtm, mask, patchsize, mid) < target_volume) {
      wse_min = mid;
    } else{
      wse_max = mid;
    }
  }
  return(0.5 * (wse_min + wse_max));
}

struct Stats { double area=0.0, perim=0.0, slope_percent = 0.0, n = 0.0;};

Stats wetted_stats(NumericVector dtm_m,
                   NumericVector slope_degrees,
                   LogicalVector mask,
                   double wse_m,
                   double cell_width) {
  Stats s;
  for(int i = 0; i < dtm_m.size(); i++) {
    if(mask[i]){
      double d = std::max(0.0, wse_m - dtm_m[i]);
      if (d > 0.0) {
        s.area  += d * cell_width;   // depth × width
        s.perim += cell_width;       // vertical “wall”
        s.slope_percent += 100.0*tan(slope_degrees[i]*(3.141592/180.0));
        s.n = s.n + 1.0;
      }
    }
  }
  s.slope_percent = s.slope_percent/s.n;
  return s;
}

//Manning's average velocity
double manning_v(double area, double perim, double slope_percent, double n) {
  if (perim==0) return 0;
  double R = area / perim;
  return (1.0/n) * std::pow(R, 2.0/3.0) * std::sqrt(slope_percent/100.0);
}

double outlet_volume(NumericVector dtm_m,
                     LogicalVector mask,
                     IntegerVector distance_to_outlet,
                     double wse, double v, double cell_area,
                     double dt_sec) {
  double cell_width = sqrt(cell_area);
  double outlet_volume = 0.0;
  for(int i = 0; i < dtm_m.size(); i++) {
    if(mask[i]){
      double d = std::max(0.0, wse - dtm_m[i]);
      if(d>0.0) {
        double dist = ((double) distance_to_outlet[i])*cell_width;
        double vol_m3 = d*cell_area;
        double sec_to_outlet = dist/v;
        if(sec_to_outlet < dt_sec) outlet_volume += vol_m3;
      }
    }
  }
  return(outlet_volume);
}

// [[Rcpp::export(".tetisChannelRouting")]]
void tetisChannelRouting(NumericVector ChannelExport, NumericVector WatershedExport,
                         NumericVector elevation, NumericVector slope, 
                         LogicalVector isChannel, LogicalVector isOutlet, 
                         IntegerVector target_outlet, IntegerVector distance_to_outlet, NumericVector outlet_backlog,
                         List watershed_control, double patchsize) {
  List tetis_parameters = watershed_control["tetis_parameters"];
  
  int nX = ChannelExport.size();  
  //1. For each channel cell, add channel export to the corresponding outlet's network volume, which may be non-zero due to previous day legacies
  for(int i=0;i<nX;i++) {
    if(isChannel[i]) {
      if(ChannelExport[i]>0.0) {
        int target = target_outlet[i] - 1; //Decrease index by one in C++
        outlet_backlog[target] = (outlet_backlog[target]  + (ChannelExport[i]*patchsize/1e3)); // Convert mm = l/m2 to m3
      }
    }
  }
  //2. For each outlet:
  LogicalVector mask = LogicalVector(nX, false);
  double cell_width = sqrt(patchsize);
  double manning_n = tetis_parameters["n_manning"];     // Manning's coefficient
  double dt_sec = 86400.0; //Seconds in 1 day
  for(int i=0;i<nX;i++) {
    if(isOutlet[i]) {
      double backlog = outlet_backlog[i];
      if(backlog > 0.0) {
        //    a. Define the mask of channel network, minimum and maximum elevation
        double wse_min = 9999.0;
        double wse_max = -9999.0;
        for(int j=0;j<nX;j++) {
           mask[j] = ((target_outlet[j] - 1) == i);
           if(mask[j]) {
            wse_min = std::min(wse_min, elevation[j]); 
            wse_max = std::max(wse_max, elevation[j]); 
          }
        }
        //    b. Find the water stream elevation corresponding to the total volume
        double wse = find_wse(elevation, mask, patchsize, backlog, wse_min, wse_max);
        // Rcout << " Outlet: " << i << " channel cells: " << sum(mask) << " total volume " << backlog << " WSE: "<<  wse<< " (m)";
        //    c. Determine the wetted area and perimeter in the channel network
        Stats s = wetted_stats(elevation, slope, mask, wse, cell_width);
        // Rcout << " wetted cells: " << s.n<< " area: "<<  s.area << " (m2) perim: " <<  s.perim <<" (m) ave slope "<< s.slope_percent<< " (%)";
        //    d. Calculate average stream velocity (m/s) using Manning's equation
        double v = manning_v(s.area, s.perim, s.slope_percent, manning_n);
        // Rcout<< " avg vel = " << v << " (m/s)";
        //    e. For every cell whose elevation is lower than the water stream elevation, determine the time to outlet
        double outlet_vol_m3 = outlet_volume(elevation, mask, distance_to_outlet, 
                                             wse, v, patchsize,
                                             dt_sec);
        outlet_vol_m3 = std::min(outlet_vol_m3, backlog);
        // Rcout<< " outlet_vol = " << outlet_vol_m3 << " (m3)\n";
        //    f. Add the volume reaching the outlet during the current day and keep the remaining for the day after.
        WatershedExport[i] = outlet_vol_m3*(1e3/patchsize);
        outlet_backlog[i] = backlog - outlet_vol_m3;
      }
    }
  }
}
