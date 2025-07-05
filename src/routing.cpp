#include <Rcpp.h>
#include <vector>
#include <iostream>
#include <cmath>
#include <map>
#include <algorithm>
using namespace Rcpp;

//Calculates the total volume of water in the channel network according to a given
//average water stream elevation (m)
double compute_volume(const std::vector<std::vector<double>>& dtm,
                      const std::vector<std::vector<bool>>& mask,
                      double cell_area,
                      double wse)
{
  double vol = 0.0;
  for (size_t i = 0; i < dtm.size(); ++i)
    for (size_t j = 0; j < dtm[0].size(); ++j)
      if (mask[i][j])
        vol += std::max(0.0, wse - dtm[i][j]) * cell_area;
      return vol;
}

//Finds the average water stream elevation (m) for the whole channel network
//corresponding to a target_volume
double find_wse(const std::vector<std::vector<double>>& dtm,
                const std::vector<std::vector<bool>>& mask,
                double cell_area,
                double target_volume,
                double wse_min,
                double wse_max,
                double tol = 0.001)
{
  while (wse_max - wse_min > tol)
  {
    double mid = 0.5 * (wse_min + wse_max);
    if (compute_volume(dtm, mask, cell_area, mid) < target_volume)
      wse_min = mid;
    else
      wse_max = mid;
  }
  return 0.5 * (wse_min + wse_max);
}

struct Stats { double area=0, perim=0; };

Stats wetted_stats(const std::vector<std::vector<double>>& dtm,
                   const std::vector<std::vector<bool>>& mask,
                   double wse,
                   double cell)
{
  Stats s;
  for (size_t i = 0; i < dtm.size(); ++i)
    for (size_t j = 0; j < dtm[0].size(); ++j)
      if (mask[i][j])
      {
        double d = std::max(0.0, wse - dtm[i][j]);
        if (d>0)
        {
          s.area  += d * cell;   // depth × width
          s.perim += cell;       // vertical “wall”
        }
      }
      return s;
}

//Manning's average velocity
double manning_v(double area, double perim, double slope, double n)
{
  if (perim==0) return 0;
  double R = area / perim;
  return (1.0/n) * std::pow(R, 2.0/3.0) * std::sqrt(slope);
}

/* ---------- pixel routing ---------- */
std::map<int,double> route(const std::vector<std::vector<double>>& dtm,
                           const std::vector<std::vector<bool>>& mask,
                           const std::vector<std::vector<double>>& dist,
                           double wse, double v, double cell_area,
                           int dt)
{
  std::map<int,double> bins;
  for (size_t i=0;i<dtm.size();++i)
    for (size_t j=0;j<dtm[0].size();++j)
      if (mask[i][j])
      {
        double d = std::max(0.0, wse - dtm[i][j]);
        if (d>0)
        {
          int bin = static_cast<int>((dist[i][j]/v)/dt);
          bins[bin] += d*cell_area;
        }
      }
      return bins;
}

void example_routing(double totalVol, double cell, int dt) {
  /* tiny 3×3 demo rasters */
  std::vector<std::vector<double>> dtm = {
    {98.0, 98.5, 99.0},
    {97.0, 97.5, 98.0},
    {96.0, 96.5, 97.0}
  };
  std::vector<std::vector<bool>> mask = {
    {true,true,true},
    {true ,true,true },
    {true,true,true}
  };
  std::vector<std::vector<double>> dist = {
    {7.0*cell,8.0*cell,9.0*cell},
    {4.0*cell,5.0*cell,6.0*cell},
    {1.0*cell,2.0*cell,3.0*cell}
  };
  
  double n       = 0.035;     // Manning
  double slope   = 0.001;
  double areaC   = cell*cell; // m²
  
  double wse = find_wse(dtm, mask, areaC, totalVol, 95.0, 100.0);
  std::cout << "WSE = " << wse << " m\n";
  
  Stats s = wetted_stats(dtm, mask, wse, cell);
  std::cout << "Wetted area = " << s.area << " m2, perimeter = " << s.perim <<" m \n";
  double v = manning_v(s.area / 9,  s.perim / 9, slope, n); // avg over 9 wet cells
  std::cout << "Avg vel = " << v << " m/s\n";
  
  auto hydro = route(dtm, mask, dist, wse, v, areaC, dt);
  std::cout << "Hydrograph (bin → vol m³)\n";
  for (auto& kv : hydro)
    std::cout << kv.first*60 << " s : " << kv.second << '\n';
}

//   double cell    = 1.0;       // m (cell width)
// [[Rcpp::export(".newChannelRouting")]]
void newChannelRouting(double totalVol = 5.0, double cell = 1.0, int dt = 60) {
  return(example_routing(totalVol, cell, dt));
}