// [[Rcpp::depends(medfate,meteoland)]]
#include <Rcpp.h>
#include <meteoland.h>
#include <medfate.h>
#include "spwbland.h"
using namespace Rcpp;
using namespace medfate;
using namespace meteoland;

#if SERGHEI_COUPLING
// Relative path to SERGHEI headers 
#include "../serghei/src/MedFateLand_Serghei.h"

MedFateSoilData soil;
MedFateSourceData sources;

#endif


// [[Rcpp::export(".initSerghei")]]
void initSerghei(NumericVector limits, int nrow, int ncol, int nlayers,
                 IntegerVector sf2cell, List xList,
                 String input_dir, String output_dir) {
#if SERGHEI_COUPLING
  
  // "rock" should have a soil with all rock and zero Ksat
  // "artificial" should have a missing soil
  // "water", "agriculture" and "wildland" should have normal soil
  int nTargetCells = xList.size();
  int nGridCells = nrow*ncol;
  
  soil.NC = nGridCells;
  soil.NZ = nlayers;
  
  sources.NC = nGridCells;
  sources.NZ = nlayers;
  
  // Allocate flat arrays for soil data for the full grid
  std::vector<real>* waterVec = new std::vector<real>(NC * NZ);
  std::vector<real>* dVecVec = new std::vector<real>(NC * NZ);
  std::vector<real>* VGalphaVec = new std::vector<real>(NC * NZ);
  std::vector<real>* VGthetaResVec = new std::vector<real>(NC * NZ);
  std::vector<real>* VGthetaSatVec = new std::vector<real>(NC * NZ);
  std::vector<real>* VGnVec = new std::vector<real>(NC * NZ);
  std::vector<real>* KsatVec = new std::vector<real>(NC * NZ);
  
  // Allocate flat arrays for sources for the full grid
  std::vector<real>* uptakeVec = new std::vector<real>(NC * NZ);
  std::vector<real>* throughfallVecCopy = new std::vector<real>(NC);
  
  
  // Extract data from Rcpp Lists
  for (int i = 0; i < nTargetCells; i++) {
    List x = Rcpp::as<Rcpp::List>(xList[i]);
    if(!x.isNULL()) {
      if(x.containsElementNamed("soil")) {
        // Get grid cell index
        int i_grid = sf2cell[i] - 1;
        DataFrame soilCell = Rcpp::as<Rcpp::List>(x["soil"]);
        
        NumericVector W = soilCell["W"];
        NumericVector dVec = soilCell["dVec"];
        NumericVector VGalpha = soilCell["VG_alpha"];
        NumericVector VGthetaRes = soilCell["VG_theta_res"];
        NumericVector VGthetaSat = soilCell["VG_theta_sat"];
        NumericVector VGn = soilCell["VG_n"];
        NumericVector Ksat = soilCell["Ksat"];
        
        for (int l = 0; l < NZ; l++) {
          int idx = i_grid * NZ + l;
          (*waterVec)[idx] = W[l];
          (*dVecVec)[idx] = dVec[l];
          (*VGalphaVec)[idx] = VGalpha[l];
          (*VGthetaResVec)[idx] = VGthetaRes[l];
          (*VGthetaSatVec)[idx] = VGthetaSat[l];
          (*VGnVec)[idx] = VGn[l];
          (*KsatVec)[idx] = Ksat[l];
        }
      }
    }
  }
  
  
  soil.water = waterVec->data();
  soil.dVec = dVecVec->data();
  soil.VGalpha = VGalphaVec->data();
  soil.VGtheta_res = VGthetaResVec->data();
  soil.VGtheta_sat = VGthetaSatVec->data();
  soil.VGn = VGnVec->data();
  soil.Ksat = KsatVec->data();
  
  sources.uptake = uptakeVec->data();
  sources.tfall = throughfallVecCopy->data();
  
  MedFateLand_Serghei::start();

#endif  
}

// [[Rcpp::export(".initSerghei")]]
void callSergheiDay(CharacterVector lct, List xList,
                    DataFrame gridMeteo, List localResults,
                    IntegerVector sf2cell) {
  
#if SERGHEI_COUPLING

  int nX = xList.size();
  
  NumericVector precVec = gridMeteo["Precipitation"];
  

  //B.1 - Fill uptake and throughfall lists
  for(int i=0;i<nX;i++) {
    if((lct[i]=="wildland") || (lct[i]=="agriculture")) {
      int i_grid = sf2cell[i] - 1;
      
      // Rcout<<".";
      List loc_res_i = localResults[i];
      List res_i = loc_res_i["simulation_results"];
      NumericVector DB = res_i["WaterBalance"];
      DataFrame SB = Rcpp::as<Rcpp::DataFrame>(res_i["Soil"]);
      
      //Adding soil evaporation and snowmelt to plant uptake
      double Snowmelt = DB["Snowmelt"];
      double Esoil = DB["SoilEvaporation"];
      NumericVector ExtractionVec = Rcpp::as<Rcpp::NumericVector>(SB["PlantExtraction"]);
      int nlayers = ExtractionVec.size();
      NumericVector EherbVec(nlayers, 0.0);
      if(lct[i]=="wildland") {
        EherbVec = Rcpp::as<Rcpp::NumericVector>(SB["HerbTranspiration"]);
      }
      
      //Copy throughfall
      sources.tfall[i_grid] = DB["NetRain"]; //indices in R
      for(int l=0;l<nlayers;l++) {
        int idx = i_grid * NZ + l;
        double vup = -1.0*(EherbVec[l] + ExtractionVec[l]); // uptake from herbs and plants are negative flows (outflow)
        if(l==0) vup += Snowmelt - Esoil; //snowmelt is a positive flow (inflow) and soil evaporation a negative flow (outflow)
        sources.uptake[idx] = vup;
      }
    } else {
      // No interception (i.e. throughfall = rain) in "rock", "artificial" or "water"
      sources.tfall[i_grid] = precVec[i_grid]; //indices in R
      // No uptake in "rock", "artificial" or "water"
      for(int l=0;l<vup.size();l++) {
        int idx = i_grid * NZ + l;
        sources.uptake[idx] = 0.0;
      }
    }
    
  }
  
  // CALL SERGHEI (call to interface function)
  MedFateLand_Serghei::compute_daily_step();
  
  //B.3 - Recover new soil moisture state from SERGHEI, calculate the difference between 
  //SERGHEI and MEDFATE and apply the differences to the soil moisture (overall or water pools)
  for(int i=0;i<nX;i++) {
    if((lct[i]=="wildland") || (lct[i]=="agriculture")) {
      int i_grid = sf2cell[i] - 1;
      List x = Rcpp::as<Rcpp::List>(xList[i]);
      if(!x.isNULL()) {
        if(x.containsElementNamed("soil")) {
          List soil_i = Rcpp::as<Rcpp::List>(x["soil"]);
          NumericVector W_soil = soil_i["W"];
          NumericVector W_soilSerghei(nlayers, 0.0);
          for(int l=0;l<nlayers;l++) {
            int idx = i_grid * NZ + l;
            W_soilSerghei[l] = soil.waterVec[idx];
          }
          // Estimate difference between current soil and serghei soil
          NumericVector W_diff = W_soilSerghei - W_soil;
          for(int l=0; l<W_soil.size();l++) {
            //Add difference to overall soil
            W_soil[l] = W_soil[i] + W_diff[l];
          }
          //Update water pools if present
          if(x.containsElementNamed("belowLayers")) {
            List belowLayers = Rcpp::as<Rcpp::List>(x["belowLayers"]);
            if(belowLayers.containsElementNamed("Wpool")) {
              NumericMatrix W_mat = belowLayers["Wpool"];
              for(int c=0;c<W_mat.nrow();c++) {
                for(int l=0; l<W_soil.size();l++) {
                  W_mat(c,l) = W_mat(c,l) + W_diff[l];
                }
              }
            }
          }
        }
      }
    }
  }
#endif
  
}


// [[Rcpp::export(".finishSerghei")]]
void finishSerghei() {
#if SERGHEI_COUPLING
  MedFateLand_Serghei::finalise();
#endif
}
