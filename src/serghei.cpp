// [[Rcpp::depends(medfate,meteoland)]]
#include <Rcpp.h>
#include <meteoland.h>
#include <medfate.h>
#include "spwbland.h"
#if SERGHEI_COUPLING
// Relative path to SERGHEI headers (from medfateland/src/ to ../../src/)
#include "../../src/MedFateLand_Serghei.h"
#endif
using namespace Rcpp;
using namespace medfate;
using namespace meteoland;

#if SERGHEI_COUPLING
// =============================================================================
// Conversion helpers: Rcpp → Native C++ structs
// =============================================================================

// Extract soil data from Rcpp List and populate MedFateSoilData struct
// The Rcpp List structure: soilList[[i]] contains W, dVec, VG_alpha, VG_theta_res, VG_theta_sat, VG_n, Ksat
MedFateSoilData convertSoilListToStruct(List soilList, int NC, int NZ) {
    MedFateSoilData soil;
    soil.NC = NC;
    soil.NZ = NZ;

    // Allocate flat arrays (caller is responsible for memory management)
    std::vector<real>* waterVec = new std::vector<real>(NC * NZ);
    std::vector<real>* dVecVec = new std::vector<real>(NC * NZ);
    std::vector<real>* VGalphaVec = new std::vector<real>(NC * NZ);
    std::vector<real>* VGthetaResVec = new std::vector<real>(NC * NZ);
    std::vector<real>* VGthetaSatVec = new std::vector<real>(NC * NZ);
    std::vector<real>* VGnVec = new std::vector<real>(NC * NZ);
    std::vector<real>* KsatVec = new std::vector<real>(NC * NZ);

    // Extract data from Rcpp Lists
    for (int i = 0; i < NC; i++) {
        if (soilList[i] == R_NilValue) continue;

        List soilCell = as<List>(soilList[i]);

        NumericVector W = soilCell["W"];
        NumericVector dVec = soilCell["dVec"];
        NumericVector VGalpha = soilCell["VG_alpha"];
        NumericVector VGthetaRes = soilCell["VG_theta_res"];
        NumericVector VGthetaSat = soilCell["VG_theta_sat"];
        NumericVector VGn = soilCell["VG_n"];
        NumericVector Ksat = soilCell["Ksat"];

        for (int l = 0; l < NZ; l++) {
            int idx = i * NZ + l;
            (*waterVec)[idx] = W[l];
            (*dVecVec)[idx] = dVec[l];
            (*VGalphaVec)[idx] = VGalpha[l];
            (*VGthetaResVec)[idx] = VGthetaRes[l];
            (*VGthetaSatVec)[idx] = VGthetaSat[l];
            (*VGnVec)[idx] = VGn[l];
            (*KsatVec)[idx] = Ksat[l];
        }
    }

    // Set pointers (note: this creates ownership that needs cleanup)
    soil.water = waterVec->data();
    soil.dVec = dVecVec->data();
    soil.VGalpha = VGalphaVec->data();
    soil.VGtheta_res = VGthetaResVec->data();
    soil.VGtheta_sat = VGthetaSatVec->data();
    soil.VGn = VGnVec->data();
    soil.Ksat = KsatVec->data();

    return soil;
}

// Extract source data from Rcpp and populate MedFateSourceData struct
MedFateSourceData convertSourceListToStruct(List uptakeList, NumericVector throughfallVec, int NC, int NZ) {
    MedFateSourceData sources;
    sources.NC = NC;
    sources.NZ = NZ;

    // Allocate flat arrays
    std::vector<real>* uptakeVec = new std::vector<real>(NC * NZ);
    std::vector<real>* throughfallVecCopy = new std::vector<real>(NC);

    // Extract uptake data
    for (int i = 0; i < NC; i++) {
        if (uptakeList[i] == R_NilValue) continue;

        NumericVector uptakeCell = as<NumericVector>(uptakeList[i]);
        for (int l = 0; l < NZ; l++) {
            int idx = i * NZ + l;
            (*uptakeVec)[idx] = uptakeCell[l];
        }
    }

    // Copy throughfall data
    for (int i = 0; i < NC; i++) {
        (*throughfallVecCopy)[i] = throughfallVec[i];
    }

    sources.uptake = uptakeVec->data();
    sources.tfall = throughfallVecCopy->data();

    return sources;
}
#endif

// [[Rcpp::export(".initSerghei")]]
List initSerghei(NumericVector limits, int nrow, int ncol,
                 IntegerVector sf2cell, List xList,
                 String input_dir, String output_dir) {
  // "rock" should have a soil with all rock and zero Ksat
  // "artificial" should have a missing soil
  // "water", "agriculture" and "wildland" should have normal soil
  int nTargetCells = xList.size();
  int nGridCells = nrow*ncol;
  //Initialize Uptake and throughfall
  NumericVector throughfall(nGridCells, NA_REAL);
  List soilListSerghei(nGridCells);
  List uptake(nGridCells);
  for(int i=0;i<nTargetCells; i++) {
    List x = Rcpp::as<Rcpp::List>(xList[i]);
    if(!x.isNULL()) {
      if(x.containsElementNamed("soil")) {
        List soil = Rcpp::as<Rcpp::List>(x["soil"]);
        soilListSerghei[sf2cell[i] - 1] = clone(soil); //indices in R
        NumericVector W = soil["W"];
        // NumericVector widths = soil["widths"];
        // for(int l=0;l<widths.size();l++) Rcout<< widths[l]<<" ";
        // Rcout<<"\n";
        NumericVector vup(W.size(), NA_REAL);
        uptake[sf2cell[i] - 1] = vup; //indices in R
      }
    }
  }  
  // Initialize SERGHEI (call to interface function)
#if SERGHEI_COUPLING
  MedFateLand_Serghei::start();
#endif
  // Use input_dir and output_dir
  List serghei_interface = List::create(_["limits"] = limits,
                                        _["dim"] = IntegerVector::create(nrow,ncol),
                                        _["soilList"] = soilListSerghei,
                                        _["throughfall"] = throughfall,
                                        _["uptake"] = uptake);
  
  return(serghei_interface);
}

// [[Rcpp::export(".callSergheiDay")]]
void callSergheiDay(CharacterVector lct, List xList,
                    DataFrame gridMeteo, List localResults,
                    IntegerVector sf2cell, List serghei_interface) {
  int nX = xList.size();
  
  NumericVector precVec = gridMeteo["Precipitation"];
  
  //B. SERGHEI
  //B.0 - Recover pointers to SERGHEI interface
  List soilListSerghei = serghei_interface["soilList"];
  NumericVector throughfallSerghei = serghei_interface["throughfall"];
  List uptakeSerghei = serghei_interface["uptake"];
  
  //B.1 - Fill uptake and throughfall lists
  for(int i=0;i<nX;i++) {
    if((lct[i]=="wildland") || (lct[i]=="agriculture")) {
      NumericVector vup = Rcpp::as<Rcpp::NumericVector>(uptakeSerghei[sf2cell[i] - 1]); //indices in R
      // Rcout<<".";
      List loc_res_i = localResults[i];
      List res_i = loc_res_i["simulation_results"];
      NumericVector DB = res_i["WaterBalance"];
      DataFrame SB = Rcpp::as<Rcpp::DataFrame>(res_i["Soil"]);
      //Copy throughfall
      throughfallSerghei[sf2cell[i] - 1] = DB["NetRain"]; //indices in R
      //Adding soil evaporation and snowmelt to plant uptake
      double Snowmelt = DB["Snowmelt"];
      double Esoil = DB["SoilEvaporation"];
      NumericVector ExtractionVec = Rcpp::as<Rcpp::NumericVector>(SB["PlantExtraction"]);
      NumericVector EherbVec(ExtractionVec.size(), 0.0);
      if(lct[i]=="wildland") {
        EherbVec = Rcpp::as<Rcpp::NumericVector>(SB["HerbTranspiration"]);
      }
      for(int l=0;l<vup.size();l++) {
        vup[l] = -1.0*(EherbVec[l] + ExtractionVec[l]); // uptake from herbs and plants are negative flows (outflow)
      }
      vup[0] += Snowmelt - Esoil; //snowmelt is a positive flow (inflow) and soil evaporation a negative flow (outflow)
    } else {
      // Rcout<<"+";
      // No interception (i.e. throughfall = rain) in "rock", "artificial" or "water"
      throughfallSerghei[sf2cell[i] - 1] = precVec[i]; //indices in R
      // No uptake in "rock", "artificial" or "water"
      // for(int l=0;l<vup.size();l++) vup[l] = 0.0;
    }
    
  }
  
  // CALL SERGHEI (call to interface function)
#if SERGHEI_COUPLING
  MedFateLand_Serghei::compute_daily_step();
#endif
  //B.3 - Recover new soil moisture state from SERGHEI, calculate the difference between 
  //SERGHEI and MEDFATE and apply the differences to the soil moisture (overall or water pools)
  for(int i=0;i<nX;i++) {
    if((lct[i]=="wildland") || (lct[i]=="agriculture")) {
      // Rcout<<".";
      List soilSerghei = soilListSerghei[sf2cell[i] - 1];  //indices in R
      List x = Rcpp::as<Rcpp::List>(xList[i]);
      if(!x.isNULL()) {
        if(x.containsElementNamed("soil")) {
          List soil = Rcpp::as<Rcpp::List>(x["soil"]);
          NumericVector W_soil = soil["W"];
          NumericVector W_soilSerghei = soilSerghei["W"];
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
}

// [[Rcpp::export(".finishSerghei")]]
void finishSerghei() {
#if SERGHEI_COUPLING
  MedFateLand_Serghei::finalise();
#endif
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
