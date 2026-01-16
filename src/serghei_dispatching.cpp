// [[Rcpp::depends(medfate,meteoland)]]
#include <Rcpp.h>
#include <meteoland.h>
#include <medfate.h>
#include "spwbland.h"
using namespace Rcpp;
using namespace medfate;
using namespace meteoland;

#if SERGHEI_MEDFATELAND
// Include the THIN public API header (no MPI, no Kokkos dependencies)
#include "../serghei/src/MedFateLand_API.h"

// Global structs for MEDFATE data (allocated in initSerghei, freed in finishSerghei)
// Now using the public POD structs from the API
std::vector<double>* waterVec = nullptr;
std::vector<double>* dVecVec = nullptr;
std::vector<double>* VGalphaVec = nullptr;
std::vector<double>* VGthetaResVec = nullptr;
std::vector<double>* VGthetaSatVec = nullptr;
std::vector<double>* VGnVec = nullptr;
std::vector<double>* KsatVec = nullptr;
std::vector<double>* uptakeVec = nullptr;
std::vector<double>* throughfallVecCopy = nullptr;

// Public API structs (allocated here, pointers passed to SERGHEI)
MedFateSoilData_C soil_C;
MedFateSourceData_C sources;

#endif

// [[Rcpp::export(".initSerghei")]]
void initSerghei(NumericVector limits, int nrow, int ncol, int nlayers,
                 IntegerVector sf2cell, List xList,
                 String input_dir, String output_dir) {
#if SERGHEI_MEDFATELAND

  // "rock" should have a soil with all rock and zero Ksat
  // "artificial" should have a missing soil
  // "water", "agriculture" and "wildland" should have normal soil
  int nTargetCells = xList.size();
  int nGridCells = nrow*ncol;

  // Allocate flat arrays for soil data for the full grid
  // FIX: Use soil_C.NC and soil_C.NZ after initialization
  waterVec = new std::vector<double>(nGridCells * nlayers);
  dVecVec = new std::vector<double>(nGridCells * nlayers);
  VGalphaVec = new std::vector<double>(nGridCells * nlayers);
  VGthetaResVec = new std::vector<double>(nGridCells * nlayers);
  VGthetaSatVec = new std::vector<double>(nGridCells * nlayers);
  VGnVec = new std::vector<double>(nGridCells * nlayers);
  KsatVec = new std::vector<double>(nGridCells * nlayers);

  // Allocate flat arrays for sources for the full grid
  uptakeVec = new std::vector<double>(nGridCells * nlayers);
  throughfallVecCopy = new std::vector<double>(nGridCells);

  // Initialize the public API structs
  soil_C.NC = nGridCells;
  soil_C.NZ = nlayers;
  soil_C.water = waterVec->data();
  soil_C.dVec = dVecVec->data();
  soil_C.VGalpha = VGalphaVec->data();
  soil_C.VGtheta_res = VGthetaResVec->data();
  soil_C.VGtheta_sat = VGthetaSatVec->data();
  soil_C.VGn = VGnVec->data();
  soil_C.Ksat = KsatVec->data();

  sources.NC = nGridCells;
  sources.NZ = nlayers;
  sources.uptake = uptakeVec->data();
  sources.tfall = throughfallVecCopy->data();

  // Extract data from Rcpp Lists
  for (int i = 0; i < nTargetCells; i++) {
    List x = Rcpp::as<Rcpp::List>(xList[i]);
    if(!x.isNULL()) {
      if(x.containsElementNamed("soil")) {
        // Get grid cell index
        int i_grid = sf2cell[i] - 1;
        DataFrame soilCell = Rcpp::as<Rcpp::List>(x["soil"]);

        NumericVector W = soilCell["W"];
        NumericVector dVec = soilCell["widths"];
        NumericVector VGalpha = soilCell["VG_alpha"];
        NumericVector VGthetaRes = soilCell["VG_theta_res"];
        NumericVector VGthetaSat = soilCell["VG_theta_sat"];
        NumericVector VGn = soilCell["VG_n"];
        NumericVector Ksat = soilCell["Ksat"];

        for (int l = 0; l < soil_C.NZ; l++) {
          int idx = i_grid * soil_C.NZ + l;
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

  // Prepare argc, argv for SERGHEI (empty for now)
  int argc = 4;
  char* argv[4];
  std::string inputDir = std::string(input_dir);
  std::string outputDir = std::string(output_dir);
  std::string numSteps = "1";

  argv[1] = const_cast<char*>(inputDir.c_str());
  argv[2] = const_cast<char*>(outputDir.c_str());
  argv[3] = const_cast<char*>(numSteps.c_str());
  // int argc = 0; 
  // char** argv = nullptr;
  // Call SERGHEI using the new C API
  Rcout << "before starting serghei\n";
  Rcout << argv[1] <<"\n";
  Rcout << argv[2] <<"\n";
  Rcout << argv[3] <<"\n";
  // stop("kk");
  int ret = MedFateLand_startFromMedFate(argc, argv, &soil_C, &sources);
  Rcout << "after starting serghei\n";
  if (ret != 0) {
    Rcpp::stop("SERGHEI initialization failed with code: " + std::to_string(ret));
  }

#endif
}

// [[Rcpp::export(".callSergheiDay")]]
void callSergheiDay(CharacterVector lct, List xList,
                    DataFrame gridMeteo, List localResults,
                    IntegerVector sf2cell) {

#if SERGHEI_MEDFATELAND

  int nX = xList.size();
  int nlayers = soil_C.NZ;
  
  NumericVector precVec = gridMeteo["Precipitation"];

  //B.1 - Fill uptake and throughfall arrays
  for(int i=0;i<nX;i++) {
    int i_grid = sf2cell[i] - 1;
    if((lct[i]=="wildland") || (lct[i]=="agriculture")) {
      
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
      sources.tfall[i_grid] = DB["NetRain"];
      for(int l=0;l<nlayers;l++) {
        int idx = i_grid * soil_C.NZ + l;
        double vup = -1.0*(EherbVec[l] + ExtractionVec[l]); // uptake from herbs and plants are negative flows (outflow)
        if(l==0) vup += Snowmelt - Esoil; //snowmelt is a positive flow (inflow) and soil evaporation a negative flow (outflow)
        sources.uptake[idx] = vup;
      }
    } else {
      // No interception (i.e. throughfall = rain) in "rock", "artificial" or "water"
      sources.tfall[i_grid] = precVec[i_grid];

      // Define nlayers before using
      for(int l=0;l<nlayers;l++) {
        int idx = i_grid * soil_C.NZ + l;
        sources.uptake[idx] = 0.0;
      }
    }

  }

  // CALL SERGHEI using the new C API
  int ret = MedFateLand_evolveFromMedFate(&sources);
  if (ret != 0) {
    Rcpp::stop("SERGHEI evolve step failed with code: " + std::to_string(ret));
  }

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
            int idx = i_grid * soil_C.NZ + l;
            // FIX: Use soil.water (not soil.waterVec)
            W_soilSerghei[l] = soil_C.water[idx];
          }
          // Estimate difference between current soil and serghei soil
          NumericVector W_diff = W_soilSerghei - W_soil;
          for(int l=0; l<W_soil.size();l++) {
            // FIX: Use l instead of i
            W_soil[l] = W_soil[l] + W_diff[l];
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
#if SERGHEI_MEDFATELAND
  // Call SERGHEI using the new C API
  int ret = MedFateLand_finishFromMedFate();
  if (ret != 0) {
    Rcpp::warning("SERGHEI finalization failed with code: " + std::to_string(ret));
  }

  // Clean up allocated memory
  delete waterVec;
  delete dVecVec;
  delete VGalphaVec;
  delete VGthetaResVec;
  delete VGthetaSatVec;
  delete VGnVec;
  delete KsatVec;
  delete uptakeVec;
  delete throughfallVecCopy;

  waterVec = nullptr;
  dVecVec = nullptr;
  VGalphaVec = nullptr;
  VGthetaResVec = nullptr;
  VGthetaSatVec = nullptr;
  VGnVec = nullptr;
  KsatVec = nullptr;
  uptakeVec = nullptr;
  throughfallVecCopy = nullptr;
#endif
}
