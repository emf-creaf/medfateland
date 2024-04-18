// [[Rcpp::depends(medfate,meteoland)]]
#include <numeric>
#include <Rcpp.h>
#include <meteoland.h>
#include <medfate.h>
using namespace Rcpp;
using namespace medfate;
using namespace meteoland;

/**
 * Conversion factor from conductivity in cm·day-1 to molH20·m-2·MPa-1·s-1
 *  1 day = 86400 sec
 *  1 mol H20 = 18.01528 g
 */
const double cmdTOmmolm2sMPa = 655.2934; //100.0/(18.01528*86400.0*0.00009804139432); 

// [[Rcpp::export("drainageCells")]]
IntegerVector drainageCells(List queenNeigh, List waterQ, int iCell) {
  IntegerVector cells = IntegerVector::create(iCell);
  IntegerVector neighbors = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[iCell-1]);
  int n = neighbors.size();
  for(int i=0;i<n;i++) {
    int nc = neighbors[i];
    IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[nc-1]);
    NumericVector qi = Rcpp::as<Rcpp::NumericVector>(waterQ[nc-1]);
    for(int j=0;j<ni.size();j++) {
      if((ni[j]==iCell) & (qi[j]>0.0)) {
        IntegerVector nicells = drainageCells(queenNeigh, waterQ, nc);
        for(int k=0;k<nicells.size();k++) {
          bool inBag = false;
          for(int l=0;l<cells.size();l++) {
            if(cells[l]==nicells[k]) inBag = true;
          }
          if(!inBag) cells.push_back(nicells[k]);
        }
      }
    }
  }
  return(cells);
}

// [[Rcpp::export(".getTrackSpeciesTranspiration")]]
NumericVector getTrackSpeciesTranspiration( NumericVector trackSpecies, NumericVector Eplant, DataFrame x) {
  int nTrackSpecies = trackSpecies.size();
  NumericVector Eplantsp(nTrackSpecies, 0.0);
  NumericVector SP = x["SP"];
  int nCoh = SP.size();
  int ts;
  for(int its =0;its<nTrackSpecies;its++) {
    ts = trackSpecies[its];
    for(int i=0;i<nCoh;i++) {
      if(SP[i]==ts) {
        Eplantsp[its] += Eplant[i];
      }
    }
  }
  return(Eplantsp);
}

// [[Rcpp::export(".getTrackSpeciesDDS")]]
NumericVector getTrackSpeciesDDS(NumericVector trackSpecies, NumericVector DDS, DataFrame x) {
  int nTrackSpecies = trackSpecies.size();
  NumericVector DDSsp(nTrackSpecies, 0.0);
  NumericVector LAI = x["LAI"];
  NumericVector SP = x["SP"];
  int nCoh = LAI.size();
  int ts;
  double laiSum;
  for(int its =0;its<nTrackSpecies;its++) {
    ts = trackSpecies[its];
    laiSum = 0.0;
    for(int i=0;i<nCoh;i++) {
      if(SP[i]==ts) {
        DDSsp[its] += DDS[i]*LAI[i];
        laiSum +=LAI[i];
      }
    }
    DDSsp = DDSsp/laiSum;
  }
  return(DDSsp);
}
// [[Rcpp::export(".copySnowpackToSoil")]]
void copySnowpackToSoil(List y) {
  CharacterVector lct = y["land_cover_type"];
  List xList = y["state"];
  NumericVector snowpack = y["snowpack"];
  int nX = xList.size();
  for(int i=0;i<nX;i++){
    if((lct[i]=="wildland") || (lct[i]=="agriculture") ) {
      List x = Rcpp::as<Rcpp::List>(xList[i]);
      List soil = Rcpp::as<Rcpp::List>(x["soil"]);
      soil["SWE"] = snowpack[i];
    }
  }
}

// [[Rcpp::export(".tetisModifyKsat")]]
void tetisModifyKsat(List y, List watershed_control, bool reverse) {
  List tetis_parameters = watershed_control["tetis_parameters"];
  CharacterVector lct = y["land_cover_type"];
  double R_localflow = tetis_parameters["R_localflow"];
  List xList = y["state"];
  int nX = xList.size();
  for(int i=0;i<nX;i++){
    if((lct[i]=="wildland") || (lct[i]=="agriculture") ) {
      List x = Rcpp::as<Rcpp::List>(xList[i]);
      List soil = Rcpp::as<Rcpp::List>(x["soil"]);
      NumericVector Ksat = soil["Ksat"];
      for(int l=0;l < Ksat.size();l++) {
        if(!reverse) {
          Ksat[l] = Ksat[l]*R_localflow;
        } else {
          Ksat[l] = Ksat[l]/R_localflow;
        }
      }
    }
  }
}

// [[Rcpp::export(".copySnowpackFromSoil")]]
void copySnowpackFromSoil(List y) {
  CharacterVector lct = y["land_cover_type"];
  List xList = y["state"];
  NumericVector snowpack = y["snowpack"];
  int nX = xList.size();
  for(int i=0;i<nX;i++){
    if((lct[i]=="wildland") || (lct[i]=="agriculture") ) {
      List x = Rcpp::as<Rcpp::List>(xList[i]);
      List soil = Rcpp::as<Rcpp::List>(x["soil"]);
      snowpack[i] = soil["SWE"];
    }
  }
}


// [[Rcpp::export(".tetisWatershedFlows")]]
DataFrame tetisWatershedFlows(List y,
                              IntegerVector waterO, List queenNeigh, List waterQ,
                              List watershed_control,
                              double patchsize) {
  
  CharacterVector lct = y["land_cover_type"];
  List xList = y["state"];
  int nX = xList.size();
  NumericVector depth_to_bedrock  = y["depth_to_bedrock"];
  NumericVector bedrock_conductivity = y["bedrock_conductivity"];
  NumericVector bedrock_porosity = y["bedrock_porosity"];
  NumericVector aquifer = y["aquifer"];
  NumericVector elevation = y["elevation"];
  
  List tetis_parameters = watershed_control["tetis_parameters"];
  double R_interflow = tetis_parameters["R_interflow"];
  double R_baseflow = tetis_parameters["R_baseflow"];
  
  //A. Subsurface fluxes
  double cellArea = patchsize; //cell size in m2
  double cellWidth = sqrt(patchsize); //cell width in m
  double n = 3.0;
  
  List x, soil, control;
  NumericVector dVec, Ksat;
  double D;
  
  //A1. Calculate soil and aquifer water table elevation (heads)
  NumericVector WTD(nX,NA_REAL); //Water table depth
  NumericVector SoilSaturatedLayerElevation(nX,NA_REAL); //water table elevation (including cell elevation) in meters
  NumericVector AquiferWaterTableElevation(nX,NA_REAL); //water table elevation (including cell elevation) in meters
  for(int i=0;i<nX;i++){
    if((lct[i]=="wildland") || (lct[i]=="agriculture") ) {
      x = Rcpp::as<Rcpp::List>(xList[i]);
      soil = Rcpp::as<Rcpp::List>(x["soil"]);
      control = x["control"];
      WTD[i] = medfate::soil_saturatedWaterDepth(soil, control["soilFunctions"]); //in mm
      if(!NumericVector::is_na(WTD[i])) {
        SoilSaturatedLayerElevation[i] = elevation[i]-(WTD[i]/1000.0); //in m
      } else {
        dVec = soil["dVec"];
        D = sum(dVec); //Soil depth in mm
        SoilSaturatedLayerElevation[i] = elevation[i] - (D/1000.0); //in m
      }
      
    }
    AquiferWaterTableElevation[i] = elevation[i]-(depth_to_bedrock[i]/1000.0) + (aquifer[i]/bedrock_porosity[i])/1000.0;
  }
  
  //A2a. Calculate INTERFLOW input/output for each cell (in m3/day)
  NumericVector interflowInput(nX, 0.0);
  NumericVector interflowOutput(nX, 0.0);
  for(int i=0;i<nX;i++){
    if((lct[i]=="wildland") || (lct[i]=="agriculture")) {
      if(!NumericVector::is_na(WTD[i])) {
        x = Rcpp::as<Rcpp::List>(xList[i]);
        soil = Rcpp::as<Rcpp::List>(x["soil"]);
        dVec = soil["dVec"];
        Ksat = soil["Ksat"];
        D = sum(dVec); //Soil depth in mm
        double Ks1 = 0.01*Ksat[0]/cmdTOmmolm2sMPa; //cm/day to m/day
        double Kinterflow = R_interflow*Ks1;
        if(WTD[i]<D) {
          double T = ((Kinterflow*D*0.001)/n)*pow(1.0-(WTD[i]/D),n); //Transmissivity in m2/day
          IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[i]);
          //water table slope between target and neighbours
          NumericVector qni(ni.size(), 0.0);
          for(int j=0;j<ni.size();j++) {
            if((lct[ni[j]-1]=="wildland") || (lct[ni[j]-1]=="agriculture")) { //Only flows to other wildland or agriculture cells
              double tanBeta = (SoilSaturatedLayerElevation[i]-SoilSaturatedLayerElevation[ni[j]-1])/cellWidth;
              if(!NumericVector::is_na(tanBeta)) {
                if(tanBeta>0.0) qni[j] = tanBeta*T*cellWidth; //flow in m3/day
              } else {
                qni[j] = 0.0;
              }
            }
          }
          for(int j=0;j<ni.size();j++) {
            if(qni[j]>0.0) {
              interflowInput[ni[j]-1] += 1000.0*qni[j]/patchsize; //in mm/day
              interflowOutput[i] += 1000.0*qni[j]/patchsize;
            }
          }
        }
      }
    }
  }
  
  //A2b. Calculate BASEFLOW output for each cell (in m3/day)
  NumericVector baseflowInput(nX, 0.0);
  NumericVector baseflowOutput(nX, 0.0);
  for(int i=0;i<nX;i++){
    double Kbaseflow = R_baseflow*bedrock_conductivity[i]; //m/day
    if(aquifer[i]>0) {
      double T = ((Kbaseflow*depth_to_bedrock[i]*0.001)/n)*pow(1.0-((depth_to_bedrock[i] - (aquifer[i]/bedrock_porosity[i]))/depth_to_bedrock[i]),n); //Transmissivity in m2
      IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[i]);
      //water table slope between target and neighbours
      NumericVector qni(ni.size(), 0.0);
      for(int j=0;j<ni.size();j++) {
        double tanBeta = (AquiferWaterTableElevation[i]-AquiferWaterTableElevation[ni[j]-1])/cellWidth;
        if(tanBeta>0.0) qni[j] = tanBeta*T*cellWidth; //flow in m3/day
      }
      double qntotal = sum(qni);
      double qntotalallowed = std::min(qntotal, (aquifer[i]/1000.0)*cellArea); //avoid excessive outflow
      double corrfactor = qntotalallowed/qntotal;
      for(int j=0;j<ni.size();j++) {
        if(qni[j]>0.0) {
          baseflowInput[ni[j]-1] += 1000.0*qni[j]*corrfactor/patchsize; // in mm/day
          baseflowOutput[i] += 1000.0*qni[j]*corrfactor/patchsize;
        }
      }
    }
  }
  DataFrame out = DataFrame::create(_["InterflowInput"] = interflowInput, _["InterflowOutput"] = interflowOutput,
                                    _["BaseflowInput"] = baseflowInput, _["BaseflowOutput"] = baseflowOutput);
  return(out);
}

// [[Rcpp::export(".tetisApplyBaseflowChangesToAquifer")]]
NumericVector tetisApplyBaseflowChangesToAquifer(List y,
                                                 NumericVector BaseflowInput, NumericVector BaseflowOutput,
                                                 double patchsize) {
  NumericVector depth_to_bedrock  = y["depth_to_bedrock"];
  NumericVector bedrock_porosity = y["bedrock_porosity"];
  NumericVector aquifer = y["aquifer"];

  int nX = aquifer.size();
  NumericVector AquiferExfiltration(nX, 0.0);
  for(int i=0;i<nX;i++){
    double deltaA = (BaseflowInput[i]-BaseflowOutput[i]); //change in moisture in mm (L/m2)
    aquifer[i] = aquifer[i] + deltaA; //New water amount in the aquifer (mm water)
    double DTAn = depth_to_bedrock[i] - (aquifer[i]/bedrock_porosity[i]); //New depth to aquifer (mm)
    if(DTAn < 0.0) { // Turn negative aquifer depth into aquifer discharge
      AquiferExfiltration[i] = - DTAn*bedrock_porosity[i];
      aquifer[i] = depth_to_bedrock[i]*bedrock_porosity[i];
    }
  }
  return(AquiferExfiltration);
}

// [[Rcpp::export(".tetisApplyLocalFlowsToAquifer")]]
void tetisApplyLocalFlowsToAquifer(List y,
                                   NumericVector CapillarityRise,
                                   NumericVector DeepDrainage) {
  NumericVector aquifer = y["aquifer"];
  
  int nX = aquifer.size();
  for(int i=0;i<nX;i++){
    aquifer[i] += DeepDrainage[i] - CapillarityRise[i];
  }
}
// [[Rcpp::export(".tetisOverlandFlows")]]
NumericVector tetisOverlandFlows( NumericVector Runoff, NumericVector AquiferExfiltration,
                                  NumericVector waterO, List queenNeigh, List waterQ) {
  int nX = Runoff.size();
  NumericVector Runon(nX,0.0);
  NumericVector RunoffExport(nX,0.0);
  for(int i=0;i<nX;i++) {
    //get next cell in order
    int iCell = waterO[i]-1; //Decrease index!!!!
    //Assign runoff to runon of downhill neighbours
    double ri_tot =  Runon[iCell] + Runoff[iCell] + AquiferExfiltration[iCell];
    if(ri_tot>0.0) {
      double ri = ri_tot;
      IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[iCell]);
      NumericVector qi = Rcpp::as<Rcpp::NumericVector>(waterQ[iCell]);
      if(ni.size()>0) {
        for(int j=0;j<ni.size();j++)  {
          Runon[ni[j]-1] += (qi[j]*ri_tot); //decrease index
          ri -= (qi[j]*ri_tot);
        }
      }
      if((sum(qi)>0.0) && (ri > 0.00001)) {
        Rcout<< i <<ni.size()<< " "<<qi.size()<<" "<<iCell<< " "<< sum(qi)<< " "<< ri<<"\n";
        stop("Non-outlet cell with runoff export");
      }
      if(sum(qi)==0.0) { // outlet
        RunoffExport[iCell] = ri;
      }
    }
  }
  return(RunoffExport);
}
  
// [[Rcpp::export(".tetisSimulationNonSoilCells")]]
DataFrame tetisSimulationNonSoilCells(List y,
                                      NumericVector tminVec, NumericVector tmaxVec, NumericVector precVec, NumericVector radVec) {
  
  CharacterVector lct = y["land_cover_type"];
  List xList = y["state"];
  int nX = xList.size();
  NumericVector elevation = y["elevation"];
  NumericVector snowpack = y["snowpack"];
  NumericVector Rain(nX, NA_REAL);
  NumericVector Runoff(nX, NA_REAL), DeepDrainage(nX, NA_REAL), Snow(nX, NA_REAL),  Snowmelt(nX, NA_REAL);
  
  for(int i=0;i<nX;i++){
    if(lct[i]=="rock" || lct[i]=="artificial" || lct[i]=="water") {
      Rain[i] = 0.0;
      Snow[i] = 0.0;
      Snowmelt[i] = 0.0;
      Runoff[i]  = 0.0;
      double tday = meteoland::utils_averageDaylightTemperature(tminVec[i], tmaxVec[i]);
      if(tday<0.0) {
        Snow[i] = precVec[i];
        snowpack[i] += Snow[i];
      } else {
        Rain[i] = precVec[i];
      }
      if(snowpack[i]>0.0) {
        double melt = medfate::hydrology_snowMelt(tday, radVec[i], 1.0, elevation[i]);
        Snowmelt[i] = std::min(melt, snowpack[i]);
        snowpack[i] -= Snowmelt[i];
      }
      if(lct[i]=="rock" || lct[i]=="artificial") {
        //all Precipitation becomes surface runoff if cell is rock outcrop/artificial
        Runoff[i] =  Snowmelt[i]+Rain[i];
        DeepDrainage[i] = 0.0;
      } else if(lct[i]=="water") {
        // water cells receive water from Precipitation
        // but do not export to the atmosphere contribute nor to other cells.
        // any received water drains directly to the aquifer so that it can feed base flow
        DeepDrainage[i] = Snowmelt[i]+ Rain[i];
        Runoff[i] = 0.0;
      }
      
       // Rcout << i<< " Rain " << Rain[i] << " Snow " << Snow[i] << " DeepDrainage " << DeepDrainage[i] << " Snowmelt " << Snowmelt[i] << " Runoff " << Runoff[i] << "\n";
    }
  }
  DataFrame out = DataFrame::create(_["Rain"] = Rain, _["Snow"] = Snow,
                                    _["Snowmelt"] = Snowmelt, 
                                    _["Runoff"] = Runoff, _["DeepDrainage"] = DeepDrainage);
  return(out);
}


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
        // NumericVector dVec = soil["dVec"];
        // for(int l=0;l<dVec.size();l++) Rcout<< dVec[l]<<" ";
        // Rcout<<"\n";
        NumericVector vup(W.size(), NA_REAL);
        uptake[sf2cell[i] - 1] = vup; //indices in R
      }
    }
  }
  // Initialize SERGHEI (call to interface function)
  // Use input_dir and output_dir
  // initializeSerghei(soilListSerghei, uptake, throughfall);
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
      List res_i = localResults[i];
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

