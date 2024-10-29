// [[Rcpp::depends(medfate,meteoland)]]
#include <Rcpp.h>
#include <meteoland.h>
#include <medfate.h>
#include "spwbland.h"
using namespace Rcpp;
using namespace medfate;
using namespace meteoland;

/**
 * Conversion factor from conductivity in cm·day-1 to molH20·m-2·MPa-1·s-1
 *  1 day = 86400 sec
 *  1 mol H20 = 18.01528 g
 */
const double cmdTOmmolm2sMPa = 655.2934; //100.0/(18.01528*86400.0*0.00009804139432); 

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

// [[Rcpp::export(".tetisInterFlow")]]
void tetisInterFlow(DataFrame outWB,
                    List y,
                    IntegerVector waterO, List queenNeigh, List waterQ,
                    List watershed_control,
                    double patchsize) {
  
  NumericVector interflowInput = outWB[WBCOM_InterflowInput];
  NumericVector interflowOutput = outWB[WBCOM_InterflowOutput];
  NumericVector interflowBalance = outWB[WBCOM_InterflowBalance];
  
  CharacterVector lct = y["land_cover_type"];
  List xList = y["state"];
  int nX = xList.size();
  NumericVector elevation = y["elevation"];
  
  List tetis_parameters = watershed_control["tetis_parameters"];
  double R_interflow = tetis_parameters["R_interflow"];
  double n_interflow = tetis_parameters["n_interflow"];
  int num_daily_substeps = tetis_parameters["num_daily_substeps"];
  
  //A. Subsurface fluxes
  double cellWidth = sqrt(patchsize); //cell width in m
  
  List x, soil, control;
  NumericVector widths, Ksat;
  
  //A1. Calculate soil and aquifer water table elevation (heads)
  LogicalVector is_soil(nX, false);
  NumericVector K_inter_max(nX,NA_REAL); //Maximum interflow conductivity
  NumericVector D(nX,NA_REAL); //Soil depth (mm)
  NumericVector WTD(nX,NA_REAL); //Water table depth (mm)
  NumericVector SoilSaturatedLayerElevation(nX,NA_REAL); //water table elevation (including cell elevation) in meters
  for(int i=0;i<nX;i++){
    if((lct[i]=="wildland") || (lct[i]=="agriculture") ) {
      is_soil[i] = true;
      x = Rcpp::as<Rcpp::List>(xList[i]);
      soil = Rcpp::as<Rcpp::List>(x["soil"]);
      widths = soil["widths"];
      Ksat = soil["Ksat"];
      control = x["control"];
      D[i] = sum(widths); //Soil depth in mm
      WTD[i] = medfate::soil_saturatedWaterDepth(soil, control["soilFunctions"]); //Depth to saturated layer in mm
      if(NumericVector::is_na(WTD[i])) WTD[i] = D[i]; //If missing, set to soil depth (should not create flow)
      SoilSaturatedLayerElevation[i] = elevation[i] - (WTD[i]/1000.0); //in m
      if(NumericVector::is_na(SoilSaturatedLayerElevation[i])) stop("Missing soil saturated elevation");
      double Ks1 = 0.01*Ksat[0]/cmdTOmmolm2sMPa; //cm/day to m/day
      K_inter_max[i] = R_interflow*Ks1/((double) num_daily_substeps); //Conductivity in m per substep
    }
  }
  
  //A2a. Calculate INTERFLOW input/output for each cell (in m3/day)
  for(int s = 0;s<num_daily_substeps;s++) {
    //Set step flows to zero
    NumericVector interflowInputStep(nX, 0.0);
    NumericVector interflowOutputStep(nX, 0.0);
    //Calculate step flows
    for(int i=0;i<nX;i++){
      if(is_soil[i]) {
        if(WTD[i]<D[i]) {
          double T = ((K_inter_max[i]*D[i]*0.001)/n_interflow)*pow(1.0-(WTD[i]/D[i]),n_interflow); //Transmissivity in m2/day
          IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[i]);
          NumericVector qni(ni.size(), 0.0);
          //water table slope between target and neighbours
          for(int j=0;j<ni.size();j++) {
            if(is_soil[ni[j]-1]) { //Only flows to other wildland or agriculture cells
              double tanBeta = (SoilSaturatedLayerElevation[i]-SoilSaturatedLayerElevation[ni[j]-1])/cellWidth;
              if(tanBeta>0.0) {
                double q_m3 = tanBeta*T*cellWidth; //flow in m3/substep 
                qni[j] = (1000.0*q_m3)/patchsize; //in mm/substep
              }
            }
          }
          double qntotal = sum(qni);
          if(qntotal>0.0) {
            double qntotalallowed = std::min(qntotal, D[i]- WTD[i]); //avoid excessive outflow
            double corrfactor = qntotalallowed/qntotal;
            if(NumericVector::is_na(corrfactor)) {
              Rcout<< D[i]<< " "<< WTD[i]<< " "<< qntotal<< " " << qntotalallowed << " " << corrfactor << "\n";
              stop("Missing corrfactor."); 
            }
            for(int j=0;j<ni.size();j++) {
              if(is_soil[ni[j]-1]) { //Only flows to other wildland or agriculture cells
                interflowInputStep[ni[j]-1] += qni[j]*corrfactor;
                interflowOutputStep[i] += qni[j]*corrfactor;
              }
            }
          }
        }
      }
    }
    
    //Update WTD and SoilSaturatedLayerElevation
    //Add substep input/output flows
    for(int i=0;i<nX;i++){
      if(is_soil[i]) {
        // subtract input (to make it shallower) and add output (to make it deeper)
        WTD[i] = WTD[i] - interflowInputStep[i] + interflowOutputStep[i];
        SoilSaturatedLayerElevation[i] = elevation[i]-(WTD[i]/1000.0); //in m
        if(NumericVector::is_na(interflowInputStep[i])) stop("Missing interflowInputStep.");
        if(NumericVector::is_na(interflowOutputStep[i])) stop("Missing interflowOutputStep.");
        interflowInput[i] +=interflowInputStep[i];
        interflowOutput[i] +=interflowOutputStep[i];
      }
    }
  }
  
  //A3. Balance
  double balsum =0.0;
  for(int i=0;i<nX;i++){
    if(is_soil[i]) {
      interflowBalance[i] = interflowInput[i] - interflowOutput[i];
      balsum +=interflowBalance[i];
    }
  }
  if(balsum>0.00001) stop("Non-negligible balance sum");
}

// [[Rcpp::export(".tetisBaseFlow")]]
void tetisBaseFlow(DataFrame outWB,
                   List y,
                   IntegerVector waterO, List queenNeigh, List waterQ,
                   List watershed_control,
                   double patchsize) {

  NumericVector baseflowInput = outWB[WBCOM_BaseflowInput];
  NumericVector baseflowOutput = outWB[WBCOM_BaseflowOutput];
  NumericVector baseflowBalance = outWB[WBCOM_BaseflowBalance];
  
  CharacterVector lct = y["land_cover_type"];
  List xList = y["state"];
  int nX = xList.size();
  NumericVector depth_to_bedrock  = y["depth_to_bedrock"];
  NumericVector bedrock_conductivity = y["bedrock_conductivity"];
  NumericVector bedrock_porosity = y["bedrock_porosity"];
  NumericVector aquifer = y["aquifer"];
  NumericVector elevation = y["elevation"];
  
  List tetis_parameters = watershed_control["tetis_parameters"];
  double R_baseflow = tetis_parameters["R_baseflow"];
  double n_baseflow = tetis_parameters["n_baseflow"];
  
  //A. Subsurface fluxes
  double cellWidth = sqrt(patchsize); //cell width in m
  
  //A1. Calculate soil and aquifer water table elevation (heads)
  NumericVector AquiferWaterTableElevation(nX,NA_REAL); //water table elevation (including cell elevation) in meters
  for(int i=0;i<nX;i++){
    AquiferWaterTableElevation[i] = elevation[i]-(depth_to_bedrock[i]/1000.0) + (aquifer[i]/bedrock_porosity[i])/1000.0;
  }
  
  //A2b. Calculate BASEFLOW output for each cell (in m3/day)
  for(int i=0;i<nX;i++){
    double Kbaseflow = R_baseflow*bedrock_conductivity[i]; //m/day
    if(aquifer[i]>0) {
      double T = ((Kbaseflow*depth_to_bedrock[i]*0.001)/n_baseflow)*pow(1.0-((depth_to_bedrock[i] - (aquifer[i]/bedrock_porosity[i]))/depth_to_bedrock[i]),n_baseflow); //Transmissivity in m2
      IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[i]);
      //water table slope between target and neighbours
      NumericVector qni(ni.size(), 0.0);
      for(int j=0;j<ni.size();j++) {
        double tanBeta = (AquiferWaterTableElevation[i]-AquiferWaterTableElevation[ni[j]-1])/cellWidth;
        if(tanBeta>0.0) qni[j] = tanBeta*T*cellWidth; //flow in m3/day
      }
      double qntotal = sum(qni);
      double qntotalallowed = std::min(qntotal, (aquifer[i]/1000.0)*patchsize); //avoid excessive outflow
      double corrfactor = qntotalallowed/qntotal;
      for(int j=0;j<ni.size();j++) {
        if(qni[j]>0.0) {
          baseflowInput[ni[j]-1] += 1000.0*qni[j]*corrfactor/patchsize; // in mm/day
          baseflowOutput[i] += 1000.0*qni[j]*corrfactor/patchsize;
        }
      }
    }
    // if(baseflowOutput[i]>aquifer[i]) {
    //   Rcerr<< " Baseoutflow greater than aquifer in "<< (i+1) <<"\n";
    //   Rcout<< baseflowOutput[i]<<" "<< aquifer[i] <<"\n";
    // }
  }
  
  //A3. Balance
  double balsum =0.0;
  for(int i=0;i<nX;i++){
    baseflowBalance[i] = baseflowInput[i] - baseflowOutput[i];
    balsum +=baseflowBalance[i];
  }
  if(balsum>0.00001) stop("Non-negligible baseflow balance sum");
}

// [[Rcpp::export(".tetisApplyBaseflowChangesToAquifer")]]
void tetisApplyBaseflowChangesToAquifer(DataFrame outWB,
                                        List y,
                                        double patchsize) {
  NumericVector depth_to_bedrock  = y["depth_to_bedrock"];
  NumericVector bedrock_porosity = y["bedrock_porosity"];
  NumericVector aquifer = y["aquifer"];
  NumericVector AquiferExfiltration = outWB[WBCOM_AquiferExfiltration];
  NumericVector baseflowBalance = outWB[WBCOM_BaseflowBalance];
  int nX = aquifer.size();
  for(int i=0;i<nX;i++){
    aquifer[i] = aquifer[i] + baseflowBalance[i]; //New water amount in the aquifer (mm water)
    if(aquifer[i] < 0.0) {
      // Rcerr << "negative aquifer in cell "<< (i+1)<<" after base flows\n";
      aquifer[i] = 0.0;
    }
    double DTAn = depth_to_bedrock[i] - (aquifer[i]/bedrock_porosity[i]); //New depth to aquifer (mm)
    if(DTAn < 0.0) { // Turn negative aquifer depth into aquifer discharge
      AquiferExfiltration[i] = - DTAn*bedrock_porosity[i];
      aquifer[i] = depth_to_bedrock[i]*bedrock_porosity[i];
    }
  }
}

// [[Rcpp::export(".tetisApplyLocalFlowsToAquifer")]]
void tetisApplyLocalFlowsToAquifer(List y,
                                   DataFrame outWB) {
  NumericVector DeepDrainage =  outWB[WBCOM_DeepDrainage];
  NumericVector CapillarityRise =  outWB[WBCOM_CapillarityRise];
  
  NumericVector aquifer = y["aquifer"];
  
  int nX = aquifer.size();
  for(int i=0;i<nX;i++){
    aquifer[i] = aquifer[i] + DeepDrainage[i] - CapillarityRise[i];
    if(aquifer[i]< 0.0) {
      // Rcerr << "negative aquifer in cell "<< (i+1)<<" after local flows\n";
      // Rcout << DeepDrainage[i]<< " " << CapillarityRise[i]<<"\n";
      aquifer[i] = 0.0;
    }
  }
}
// [[Rcpp::export(".tetisApplyDeepAquiferLossToAquifer")]]
void tetisApplyDeepAquiferLossToAquifer(DataFrame outWB, List y,
                                        List watershed_control) {
  
  NumericVector DeepAquiferLoss =  outWB[WBCOM_DeepAquiferLoss];
  
  List tetis_parameters = watershed_control["tetis_parameters"];
  double deep_aquifer_loss = tetis_parameters["deep_aquifer_loss"];
  
  NumericVector aquifer = y["aquifer"];
  int nX = aquifer.size();
  NumericVector loss_rate(nX, deep_aquifer_loss); 
  if(y.containsElementNamed("deep_aquifer_loss")) {
    loss_rate = Rcpp::as<Rcpp::NumericVector>(y["deep_aquifer_loss"]);
  }
  for(int i=0;i<nX;i++){
    DeepAquiferLoss[i] = std::max(std::min(aquifer[i], loss_rate[i]), 0.0);
    aquifer[i] -= DeepAquiferLoss[i];
  }
}

// [[Rcpp::export(".tetisOverlandFlows")]]
void tetisOverlandFlows(DataFrame outWB,
                        IntegerVector waterO, List queenNeigh, List waterQ) {
  
  NumericVector WatershedExport=  outWB[WBCOM_WatershedExport];
  NumericVector Runoff=  outWB[WBCOM_Runoff];
  NumericVector AquiferExfiltration =  outWB[WBCOM_AquiferExfiltration];
  
  int nX = Runoff.size();
  // Here runon is an independent vector because runon from rock/artificial/water cells 
  // has already taken into account previously. This is to process runoff from water balance
  // and aquifer exfiltration
  NumericVector Runon(nX,0.0);

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
        WatershedExport[iCell] += ri;
      }
    }
  }
}

// [[Rcpp::export(".tetisSimulationNonSoilCells")]]
void tetisSimulationNonSoilCells(DataFrame outWB,
                                 List y,
                                 NumericVector tminVec, NumericVector tmaxVec, NumericVector precVec, NumericVector radVec,
                                 IntegerVector waterO, List queenNeigh, List waterQ,
                                 List watershed_control) {
  
  NumericVector Rain = outWB[WBCOM_Rain];
  NumericVector Interception = outWB[WBCOM_Interception];
  NumericVector NetRain = outWB[WBCOM_NetRain];
  NumericVector Runoff = outWB[WBCOM_Runoff];
  NumericVector Infiltration = outWB[WBCOM_Infiltration];
  NumericVector InfiltrationExcess = outWB[WBCOM_InfiltrationExcess];
  NumericVector DeepDrainage = outWB[WBCOM_DeepDrainage];
  NumericVector Snow = outWB[WBCOM_Snow];
  NumericVector Snowmelt = outWB[WBCOM_Snowmelt];
  NumericVector Runon = outWB[WBCOM_Runon];
  NumericVector WatershedExport = outWB[WBCOM_WatershedExport];
  
  List tetis_parameters = watershed_control["tetis_parameters"];
  double rock_max_infiltration = tetis_parameters["rock_max_infiltration"];
  
  CharacterVector lct = y["land_cover_type"];
  List xList = y["state"];
  int nX = xList.size();
  NumericVector elevation = y["elevation"];
  NumericVector snowpack = y["snowpack"];
  
  for(int i=0;i<nX;i++){
    if(lct[i]=="rock" || lct[i]=="artificial" || lct[i]=="water") {
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
      if(lct[i]=="rock") {
        //Part of the water is allowed to infiltrate (draining to the aquifer)
        Infiltration[i] = std::min(rock_max_infiltration, Snowmelt[i]+Rain[i]);
        DeepDrainage[i] = Infiltration[i];
        InfiltrationExcess[i] = Snowmelt[i]+Rain[i] - DeepDrainage[i];
        Runoff[i] = InfiltrationExcess[i];
      } else if(lct[i]=="artificial") {
        //all Precipitation becomes surface runoff if cell is rock artificial
        InfiltrationExcess[i] =  Snowmelt[i]+Rain[i];
        Runoff[i] = InfiltrationExcess[i];
      } else if(lct[i]=="water") {
        // water cells receive water from Precipitation
        // but do not export to the atmosphere contribute nor to other cells.
        // any received water drains directly to the aquifer so that it can feed base flow
        DeepDrainage[i] = Snowmelt[i]+ Rain[i];
        Infiltration[i] = DeepDrainage[i];
      }
      NetRain[i] = Rain[i];
      // Rcout << i<< " Rain " << Rain[i] << " Snow " << Snow[i] << " DeepDrainage " << DeepDrainage[i] << " Snowmelt " << Snowmelt[i] << " Runoff " << Runoff[i] << "\n";
    }
  }
  //Estimate Runon to wildland/agriculture cells
  for(int i=0;i<nX;i++) {
    //get next cell in order
    int iCell = waterO[i]-1; //Decrease index!!!!
    //Only distribute runoff if the cell is rock or artificial
    if(lct[iCell]=="rock" || lct[iCell]=="artificial") {
      //Assign runoff to runon of downhill neighbours
      double ri_tot =  Runon[iCell] + Runoff[iCell];
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
        if(sum(qi)==0.0) { // outlet rock/artificial cell
          WatershedExport[iCell] += ri;
        }
      }      
    }
  }
}

// [[Rcpp::export(".tetisCopySoilResultsToOutput")]]
void tetisCopySoilResultsToOutput(List y, List soilCellResults, List output,
                                  NumericVector tminVec, NumericVector tmaxVec){
  
  CharacterVector land_cover_type = y["land_cover_type"];
  
  DataFrame outWB = as<DataFrame>(output["WatershedWaterBalance"]);
  List localResults = output["LocalResults"];
  int nX = outWB.nrow();
  
  NumericVector InterflowInput = outWB[WBCOM_InterflowInput];
  NumericVector InterflowOutput = outWB[WBCOM_InterflowOutput];
  NumericVector InterflowBalance = outWB[WBCOM_InterflowBalance];
  
  NumericVector BaseflowInput = outWB[WBCOM_BaseflowInput];
  NumericVector BaseflowOutput = outWB[WBCOM_BaseflowOutput];
  NumericVector BaseflowBalance = outWB[WBCOM_BaseflowBalance];
  
  NumericVector AquiferExfiltration = outWB[WBCOM_AquiferExfiltration];
  
  NumericVector MinTemperature = outWB[WBCOM_MinTemperature];
  NumericVector MaxTemperature = outWB[WBCOM_MaxTemperature];
  NumericVector Rain = outWB[WBCOM_Rain];
  NumericVector Interception = outWB[WBCOM_Interception];
  NumericVector NetRain = outWB[WBCOM_NetRain];
  NumericVector Runoff = outWB[WBCOM_Runoff];
  NumericVector Infiltration = outWB[WBCOM_Infiltration];
  NumericVector InfiltrationExcess = outWB[WBCOM_InfiltrationExcess];
  NumericVector DeepDrainage = outWB[WBCOM_DeepDrainage];
  NumericVector Snow = outWB[WBCOM_Snow];
  NumericVector Snowmelt = outWB[WBCOM_Snowmelt];
  NumericVector Runon = outWB[WBCOM_Runon];
  NumericVector CapillarityRise = outWB[WBCOM_CapillarityRise];
  NumericVector DeepAquiferLoss = outWB[WBCOM_DeepAquiferLoss];
  NumericVector SoilEvaporation = outWB[WBCOM_SoilEvaporation];
  NumericVector PET = outWB[WBCOM_PET];
  NumericVector WatershedExport = outWB[WBCOM_WatershedExport];
  NumericVector SaturationExcess = outWB[WBCOM_SaturationExcess];
  NumericVector Transpiration = outWB[WBCOM_Transpiration];
  NumericVector HerbTranspiration = outWB[WBCOM_HerbTranspiration];
  
  for(int i=0;i<nX;i++){
    if((land_cover_type[i] == "wildland") || (land_cover_type[i] == "agriculture")) {
      List lr = soilCellResults[i];
      localResults[i] = lr;
      List sr = lr["simulation_results"];
      NumericVector DB = sr["WaterBalance"];
      MinTemperature[i] = tminVec[i];
      MaxTemperature[i] = tmaxVec[i];
      Snow[i] = DB["Snow"];
      Snowmelt[i] = DB["Snowmelt"];
      PET[i] = DB["PET"];
      Rain[i] = DB["Rain"];
      SoilEvaporation[i] = DB["SoilEvaporation"];
      NetRain[i] = DB["NetRain"];
      Interception[i] = Rain[i] - NetRain[i];
      Infiltration[i] = DB["Infiltration"];
      Runoff[i] = DB["Runoff"];
      InfiltrationExcess[i] = DB["InfiltrationExcess"];
      SaturationExcess[i] = DB["SaturationExcess"];
      DeepDrainage[i] = DB["DeepDrainage"];
      CapillarityRise[i] = DB["CapillarityRise"];
      if(DeepDrainage[i] > CapillarityRise[i]) {
        DeepDrainage[i] = DeepDrainage[i] - CapillarityRise[i];
        CapillarityRise[i] = 0.0;
      }
      Transpiration[i] = DB["Transpiration"];
      if(land_cover_type[i]=="wildland") {
        HerbTranspiration[i] = DB["HerbTranspiration"];
      }
    }
  }
}
