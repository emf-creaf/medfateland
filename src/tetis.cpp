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
                    IntegerVector waterOrder, List queenNeigh, List waterQ,
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
  
  //A1. Calculate soil water table elevation (heads)
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
                   IntegerVector waterOrder, List queenNeigh, List waterQ,
                   LogicalVector isChannel, LogicalVector isOutlet,
                   List watershed_control,
                   double patchsize) {

  NumericVector baseflowInput = outWB[WBCOM_BaseflowInput];
  NumericVector baseflowOutput = outWB[WBCOM_BaseflowOutput];
  NumericVector baseflowBalance = outWB[WBCOM_BaseflowBalance];
  NumericVector AquiferExfiltration = outWB[WBCOM_AquiferExfiltration];
  NumericVector ChannelExport =  outWB[WBCOM_ChannelExport];
  NumericVector WatershedExport =  outWB[WBCOM_WatershedExport];
  NumericVector DeepDrainage =  outWB[WBCOM_DeepDrainage];
  NumericVector CapillarityRise =  outWB[WBCOM_CapillarityRise];
  NumericVector NegativeAquiferCorrection =  outWB[WBCOM_NegativeAquiferCorrection];
  
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
  int num_daily_substeps = tetis_parameters["num_daily_substeps"];
  
  //A. Subsurface fluxes
  double cellWidth = sqrt(patchsize); //cell width in m
  
  NumericVector baseflowBalance_step(nX,NA_REAL);
  NumericVector baseflowInput_step(nX,NA_REAL);
  NumericVector baseflowOutput_step(nX,NA_REAL);
  NumericVector AquiferWaterTableElevation(nX,NA_REAL); //water table elevation (including cell elevation) in meters
  
  double tfactor = 1.0/((double) num_daily_substeps);
  for(int d=0;d<num_daily_substeps;d++) {
    //A1. Add vertical flows and calculate aquifer water table elevation (heads in m)
    for(int i=0;i<nX;i++){
      baseflowInput_step[i] = 0.0;
      baseflowOutput_step[i] = 0.0;
      //Distribute deep drainage among subdaily time steps
      aquifer[i] += tfactor*(DeepDrainage[i] - CapillarityRise[i]);
      //Correct for capillarity when aquifer is empty
      if(aquifer[i] < 0.0) {
        NegativeAquiferCorrection[i] += (-1.0)*aquifer[i];
        aquifer[i] = 0.0;
      }
      // Calculate aquifer water table elevation (heads in m)
      AquiferWaterTableElevation[i] = elevation[i]-(depth_to_bedrock[i]/1000.0) + (aquifer[i]/bedrock_porosity[i])/1000.0;
    }
    //A2b. Calculate BASEFLOW output for each cell
    for(int i=0;i<nX;i++){
      double Kbaseflow = R_baseflow*bedrock_conductivity[i]; //m/day
      if(aquifer[i]>0) {
        double T = ((Kbaseflow*depth_to_bedrock[i]*0.001)/n_baseflow)*pow(1.0-((depth_to_bedrock[i] - (aquifer[i]/bedrock_porosity[i]))/depth_to_bedrock[i]),n_baseflow); //Transmissivity in m2
        IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[i]);
        //water table slope between target and neighbours
        NumericVector qni(ni.size(), 0.0);
        for(int j=0;j<ni.size();j++) {
          double tanBeta = (AquiferWaterTableElevation[i]-AquiferWaterTableElevation[ni[j]-1])/cellWidth;
          if(tanBeta>0.0) {
            qni[j] = tanBeta*T*cellWidth; //flow in m3/day
            qni[j] *= tfactor; //Apply reduction factor for multiple num_daily_substeps
          }
        }
        double qntotal = sum(qni);
        double qntotalallowed = std::min(qntotal, (aquifer[i]/1000.0)*patchsize); //avoid excessive outflow
        double corrfactor = qntotalallowed/qntotal;
        for(int j=0;j<ni.size();j++) {
          if(qni[j]>0.0) {
            baseflowInput_step[ni[j]-1] += 1000.0*qni[j]*corrfactor/patchsize; // in mm/day
            baseflowOutput_step[i] += 1000.0*qni[j]*corrfactor/patchsize;
          }
        }
      }
    }
    
    //Apply changes to aquifer  
    double balsum =0.0;
    for(int i=0;i<nX;i++){
      //Balance for this subdaily time step
      baseflowBalance_step[i] = baseflowInput_step[i] - baseflowOutput_step[i];
      aquifer[i] = aquifer[i] + baseflowBalance_step[i]; //New water amount in the aquifer (mm water)
      baseflowBalance[i] += baseflowBalance_step[i];
      baseflowInput[i] += baseflowInput_step[i];
      baseflowOutput[i] += baseflowOutput_step[i];
      balsum += baseflowBalance_step[i];
      double DTAn = depth_to_bedrock[i] - (aquifer[i]/bedrock_porosity[i]); //New depth to aquifer (mm)
      if((DTAn < 0.0) && (isChannel[i] || isOutlet[i])) { // Turn negative depth to aquifer into aquifer discharge
        double offset = -1.0*DTAn*bedrock_porosity[i];
        AquiferExfiltration[i] += offset;
        aquifer[i] -= offset;
        if(isChannel[i]) {
          ChannelExport[i] += offset;
        } else {
          WatershedExport[i] += offset;
        }
      }
    }
    if(balsum>0.00001) stop("Non-negligible baseflow balance sum");
  }
}


// [[Rcpp::export(".tetisDeepAquiferLossToAquifer")]]
void tetisDeepAquiferLossToAquifer(DataFrame outWB, List y,
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

// [[Rcpp::export(".tetisSimulationWithOverlandFlows")]]
void tetisSimulationWithOverlandFlows(String model, CharacterVector date, List internalCommunication,
                                      bool standSummary, bool carbonBalanceSummary, bool biomassBalanceSummary,
                                      List output,
                                      List y,
                                      NumericVector latitude,
                                      DataFrame gridMeteo,
                                      IntegerVector waterOrder, List queenNeigh, List waterQ, LogicalVector isChannel,
                                      List watershed_control) {
  
  DataFrame outWB = Rcpp::as<Rcpp::DataFrame>(output["WatershedWaterBalance"]);
  List localResults = output["LocalResults"];
  
  List tetis_parameters = watershed_control["tetis_parameters"];
  double rock_max_infiltration = tetis_parameters["rock_max_infiltration"];

  NumericVector Runoff=  outWB[WBCOM_Runoff];
  NumericVector Runon=  outWB[WBCOM_Runon];
  NumericVector MinTemperature = outWB[WBCOM_MinTemperature];
  NumericVector MaxTemperature = outWB[WBCOM_MaxTemperature];
  NumericVector PET = outWB[WBCOM_PET];
  NumericVector Rain = outWB[WBCOM_Rain];
  NumericVector Interception = outWB[WBCOM_Interception];
  NumericVector NetRain = outWB[WBCOM_NetRain];
  NumericVector Infiltration = outWB[WBCOM_Infiltration];
  NumericVector InfiltrationExcess = outWB[WBCOM_InfiltrationExcess];
  NumericVector InterflowBalance = outWB[WBCOM_InterflowBalance];
  NumericVector DeepDrainage = outWB[WBCOM_DeepDrainage];
  NumericVector Snow = outWB[WBCOM_Snow];
  NumericVector Snowmelt = outWB[WBCOM_Snowmelt];
  NumericVector ChannelExport = outWB[WBCOM_ChannelExport];
  NumericVector WatershedExport = outWB[WBCOM_WatershedExport];
  NumericVector InterflowInput = outWB[WBCOM_InterflowInput];
  NumericVector InterflowOutput = outWB[WBCOM_InterflowOutput];
  NumericVector BaseflowInput = outWB[WBCOM_BaseflowInput];
  NumericVector BaseflowOutput = outWB[WBCOM_BaseflowOutput];
  NumericVector BaseflowBalance = outWB[WBCOM_BaseflowBalance];
  NumericVector AquiferExfiltration = outWB[WBCOM_AquiferExfiltration];
  NumericVector CapillarityRise = outWB[WBCOM_CapillarityRise];
  NumericVector DeepAquiferLoss = outWB[WBCOM_DeepAquiferLoss];
  NumericVector SoilEvaporation = outWB[WBCOM_SoilEvaporation];
  NumericVector SaturationExcess = outWB[WBCOM_SaturationExcess];
  NumericVector Transpiration = outWB[WBCOM_Transpiration];
  NumericVector HerbTranspiration = outWB[WBCOM_HerbTranspiration];
  NumericVector LAI, LAIherb, LAIlive, LAIexpanded, LAIdead, Cm, LgroundPAR, LgroundSWR;
  NumericVector StructuralBalance, LabileBalance, PlantBalance, MortalityLoss, CohortBalance;
  NumericVector GrossPrimaryProduction, MaintenanceRespiration, SynthesisRespiration, NetPrimaryProduction;
  if(standSummary) {
    DataFrame outStand = as<DataFrame>(output["WatershedStand"]);
    LAI = outStand[STCOM_LAI];
    LAIherb = outStand[STCOM_LAIherb];
    LAIlive = outStand[STCOM_LAIlive];
    LAIexpanded = outStand[STCOM_LAIexpanded];
    LAIdead = outStand[STCOM_LAIdead];
    Cm = outStand[STCOM_Cm];
    LgroundPAR = outStand[STCOM_LgroundPAR];
    LgroundSWR = outStand[STCOM_LgroundSWR];
  }
  if(carbonBalanceSummary) {
    DataFrame outCB = as<DataFrame>(output["WatershedCarbonBalance"]);
    GrossPrimaryProduction = outCB[CBCOM_GrossPrimaryProduction];
    MaintenanceRespiration = outCB[CBCOM_MaintenanceRespiration];
    SynthesisRespiration = outCB[CBCOM_SynthesisRespiration];
    NetPrimaryProduction = outCB[CBCOM_NetPrimaryProduction];
  }
  if(biomassBalanceSummary) {
    DataFrame outBB = as<DataFrame>(output["WatershedBiomassBalance"]);
     StructuralBalance = outBB[BBCOM_StructuralBalance];
     LabileBalance = outBB[BBCOM_LabileBalance];
     PlantBalance = outBB[BBCOM_PlantBalance];
     MortalityLoss = outBB[BBCOM_MortalityLoss];
     CohortBalance = outBB[BBCOM_CohortBalance];
  }
    
  NumericVector tminVec = Rcpp::as<Rcpp::NumericVector>(gridMeteo["MinTemperature"]);
  NumericVector tmaxVec = Rcpp::as<Rcpp::NumericVector>(gridMeteo["MaxTemperature"]);
  NumericVector rhminVec = Rcpp::as<Rcpp::NumericVector>(gridMeteo["MinRelativeHumidity"]);
  NumericVector rhmaxVec = Rcpp::as<Rcpp::NumericVector>(gridMeteo["MaxRelativeHumidity"]);
  NumericVector precVec = Rcpp::as<Rcpp::NumericVector>(gridMeteo["Precipitation"]);
  NumericVector radVec = Rcpp::as<Rcpp::NumericVector>(gridMeteo["Radiation"]);
  NumericVector wsVec = Rcpp::as<Rcpp::NumericVector>(gridMeteo["WindSpeed"]);
  NumericVector C02Vec = Rcpp::as<Rcpp::NumericVector>(gridMeteo["CO2"]);

  CharacterVector lct = y["land_cover_type"];
  List xList = y["state"];
  int nX = xList.size();
  NumericVector elevation = y["elevation"];
  NumericVector slope = y["slope"];
  NumericVector aspect = y["aspect"];
  NumericVector snowpack = y["snowpack"];
  NumericVector depth_to_bedrock = y["depth_to_bedrock"];
  NumericVector bedrock_porosity = y["bedrock_porosity"];
  NumericVector aquifer = y["aquifer"];
  LogicalVector result_cell = y["result_cell"]; 
  
  NumericVector meteovec = NumericVector::create(_["MinTemperature"] = NA_REAL,
                                                 _["MaxTemperature"] = NA_REAL,
                                                 _["MinRelativeHumidity"] = NA_REAL,
                                                 _["MaxRelativeHumidity"] = NA_REAL,
                                                 _["Precipitation"] = NA_REAL,
                                                 _["Radiation"] = NA_REAL,
                                                 _["WindSpeed"] = NA_REAL,    
                                                 _["CO2"] = NA_REAL);
  List XI = List::create(_["i"] = NA_INTEGER, 
                         _["x"] = NULL,
                         _["result_cell"] = NA_LOGICAL,
                         _["meteovec"] = meteovec,
                         _["latitude"] = NA_REAL, 
                         _["elevation"] = NA_REAL, 
                         _["slope"] = NA_REAL, 
                         _["aspect"] = NA_REAL,
                         _["runon"] = NA_REAL, 
                         _["lateralFlows"] = NULL,
                         _["waterTableDepth"] = NA_REAL); 
  List lr;
  for(int i=0;i<nX;i++) {
    //get next cell in order
    int iCell = waterOrder[i]-1; //Decrease index!!!!
    if(lct[iCell]=="wildland" || lct[iCell]=="agriculture") {
      //Soil cell: Prepare input
      meteovec["MinTemperature"] = tminVec[iCell];
      meteovec["MaxTemperature"] = tmaxVec[iCell];
      meteovec["MinRelativeHumidity"] = rhminVec[iCell];
      meteovec["MaxRelativeHumidity"] = rhmaxVec[iCell];
      meteovec["Precipitation"] = precVec[iCell];
      meteovec["Radiation"] = radVec[iCell];
      meteovec["WindSpeed"] = wsVec[iCell];
      meteovec["CO2"] = C02Vec[iCell];
      
      double wtd = std::max(0.0, depth_to_bedrock[iCell] - (aquifer[iCell]/bedrock_porosity[iCell]));

      List xi = xList[iCell];
      List soil_i = xi["soil"];
      NumericVector widths = Rcpp::as<Rcpp::NumericVector>(soil_i["widths"]);
      NumericVector rfc = Rcpp::as<Rcpp::NumericVector>(soil_i["rfc"]);
      double interflowbalance = InterflowBalance[iCell];
      NumericVector wl = widths*(1.0 - (rfc/100.0));
      NumericVector lateralFlows = (interflowbalance*wl)/sum(wl);

      //Replace values
      XI["i"] = iCell; 
      XI["x"] = xi;
      XI["result_cell"] = result_cell[iCell];
      XI["latitude"] = latitude[iCell]; 
      XI["elevation"] = elevation[iCell]; 
      XI["slope"] = slope[iCell]; 
      XI["aspect"] = aspect[iCell];
      XI["runon"] = Runon[iCell];
      XI["lateralFlows"] = lateralFlows;
      XI["waterTableDepth"] = wtd; 
      //Launch simulation
      lr = fcpp_landunit_day(XI, model, date, internalCommunication,
                             standSummary, carbonBalanceSummary, biomassBalanceSummary);
      
      //Copy water balance
      localResults[iCell] = lr;
      List sr = lr["simulation_results"];
      NumericVector DB = sr["WaterBalance"];
      MinTemperature[iCell] = tminVec[iCell];
      MaxTemperature[iCell] = tmaxVec[iCell];
      Snow[iCell] = DB["Snow"];
      Snowmelt[iCell] = DB["Snowmelt"];
      PET[iCell] = DB["PET"];
      Rain[iCell] = DB["Rain"];
      SoilEvaporation[iCell] = DB["SoilEvaporation"];
      NetRain[iCell] = DB["NetRain"];
      Interception[iCell] = Rain[iCell] - NetRain[iCell];
      Infiltration[iCell] = DB["Infiltration"];
      Runoff[iCell] = DB["Runoff"];
      InfiltrationExcess[iCell] = DB["InfiltrationExcess"];
      SaturationExcess[iCell] = DB["SaturationExcess"];
      DeepDrainage[iCell] = DB["DeepDrainage"];
      CapillarityRise[iCell] = DB["CapillarityRise"];
      // if(DeepDrainage[iCell] > CapillarityRise[iCell]) {
      //   DeepDrainage[iCell] = DeepDrainage[iCell] - CapillarityRise[iCell];
      //   CapillarityRise[iCell] = 0.0;
      //   DB["CapillarityRise"] = CapillarityRise[iCell];
      //   DB["DeepDrainage"] = DeepDrainage[iCell];
      // } else if (DeepDrainage[iCell] < CapillarityRise[iCell]) {
      //   CapillarityRise[iCell] = CapillarityRise[iCell] - DeepDrainage[iCell];
      //   DeepDrainage[iCell] = 0.0;
      //   DB["CapillarityRise"] = CapillarityRise[iCell];
      //   DB["DeepDrainage"] = DeepDrainage[iCell];
      // }
      // if((AquiferExfiltration[iCell]>0.0) || (SaturationExcess[iCell]>0.0)) { //If soil is saturated deep drainage cannot occur
      //   SaturationExcess[iCell] +=DeepDrainage[iCell];
      //   Runoff[iCell] +=DeepDrainage[iCell];
      //   DeepDrainage[iCell] = 0.0;
      //   DB["SaturationExcess"] = SaturationExcess[iCell];
      //   DB["Runoff"] = Runoff[iCell];
      //   DB["DeepDrainage"] = DeepDrainage[iCell];
      // }
      Transpiration[iCell] = DB["Transpiration"];
      if(lct[iCell]=="wildland") {
        HerbTranspiration[iCell] = DB["HerbTranspiration"];
        if(standSummary){
          NumericVector Stand = sr["Stand"];
          LAI[iCell] = Stand["LAI"];
          LAIherb[iCell] = Stand["LAIherb"];
          LAIlive[iCell] = Stand["LAIlive"];
          LAIexpanded[iCell] = Stand["LAIexpanded"];
          LAIdead[iCell] = Stand["LAIdead"];
          Cm[iCell] = Stand["Cm"];
          LgroundPAR[iCell] = Stand["LgroundPAR"];
          LgroundSWR[iCell] = Stand["LgroundSWR"];
        } 
        if(carbonBalanceSummary){
          NumericVector CarbonBalance = sr["CarbonBalance"];
          GrossPrimaryProduction[iCell] = CarbonBalance["GrossPrimaryProduction"];
          MaintenanceRespiration[iCell] = CarbonBalance["MaintenanceRespiration"];
          SynthesisRespiration[iCell] = CarbonBalance["SynthesisRespiration"];
          NetPrimaryProduction[iCell] = CarbonBalance["NetPrimaryProduction"];
        }
        if(biomassBalanceSummary) {
          DataFrame pbb = Rcpp::as<Rcpp::DataFrame>(sr["PlantBiomassBalance"]);
          StructuralBalance[iCell] = sum(Rcpp::as<Rcpp::NumericVector>(pbb["StructuralBiomassBalance"]));
          LabileBalance[iCell] = sum(Rcpp::as<Rcpp::NumericVector>(pbb["LabileBiomassBalance"]));
          PlantBalance[iCell] = sum(Rcpp::as<Rcpp::NumericVector>(pbb["PlantBiomassBalance"]));
          MortalityLoss[iCell] = sum(Rcpp::as<Rcpp::NumericVector>(pbb["MortalityBiomassLoss"]));
          CohortBalance[iCell] = sum(Rcpp::as<Rcpp::NumericVector>(pbb["CohortBiomassBalance"]));
        }
      }
    } else if(lct[iCell]=="rock" || lct[iCell]=="artificial" || lct[iCell]=="water") {
      //NON-SOIL CELL
      MinTemperature[iCell] = tminVec[iCell];
      MaxTemperature[iCell] = tmaxVec[iCell];
      PET[iCell] = NA_REAL;
      double tday = meteoland::utils_averageDaylightTemperature(tminVec[iCell], tmaxVec[iCell]);
      if(tday<0.0) {
        Snow[iCell] = precVec[iCell];
        snowpack[iCell] += Snow[iCell];
      } else {
        Rain[iCell] = precVec[iCell];
      }
      if(snowpack[iCell]>0.0) {
        double melt = medfate::hydrology_snowMelt(tday, radVec[iCell], 1.0, elevation[iCell]);
        Snowmelt[iCell] = std::min(melt, snowpack[iCell]);
        snowpack[iCell] -= Snowmelt[iCell];
      }
      if(lct[iCell]=="rock") {
        //Part of the water is allowed to infiltrate (draining to the aquifer)
        Infiltration[iCell] = std::min(rock_max_infiltration, Snowmelt[iCell]+Rain[iCell]);
        DeepDrainage[iCell] = Infiltration[iCell];
        InfiltrationExcess[iCell] = Snowmelt[iCell]+Rain[iCell] - DeepDrainage[iCell];
        Runoff[iCell] = InfiltrationExcess[iCell];
      } else if(lct[iCell]=="artificial") {
        //all Precipitation becomes surface runoff if cell is rock artificial
        InfiltrationExcess[iCell] =  Snowmelt[iCell]+Rain[iCell];
        Runoff[iCell] = InfiltrationExcess[iCell];
      } else if(lct[iCell]=="water") {
        // water cells receive water from Precipitation
        // but do not export to the atmosphere contribute nor to other cells.
        // any received water drains directly to the aquifer so that it can feed base flow
        DeepDrainage[iCell] = Snowmelt[iCell]+ Rain[iCell];
        Infiltration[iCell] = DeepDrainage[iCell];
      }
      NetRain[iCell] = Rain[iCell];
      // Rcout << i<< " Rain " << Rain[i] << " Snow " << Snow[i] << " DeepDrainage " << DeepDrainage[i] << " Snowmelt " << Snowmelt[i] << " Runoff " << Runoff[i] << "\n";
    }
    
    // OVERLAND RUNOFF
    // Assign runoff to runon of downhill neighbours
    double ri_tot =  Runoff[iCell];
    NumericVector qi = Rcpp::as<Rcpp::NumericVector>(waterQ[iCell]);
    IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[iCell]);
    // Add aquifer exfiltration to the water to be distributed if not an outlet or channel
    if(ri_tot>0.0) {
      double ri = ri_tot;
      if(ni.size()>0) {
        for(int j=0;j<ni.size();j++)  {
          Runon[ni[j]-1] += (qi[j]*ri_tot); //decrease index
          ri -= (qi[j]*ri_tot);
        }
      }
      if(sum(qi)==0.0 || isChannel[iCell]) { // outlet or channel
        if(isChannel[iCell]) {
          ChannelExport[iCell] += ri;
        } else {
          WatershedExport[iCell] += ri;
        }
      } else if(ri > 0.000001) {
        Rcout<< i <<ni.size()<< " "<<qi.size()<<" "<<iCell<< " "<< sum(qi)<< " "<< ri<<"\n";
        stop("Non-outlet or channel cell with runoff export");
      }
    }
  }
}

// [[Rcpp::export(".tetisWatershedDay")]]
void tetisWatershedDay(List output,
                       List internalCommunication,
                       String local_model,
                       List y,
                       List sf_routing,
                       List watershed_control,
                       CharacterVector date,
                       DataFrame gridMeteo,
                       NumericVector latitude, 
                       bool standSummary = false, 
                       bool carbonBalanceSummary = false, 
                       bool biomassBalanceSummary = false,
                       double patchsize = NA_REAL) {
  
  DataFrame outWB = Rcpp::as<Rcpp::DataFrame>(output["WatershedWaterBalance"]);
  
  IntegerVector waterOrder = sf_routing["waterOrder"];
  List queenNeigh = sf_routing["queenNeigh"];
  List waterQ = sf_routing["waterQ"];
  LogicalVector isChannel = sf_routing["channel"];
  LogicalVector isOutlet = sf_routing["outlet"];
  IntegerVector target_outlet = sf_routing["target_outlet"];
  NumericVector distance_to_outlet = sf_routing["distance_to_outlet"];
  
  List tetis_parameters = watershed_control["tetis_parameters"];
  bool interflow = tetis_parameters["interflow"];
  bool baseflow = tetis_parameters["baseflow"];

    
  // Reset from previous days
  resetWaterBalanceDayOutput(outWB);

  // A. Landscape interflow
  if(interflow) {
    tetisInterFlow(outWB, y, waterOrder, queenNeigh, waterQ,
                   watershed_control,
                   patchsize);
  }
  
  // B. Simulation of soil cells, non-soil cells and overland flows
  copySnowpackToSoil(y);
  tetisModifyKsat(y, watershed_control, false);
  tetisSimulationWithOverlandFlows(local_model, date, internalCommunication,
                                   standSummary, carbonBalanceSummary, biomassBalanceSummary,
                                   output,
                                   y, 
                                   latitude,
                                   gridMeteo,
                                   waterOrder, queenNeigh, waterQ, isChannel,
                                   watershed_control);
  copySnowpackFromSoil(y);
  tetisModifyKsat(y, watershed_control, true);
  
  //C. Baseflow
  if(baseflow) {
     tetisBaseFlow(outWB,
                   y,
                   waterOrder, queenNeigh, waterQ,
                   isChannel, isOutlet,
                   watershed_control,
                   patchsize);
  }
  
  //D. Applies drainage from aquifer to a deeper aquifer
  tetisDeepAquiferLossToAquifer(outWB, 
                                y, watershed_control);
    
}



