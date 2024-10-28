// [[Rcpp::depends(medfate,meteoland)]]
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
DataFrame tetisInterFlow(List y,
                         IntegerVector waterO, List queenNeigh, List waterQ,
                         List watershed_control,
                         double patchsize) {
  
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
  NumericVector interflowInput(nX, NA_REAL);
  NumericVector interflowOutput(nX, NA_REAL);
  NumericVector interflowBalance(nX, NA_REAL);
  for(int i=0;i<nX;i++){
    if(is_soil[i]) {
      interflowInput[i] = 0.0;
      interflowOutput[i] = 0.0;
    }
  }
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
  DataFrame out = DataFrame::create(_["InterflowInput"] = interflowInput, 
                                    _["InterflowOutput"] = interflowOutput,
                                    _["InterflowBalance"] = interflowBalance);
  return(out);
}

// [[Rcpp::export(".tetisBaseFlow")]]
DataFrame tetisBaseFlow(List y,
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
  NumericVector baseflowInput(nX, 0.0);
  NumericVector baseflowOutput(nX, 0.0);
  NumericVector baseflowBalance(nX, 0.0);
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
  DataFrame out = DataFrame::create(_["BaseflowInput"] = baseflowInput, 
                                    _["BaseflowOutput"] = baseflowOutput,
                                    _["BaseflowBalance"] = baseflowBalance);
  return(out);
}

// [[Rcpp::export(".tetisApplyBaseflowChangesToAquifer")]]
NumericVector tetisApplyBaseflowChangesToAquifer(List y,
                                                 NumericVector baseflowBalance,
                                                 double patchsize) {
  NumericVector depth_to_bedrock  = y["depth_to_bedrock"];
  NumericVector bedrock_porosity = y["bedrock_porosity"];
  NumericVector aquifer = y["aquifer"];
  
  int nX = aquifer.size();
  NumericVector AquiferExfiltration(nX, 0.0);
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
  return(AquiferExfiltration);
}

// [[Rcpp::export(".tetisApplyLocalFlowsToAquifer")]]
void tetisApplyLocalFlowsToAquifer(List y,
                                   NumericVector CapillarityRise,
                                   NumericVector DeepDrainage) {
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
NumericVector tetisApplyDeepAquiferLossToAquifer(List y,
                                                 List watershed_control) {
  List tetis_parameters = watershed_control["tetis_parameters"];
  double deep_aquifer_loss = tetis_parameters["deep_aquifer_loss"];
  
  NumericVector aquifer = y["aquifer"];
  int nX = aquifer.size();
  NumericVector loss_rate(nX, deep_aquifer_loss); 
  if(y.containsElementNamed("deep_aquifer_loss")) {
    loss_rate = Rcpp::as<Rcpp::NumericVector>(y["deep_aquifer_loss"]);
  }
  NumericVector DeepAquiferLoss(nX, 0.0);
  for(int i=0;i<nX;i++){
    DeepAquiferLoss[i] = std::max(std::min(aquifer[i], loss_rate[i]), 0.0);
    aquifer[i] -= DeepAquiferLoss[i];
  }
  return(DeepAquiferLoss);
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
                                      NumericVector tminVec, NumericVector tmaxVec, NumericVector precVec, NumericVector radVec,
                                      NumericVector waterO, List queenNeigh, List waterQ,
                                      List watershed_control) {
  
  List tetis_parameters = watershed_control["tetis_parameters"];
  double rock_max_infiltration = tetis_parameters["rock_max_infiltration"];
  
  CharacterVector lct = y["land_cover_type"];
  List xList = y["state"];
  int nX = xList.size();
  NumericVector elevation = y["elevation"];
  NumericVector snowpack = y["snowpack"];
  NumericVector Rain(nX, NA_REAL);
  NumericVector Runoff(nX, NA_REAL), Infiltration(nX, NA_REAL), InfiltrationExcess(nX, NA_REAL), DeepDrainage(nX, NA_REAL), Snow(nX, NA_REAL),  Snowmelt(nX, NA_REAL);
  
  for(int i=0;i<nX;i++){
    if(lct[i]=="rock" || lct[i]=="artificial" || lct[i]=="water") {
      Rain[i] = 0.0;
      Snow[i] = 0.0;
      Snowmelt[i] = 0.0;
      Runoff[i]  = 0.0;
      Infiltration[i] = 0.0;
      InfiltrationExcess[i] = 0.0;
      DeepDrainage[i] = 0.0;
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
        Infiltration[i] = std::min(rock_max_infiltration, Snowmelt[i]+Rain[i]);
        DeepDrainage[i] = Infiltration[i];
        InfiltrationExcess[i] = Snowmelt[i]+Rain[i] - DeepDrainage[i];
        Runoff[i] = InfiltrationExcess[i];
      } else if(lct[i]=="artificial") {
        //all Precipitation becomes surface runoff if cell is rock outcrop/artificial
        InfiltrationExcess[i] =  Snowmelt[i]+Rain[i];
        Runoff[i] = InfiltrationExcess[i];
      } else if(lct[i]=="water") {
        // water cells receive water from Precipitation
        // but do not export to the atmosphere contribute nor to other cells.
        // any received water drains directly to the aquifer so that it can feed base flow
        DeepDrainage[i] = Snowmelt[i]+ Rain[i];
        Infiltration[i] = DeepDrainage[i];
      }
      
      // Rcout << i<< " Rain " << Rain[i] << " Snow " << Snow[i] << " DeepDrainage " << DeepDrainage[i] << " Snowmelt " << Snowmelt[i] << " Runoff " << Runoff[i] << "\n";
    }
  }
  //Estimate Runon to wildland/agriculture cells
  NumericVector Runon(nX,0.0);
  NumericVector WatershedExport(nX,0.0);
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
        if(sum(qi)==0.0) { // outlet rock cell
          WatershedExport[iCell] = ri;
        }
      }      
    }
  }
  DataFrame out = DataFrame::create(_["Rain"] = Rain, _["Snow"] = Snow,
                                    _["Snowmelt"] = Snowmelt, 
                                    _["Infiltration"] = Infiltration,
                                    _["InfiltrationExcess"] = InfiltrationExcess,
                                    _["Runoff"] = Runoff,
                                    _["Runon"] = Runon,
                                    _["DeepDrainage"] = DeepDrainage,
                                    _["WatershedExport"] = WatershedExport);
  return(out);
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
