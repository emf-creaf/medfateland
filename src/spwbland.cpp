// [[Rcpp::depends(medfate,meteoland)]]
#include <numeric>
#include <Rcpp.h>
#include <meteoland.h>
#include <medfate.h>
#include "aspwb.h"
using namespace Rcpp;
using namespace medfate;
using namespace meteoland;

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

// [[Rcpp::export(".watershedDay")]]
List watershedDay(String localModel,
                  CharacterVector lct, List xList, List soilList,
                  IntegerVector waterO, List queenNeigh, List waterQ,
                  NumericVector depth_to_bedrock, NumericVector bedrock_conductivity, NumericVector bedrock_porosity,
                  NumericVector aquifer, NumericVector snowpack,
                  List correction_factors,
                  CharacterVector date,
                  DataFrame gridMeteo,
                  NumericVector latitude, NumericVector elevation, NumericVector slope, NumericVector aspect,
                  double patchsize, bool progress = true) {
  int nX = xList.size();
  NumericVector MinTemperature(nX, NA_REAL), MaxTemperature(nX, NA_REAL), PET(nX, NA_REAL);
  NumericVector Precipitation(nX, NA_REAL), Rain(nX, NA_REAL), Snow(nX, NA_REAL),  Snowmelt(nX, NA_REAL);
  NumericVector NetRain(nX,NA_REAL), Runon(nX,0.0), Infiltration(nX,NA_REAL);
  NumericVector SaturationExcess(nX, 0.0);
  NumericVector Runoff(nX,NA_REAL), DeepDrainage(nX,0.0), AquiferDischarge(nX, 0.0);
  NumericVector SoilEvaporation(nX,NA_REAL), Transpiration(nX,NA_REAL);
  double runoffExport = 0.0;

  double Rdrain = correction_factors["Rdrain"];
  double Rinterflow = correction_factors["Rinterflow"];
  double Rbaseflow = correction_factors["Rbaseflow"];

  //A. Subsurface fluxes
  double cellArea = patchsize; //cell size in m2
  double cellWidth = sqrt(patchsize); //cell width in m
  double n = 3.0;

  NumericVector tminVec = gridMeteo["MinTemperature"];
  NumericVector tmaxVec = gridMeteo["MaxTemperature"];
  NumericVector rhminVec = gridMeteo["MinRelativeHumidity"];
  NumericVector rhmaxVec = gridMeteo["MaxRelativeHumidity"];
  NumericVector precVec = gridMeteo["Precipitation"];
  NumericVector radVec = gridMeteo["Radiation"];
  NumericVector wsVec = gridMeteo["WindSpeed"];
  NumericVector C02Vec = gridMeteo["CO2"];
  

  //A1. Calculate soil and aquifer water table elevation (heads)
  if(progress) Rcout<<"+";
  NumericVector WTD(nX,NA_REAL); //Water table depth
  NumericVector SoilWaterTableElevation(nX,NA_REAL); //water table elevation (including cell elevation) in meters
  NumericVector AquiferWaterTableElevation(nX,NA_REAL); //water table elevation (including cell elevation) in meters
  for(int i=0;i<nX;i++){
    if((lct[i]=="wildland") || (lct[i]=="agriculture") ) {
      List x = Rcpp::as<Rcpp::List>(xList[i]);
      List soil = Rcpp::as<Rcpp::List>(soilList[i]);
      List control = x["control"];
      WTD[i] = medfate::soil_waterTableDepth(soil, control["soilFunctions"]);
      SoilWaterTableElevation[i] = elevation[i]-(WTD[i]/1000.0);
    }
    AquiferWaterTableElevation[i] = elevation[i]-(depth_to_bedrock[i]/1000.0) + (aquifer[i]/bedrock_porosity[i])/1000.0;
  }

  //A2a. Calculate INTERFLOW input/output for each cell (in m3/day)
  if(progress) Rcout<<"+";
  NumericVector interflowInput(nX, 0.0);
  NumericVector interflowOutput(nX, 0.0);
  for(int i=0;i<nX;i++){
    if((lct[i]=="wildland") || (lct[i]=="agriculture")) {
      List soil = Rcpp::as<Rcpp::List>(soilList[i]);
      double D = soil["SoilDepth"]; //Soil depth in mm
      NumericVector clay = soil["clay"];
      NumericVector sand = soil["sand"];
      NumericVector om = soil["om"];
      double Ks1 = 0.01*medfate::soil_saturatedConductivitySX(clay[1], sand[1], om[1], false); //cm/day to m/day
      double Kinterflow = Rinterflow*Ks1;
      if(WTD[i]<D) {
        double T = ((Kinterflow*D*0.001)/n)*pow(1.0-(WTD[i]/D),n); //Transmissivity in m2
        List x = Rcpp::as<Rcpp::List>(xList[i]);
        List control = x["control"];
        String model = control["soilFunctions"];
        NumericVector saturatedVolume = medfate::soil_waterSAT(soil, model);
        NumericVector fieldCapacityVolume = medfate::soil_waterFC(soil, model);
        IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[i]);
        //water table slope between target and neighbours
        NumericVector qni(ni.size(), 0.0);
        for(int j=0;j<ni.size();j++) {
          if((lct[ni[j]-1]=="wildland") || (lct[ni[j]-1]=="agriculture")) { //Only flows to other wildland or agriculture cells
            double tanBeta = (SoilWaterTableElevation[i]-SoilWaterTableElevation[ni[j]-1])/cellWidth;
            if(tanBeta>0.0) qni[j] = tanBeta*T*cellWidth; //flow in m3
          }
        }
        double qntotal = sum(qni);
        double macroporeVolume = sum(saturatedVolume)-sum(fieldCapacityVolume);
        double qntotalallowed = std::min(qntotal, (macroporeVolume/1000.0)*cellArea); //avoid excessive outflow
        double corrfactor = qntotalallowed/qntotal;
        for(int j=0;j<ni.size();j++) {
          if(qni[j]>0.0) {
            interflowInput[ni[j]-1] += qni[j]*corrfactor;
            interflowOutput[i] += qni[j]*corrfactor;
          }
        }

      }
    }
  }

  //A2b. Calculate BASEFLOW output for each cell (in m3/day)
  NumericVector baseflowInput(nX, 0.0);
  NumericVector baseflowOutput(nX, 0.0);
  for(int i=0;i<nX;i++){
    double Kbaseflow = Rbaseflow*bedrock_conductivity[i]; //m/day
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
          baseflowInput[ni[j]-1] += qni[j]*corrfactor;
          baseflowOutput[i] += qni[j]*corrfactor;
        }
      }
    }
  }

  //A3a. Apply changes in soil moisture to each cell
  if(progress) Rcout<<"+";
  for(int i=0;i<nX;i++){
    if((lct[i]=="wildland") || (lct[i]=="agriculture")) {
      double deltaS = 1000.0*((interflowInput[i]-interflowOutput[i])/cellArea); //change in moisture in mm (L/m2)
      if(deltaS != 0.0) {
        // Rcout<<inflow[i]<< " "<<outflow[i]<< " "<<cellArea<<" "<<deltaS<<"_";
        List x = Rcpp::as<Rcpp::List>(xList[i]);
        List soil = Rcpp::as<Rcpp::List>(soilList[i]);
        NumericVector W = soil["W"]; //Access to soil state variable
        NumericVector dVec = soil["dVec"];
        NumericVector macro = soil["macro"];
        NumericVector rfc = soil["rfc"];
        List control = x["control"];
        String soilFunctions = control["soilFunctions"];
        NumericVector Water_FC = medfate::soil_waterFC(soil, soilFunctions);
        NumericVector Water_SAT = medfate::soil_waterSAT(soil, soilFunctions);
        int nlayers = dVec.length();
        // Rcout<<W[0]<<" A ";
        for(int l=(nlayers-1);l>=0;l--) {
          if(dVec[l]>0) {
            double Wn = W[l]*Water_FC[l] + deltaS; //Update water volume
            deltaS = std::max(Wn - Water_SAT[l],0.0); //Update deltaS, using the excess of water over saturation
            W[l] = std::max(0.0,std::min(Wn, Water_SAT[l])/Water_FC[l]); //Update theta (this modifies 'soil') here no upper
          }
        }
        // Rcout<<W[0]<<"\n";
        // stop("kk");
        if(deltaS>0) { //If soil is completely saturated increase subsurface return flow to be processed with vertical flows
          SaturationExcess[i] += deltaS;
        }
        // Rcout<<WTD[i]<<"/"<<waterTableDepth(soil,soilFunctions)<<"\n";
      }
    }
  }
  //A3b. Apply changes in aquifer to each cell
  for(int i=0;i<nX;i++){
    double deltaA = 1000.0*((baseflowInput[i]-baseflowOutput[i])/cellArea); //change in moisture in mm (L/m2)
    aquifer[i] = aquifer[i] + deltaA; //New water amount in the aquifer (mm water)
    double DTAn = depth_to_bedrock[i] - (aquifer[i]/bedrock_porosity[i]); // New depth to aquifer (mm)
    if((lct[i]=="wildland") || (lct[i]=="agriculture")) {
      List soil = Rcpp::as<Rcpp::List>(soilList[i]);
      double D = soil["SoilDepth"];
      if(DTAn<D){
        List x = Rcpp::as<Rcpp::List>(xList[i]);
        NumericVector W = soil["W"]; //Access to soil state variable
        NumericVector dVec = soil["dVec"];
        NumericVector macro = soil["macro"];
        NumericVector rfc = soil["rfc"];
        List control = x["control"];
        String soilFunctions = control["soilFunctions"];
        NumericVector Water_FC = medfate::soil_waterFC(soil, soilFunctions);
        NumericVector Water_SAT = medfate::soil_waterSAT(soil, soilFunctions);
        int nlayers = dVec.length();

        double deltaS = (D-DTAn)*bedrock_porosity[i]; //mm = l/m2 of water
        AquiferDischarge[i] = deltaS;
        aquifer[i] = aquifer[i] - deltaS; //Update aquifer to its maximum limit (soil depth)
        for(int l=(nlayers-1);l>=0;l--) { //Fill layers from bottom to top
          if(dVec[l]>0) {
            double Wn = W[l]*Water_FC[l] + deltaS; //Update water volume
            deltaS = std::max(Wn - Water_SAT[l],0.0); //Update deltaS, using the excess of water over saturation
            W[l] = std::max(0.0,std::min(Wn, Water_SAT[l])/Water_FC[l]); //Update theta (this modifies 'soil') here no upper
          }
        }
        if(deltaS>0) { //If soil is completely saturated increase subsurface return flow to be processed with vertical flows
          SaturationExcess[i] += deltaS;
        }
      }

    } else if(DTAn<0) { //Turn negative aquifer depth into surface flow
      AquiferDischarge[i] += -DTAn*bedrock_porosity[i];
      aquifer[i] = depth_to_bedrock[i]*bedrock_porosity[i];
      SaturationExcess[i] = AquiferDischarge[i];
    }
  }

  // Rcout<<"\n";

  //B. Vertical and surface fluxes
  List localResults(nX);
  if(progress) Rcout<<"+";
  for(int i=0;i<nX;i++) {
    //get next cell in order
    int iCell = waterO[i]-1; //Decrease index!!!!
    if((lct[iCell]=="wildland") || (lct[iCell]=="agriculture")) {
      List x = Rcpp::as<Rcpp::List>(xList[iCell]);
      List soil = soilList[iCell];
      double Kdrain = soil["Kdrain"];
      double D = soil["SoilDepth"]; //Soil depth in mm
      double DTA = depth_to_bedrock[i] - (aquifer[iCell]/bedrock_porosity[iCell]);
      if(DTA < D) {
        soil["Kdrain"] = 0.0; //If aquifer depth over soil depth do not allow percolation to aquifer
      } else {
        soil["Kdrain"] = 1000.0*bedrock_conductivity[iCell]*Rdrain; //Saturated vertical hydraulic conductivity in mm/day
        // Rcout<<Kdrain<< " "<<1000.0*bedrock_conductivity[i]*Rdrain<<"\n";
      }
      //copy snowpack
      soil["SWE"] = snowpack[iCell];

      //Run daily soil water balance for the current cell
      List res;
      NumericVector meteovec = NumericVector::create(
        _["MinTemperature"] = tminVec[iCell],
        _["MaxTemperature"] = tmaxVec[iCell],
        _["MinRelativeHumidity"] = rhminVec[iCell],
        _["MaxRelativeHumidity"] = rhmaxVec[iCell],
        _["Precipitation"]  =precVec[iCell],
        _["Radiation"] = radVec[iCell],
        _["WindSpeed"] = wsVec[iCell],
        _["CO2"] = C02Vec[iCell]
      );
      if(lct[iCell]=="agriculture") {
        res = aspwb_day(x, date, meteovec,
                        latitude[iCell], elevation[iCell], slope[iCell], aspect[iCell],
                        Runon[iCell]+SaturationExcess[iCell], true);
      } else {
        if(localModel=="spwb") {
          res = medfate::spwb_day(x, date, meteovec,
                                  latitude[iCell], elevation[iCell], slope[iCell], aspect[iCell],
                                  Runon[iCell]+SaturationExcess[iCell], true);
        } else if(localModel =="growth") {
          res = medfate::growth_day(x, date, meteovec,
                                    latitude[iCell], elevation[iCell], slope[iCell], aspect[iCell],
                                    Runon[iCell]+SaturationExcess[iCell], true);
        }
      }
      localResults[iCell] = res; //Store for output
      soil["Kdrain"] = Kdrain; //Restore value
      snowpack[iCell] = soil["SWE"]; //Copy back snowpack
      NumericVector DB = res["WaterBalance"];
      DataFrame SB = Rcpp::as<Rcpp::DataFrame>(res["Soil"]);
      MinTemperature[iCell] = tminVec[iCell];
      MaxTemperature[iCell] = tmaxVec[iCell];
      Snow[iCell] = DB["Snow"];
      Snowmelt[iCell] = DB["Snowmelt"];
      PET[iCell] = DB["PET"];
      Rain[iCell] = DB["Rain"];
      Infiltration[iCell] = DB["Infiltration"];
      Runoff[iCell] = DB["Runoff"];
      DeepDrainage[iCell] = DB["DeepDrainage"];
      SoilEvaporation[iCell] = sum(Rcpp::as<Rcpp::NumericVector>(SB["SoilEvaporation"]));
      
      if(lct[iCell]=="wildland") {
        NetRain[iCell] = DB["NetRain"];
        DataFrame PL = Rcpp::as<Rcpp::DataFrame>(res["Plants"]);
        NumericVector EplantCoh = Rcpp::as<Rcpp::NumericVector>(PL["Transpiration"]);
        Transpiration[iCell] = sum(EplantCoh);
      }

      //Add deep drainage to aquifer of the cell
      aquifer[iCell] += DeepDrainage[iCell];
    } else {
      Rain[iCell] = 0.0;
      Snow[iCell] = 0.0;
      Snowmelt[iCell] = 0.0;
      SoilEvaporation[iCell] = 0.0;
      double tday = meteoland::utils_averageDaylightTemperature(tminVec[iCell], tmaxVec[iCell]);
      if(tday<0.0) {
        Snow[iCell] = precVec[iCell];
        snowpack[iCell] += Snow[iCell];
      } else {
        Rain[iCell] = precVec[iCell];
      }
      NetRain[iCell] = Rain[iCell];
      if(snowpack[iCell]>0.0) {
        double melt = medfate::hydrology_snowMelt(tday, radVec[iCell], 1.0, elevation[iCell]);
        Snowmelt[iCell] = std::min(melt, snowpack[iCell]);
        snowpack[iCell] -= Snowmelt[iCell];
      }
      if(lct[iCell]=="rock" || lct[iCell]=="artificial") {
        Infiltration[iCell] = 0.0;
        //all Precipitation becomes surface runoff if cell is rock outcrop/artificial
        Runoff[iCell] =  SaturationExcess[iCell]+Runon[iCell]+Snowmelt[iCell]+Rain[iCell];
        DeepDrainage[iCell] = 0.0;
      } else if(lct[iCell]=="water") {
        // water cells receive water from other cells or Precipitation
        // but do not export to the atmosphere contribute nor to other cells.
        // any received water drains directly to the aquifer so that it can feed base flow
        Infiltration[iCell] = SaturationExcess[iCell]+Runon[iCell] + Snowmelt[iCell]+ Rain[iCell];
        DeepDrainage[iCell] = Infiltration[iCell];
        aquifer[iCell] += DeepDrainage[iCell];
        double DTAn = depth_to_bedrock[iCell] - (aquifer[iCell]/bedrock_porosity[iCell]); // New depth to aquifer (mm)
        if(DTAn<0.0) { //Turn excess into Runoff
          Runoff[iCell] = aquifer[iCell] - (depth_to_bedrock[iCell]*bedrock_porosity[iCell]);
          DeepDrainage[iCell] = DeepDrainage[iCell] - Runoff[iCell];
          aquifer[iCell] = depth_to_bedrock[iCell]*bedrock_porosity[iCell];
        }
      }
    }
    //Assign runoff to runon of downhill neighbours
    double ri =  Runoff[iCell];
    if(ri>0.0) {
      IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[iCell]);
      NumericVector qi = Rcpp::as<Rcpp::NumericVector>(waterQ[iCell]);
      if(ni.size()>0) {
        for(int j=0;j<ni.size();j++)  {
          Runon[ni[j]-1] += (qi[j]*Runoff[iCell]); //decrease index
          ri -= (qi[j]*Runoff[iCell]);
        }
      }
      if(ri>0.0) {
        if((sum(qi)>0.0) & (ri > 0.00001)) {
          Rcout<<ni.size()<< " "<<qi.size()<<" "<<iCell<< " "<< sum(qi)<< " "<< ri<<"\n";
          stop("Non-outlet cell with runoff export");
        }
        runoffExport += ri; //Add remaining to landscape export
      }
    }

  }
  // Rcout<<"C";

  DataFrame waterBalance = DataFrame::create(_["MinTemperature"] = MinTemperature, _["MaxTemperature"] = MaxTemperature, _["PET"] = PET,
                                             _["Rain"] = Rain, _["Snow"] = Snow,_["Snowmelt"] = Snowmelt,
                                             _["NetRain"] = NetRain, _["Runon"] = Runon, _["Infiltration"] = Infiltration,
                                             _["Runoff"] = Runoff, _["SaturationExcess"] = SaturationExcess,
                                             _["DeepDrainage"] = DeepDrainage, _["AquiferDischarge"] = AquiferDischarge,
                                             _["InterflowInput"] = interflowInput, _["InterflowOutput"] = interflowOutput,
                                             _["BaseflowInput"] = baseflowInput, _["BaseflowOutput"] = baseflowOutput,
                                             _["SoilEvaporation"] = SoilEvaporation, _["Transpiration"] = Transpiration);
  return(List::create(_["WatershedWaterBalance"] = waterBalance,
                      _["LocalResults"] = localResults));
}


