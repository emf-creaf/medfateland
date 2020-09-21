#include <numeric>
#include <Rcpp.h>
#include <meteoland.h>
#include <medfate.h>
using namespace Rcpp;


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

// [[Rcpp::export(".wswbDay")]]
List wswbDay(CharacterVector lct, List xList, List soilList,
             IntegerVector waterO, List queenNeigh, List waterQ, 
             DataFrame bedrock, NumericVector aquifer,
             List correctionFactors,
             CharacterVector date,
             DataFrame gridMeteo,
             NumericVector latitude, NumericVector elevation, NumericVector slope, NumericVector aspect,
             double patchsize) {
  int nX = xList.size();
  NumericVector Rain(nX, NA_REAL), Snow(nX, NA_REAL), NetRain(nX,NA_REAL), Runon(nX,0.0), Infiltration(nX,NA_REAL);
  NumericVector Runoff(nX,NA_REAL), DeepDrainage(nX,NA_REAL);
  NumericVector SoilEvaporation(nX,NA_REAL), Transpiration(nX,NA_REAL);
  double runoffExport = 0.0;

  double Rdrain = correctionFactors["Rdrain"];
  double Rinterflow = correctionFactors["Rinterflow"];
  double Rbaseflow = correctionFactors["Rbaseflow"];

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
  
  
  NumericVector DTB = bedrock["DepthToBedrock"]; //mm
  NumericVector RockConductivity = bedrock["Conductivity"]; //m/day
  NumericVector RockPorosity = bedrock["Porosity"]; //[0-1]
  
  //A1. Calculate soil and aquifer water table elevation (heads)
  Rcout<<"+";
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
      AquiferWaterTableElevation[i] = elevation[i]-(DTB[i]/1000.0) + (aquifer[i]/RockPorosity[i])/1000.0;
    }
  }
  
  //A2a. Calculate INTERFLOW input/output for each cell (in m3/day)
  Rcout<<"+";
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
        IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[i]);
        //water table slope between target and neighbours
        for(int j=0;j<ni.size();j++) {
          double tanBeta = (SoilWaterTableElevation[i]-SoilWaterTableElevation[ni[j]-1])/cellWidth;
          if(tanBeta>0.0) {
            if((lct[ni[j]-1]=="wildland") || (lct[ni[j]-1]=="agriculture")) { //Only flows to other wildland or agriculture cells
              double qn = tanBeta*T*cellWidth; //flow in m3
              interflowInput[ni[j]-1] += qn;
              interflowOutput[i] += qn;
            }
          }
        }
      }
    }
  }
  
  //A2b. Calculate BASEFLOW input/output for each cell (in m3/day)
  NumericVector baseflowInput(nX, 0.0);
  NumericVector baseflowOutput(nX, 0.0);
  for(int i=0;i<nX;i++){
    double Kbaseflow = Rbaseflow*RockConductivity[i];
    if(aquifer[i]>0) {
      double T = ((Kbaseflow*DTB[i]*0.001)/n)*pow(1.0-((DTB[i] - (aquifer[i]/RockPorosity[i]))/DTB[i]),n); //Transmissivity in m2
      IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[i]);
      //water table slope between target and neighbours
      for(int j=0;j<ni.size();j++) {
        double tanBeta = (AquiferWaterTableElevation[i]-AquiferWaterTableElevation[ni[j]-1])/cellWidth;
        if(tanBeta>0.0) {
          double qn = tanBeta*T*cellWidth; //flow in m3/day
          qn = std::min(qn, aquifer[i]); //avoid excessive flow
          baseflowInput[ni[j]-1] += qn;
          baseflowOutput[i] += qn;
        }
      }
    }
  }
  
  //A3a. Apply changes in soil moisture to each cell
  Rcout<<"+";
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
        if(deltaS>0) { //If soil is completely saturated increase Runon (return flow) to be processed with vertical flows
          Runon[i] += deltaS;
        }
        // Rcout<<WTD[i]<<"/"<<waterTableDepth(soil,soilFunctions)<<"\n";
      }
    }
  }
  //A3b. Apply changes in aquifer to each cell
  for(int i=0;i<nX;i++){
    double deltaA = 1000.0*((baseflowInput[i]-baseflowOutput[i])/cellArea); //change in moisture in mm (L/m2)
    if(deltaA != 0.0) {
      double Wn = aquifer[i] + deltaA; //New water amount in the aquifer (mm water)
      double DTAn = DTB[i] - (Wn/RockPorosity[i]); // New depth to aquifer (mm)
      if((lct[i]=="wildland") || (lct[i]=="agriculture")) {
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
        double D = sum(dVec);
        if(DTAn>D){ 
          double deltaS = (DTAn-D)*RockPorosity[i]; //mm = l/m2 of water
          Wn = Wn - deltaS; //Update Wn to limit depth to aquifer
          for(int l=(nlayers-1);l>=0;l--) { //Fill layers from bottom to top
            if(dVec[l]>0) {
              double Wn = W[l]*Water_FC[l] + deltaS; //Update water volume
              deltaS = std::max(Wn - Water_SAT[l],0.0); //Update deltaS, using the excess of water over saturation
              W[l] = std::max(0.0,std::min(Wn, Water_SAT[l])/Water_FC[l]); //Update theta (this modifies 'soil') here no upper
            }
          }
          if(deltaS>0) { //If soil is completely saturated increase Runon (return flow) to be processed with vertical flows
            Runon[i] += deltaS;
          }
        }  
      }
      //Store new aquifer water volume (mm)
      aquifer[i] = Wn;
    }
  }
  
  // Rcout<<"\n";

  //B. Vertical and surface fluxes
  Rcout<<"+";
  for(int i=0;i<nX;i++) {
    //get next cell in order
    int iCell = waterO[i]-1; //Decrease index!!!!
    if((lct[iCell]=="wildland") || (lct[iCell]=="agriculture")) {
      List x = Rcpp::as<Rcpp::List>(xList[iCell]);
      List soil = soilList[iCell]; 
      // double Kperc = soil["Kperc"];
      // soil["Kperc"] = Kperc*Rdrain;

      //Run daily soil water balance for the current cell
      List res;
      res = medfate::spwb_day(x, soil, date,
                        tminVec[iCell], tmaxVec[iCell], rhminVec[iCell], rhmaxVec[iCell],
                        radVec[iCell], wsVec[iCell],
                        latitude[iCell], elevation[iCell], slope[iCell], aspect[iCell],
                        precVec[iCell], Runon[iCell]);
      NumericVector DB = res["WaterBalance"];
      DataFrame SB = res["Soil"];
      DataFrame PL = res["Plants"];
      Snow[iCell] = DB["Snow"];
      Rain[iCell] = DB["Rain"];
      NetRain[iCell] = DB["NetRain"];
      Runon[iCell] = DB["Runon"];
      Infiltration[iCell] = DB["Infiltration"];
      Runoff[iCell] = DB["Runoff"];
      DeepDrainage[iCell] = DB["DeepDrainage"];
      SoilEvaporation[iCell] = sum(Rcpp::as<Rcpp::NumericVector>(SB["SoilEvaporation"]));
      NumericVector EplantCoh = Rcpp::as<Rcpp::NumericVector>(PL["Transpiration"]);
      Transpiration[iCell] = sum(EplantCoh);

      //Add deep drainage to aquifer of the cell
      aquifer[iCell] += DeepDrainage[iCell];
      
      //Assign runoff to runon of neighbours
      double ri =  Runoff[iCell];
      if(ri>0.0) {
        IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[iCell]);
        NumericVector qi = Rcpp::as<Rcpp::NumericVector>(waterQ[iCell]);
        if(ni.size()>0) {
          for(int j=0;j<ni.size();j++)  {
            Runon[ni[j]-1] += qi[j]*ri; //decrease index
            ri -= qi[j]*ri;
          }
        }
      }
      runoffExport += ri; //Add remaining
    } else if(lct[iCell]=="rock") {//all Precipitation becomes surface runoff if cell is rock outcrop
      double tday = meteoland::utils_averageDaylightTemperature(tminVec[iCell], tmaxVec[iCell]);
      Rain[iCell] = 0.0;
      NetRain[iCell] = 0.0;
      Snow[iCell] = 0.0;
      if(tday<0.0) {
        Snow[iCell] = precVec[iCell];
      } else {
        Rain[iCell] = precVec[iCell];
        NetRain[iCell] = precVec[iCell];
      }
      Runoff[iCell] =  Runon[iCell]+precVec[iCell];
      double ri = Runoff[iCell];
      if(ri>0.0) {
        IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[iCell]);
        NumericVector qi = Rcpp::as<Rcpp::NumericVector>(waterQ[iCell]);
        for(int j=0;j<ni.size();j++)  {
          Runon[ni[j]-1] += qi[j]*ri; //decrease index
          ri -= qi[j]*ri;
        }
        runoffExport += ri; //Add remaining
      }
    } else if(lct[iCell]=="static") {
      double tday = meteoland::utils_averageDaylightTemperature(tminVec[iCell], tmaxVec[iCell]);
      Rain[iCell] = 0.0;
      NetRain[iCell] = 0.0;
      Snow[iCell] = 0.0;
      if(tday<0.0) {
        Snow[iCell] = precVec[iCell];
      } else {
        Rain[iCell] = precVec[iCell];
        NetRain[iCell] = precVec[iCell];
      }
      // static cells receive water from other cells or Precipitation
      // but do not export to the atmosphere contribute nor to other cells.
      // Hence, water balance over the landscape is achieved by
      // adding this water to the landscape export via landscape runoff.
      runoffExport += Runon[iCell] + precVec[iCell];
    }
  }
  // Rcout<<"C";

  DataFrame waterBalance = DataFrame::create(_["Rain"] = Rain, _["Snow"] = Snow, _["NetRain"] = NetRain, _["Runon"] = Runon, _["Infiltration"] = Infiltration,
                                             _["Runoff"] = Runoff, _["DeepDrainage"] = DeepDrainage,
                                             _["SoilEvaporation"] = SoilEvaporation, _["Transpiration"] = Transpiration);
  return(List::create(_["WaterBalance"] = waterBalance,
                      _["RunoffExport"] = runoffExport));
}


