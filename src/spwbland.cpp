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

// [[Rcpp::export(".spwbgridDay")]]
List spwbgridDay(CharacterVector lct, List xList, List soilList,
                 IntegerVector waterO, List queenNeigh, List waterQ,
                 CharacterVector date,
                 NumericVector tminVec, NumericVector tmaxVec, NumericVector rhminVec, NumericVector rhmaxVec,
                 NumericVector precVec, NumericVector radVec, NumericVector wsVec,
                 NumericVector latitude, NumericVector elevation, NumericVector slope, NumericVector aspect,
                 double patchsize) {
  int nX = xList.size();
  NumericVector Rain(nX, NA_REAL), Snow(nX, NA_REAL), NetRain(nX,NA_REAL), Runon(nX,0.0), Infiltration(nX,NA_REAL);
  NumericVector Runoff(nX,NA_REAL), DeepDrainage(nX,NA_REAL);
  NumericVector SoilEvaporation(nX,NA_REAL), Transpiration(nX,NA_REAL);
  double runoffExport = 0.0;

  //A. Subsurface fluxes
  double cellArea = patchsize; //cell size in m2
  double cellWidth = sqrt(patchsize); //cell width in m
  double n = 3.0;
  double K = 7.2; //7.2 m/day
  //1. Calculate water table depth
  NumericVector WTD(nX,NA_REAL); //Water table depth
  NumericVector WaterTableElevation(nX,NA_REAL); //water table elevation (including cell elevation) in meters
  for(int i=0;i<nX;i++){
    if((lct[i]=="wildland") || (lct[i]=="agriculture") ) {
      List x = Rcpp::as<Rcpp::List>(xList[i]);
      List soil = Rcpp::as<Rcpp::List>(soilList[i]);
      List control = x["control"];
      WTD[i] = medfate::soil_waterTableDepth(soil, control["soilFunctions"]);
      WaterTableElevation[i] = elevation[i]-(WTD[i]/1000.0);
    }
  }
  //2. Calculate inflow/outflow for each cell (in m3/day)
  NumericVector inflow(nX, 0.0);
  NumericVector outflow(nX, 0.0);
  for(int i=0;i<nX;i++){
    if((lct[i]=="wildland") || (lct[i]=="agriculture") ) {
      List soil = Rcpp::as<Rcpp::List>(soilList[i]);
      double D = soil["SoilDepth"]; //Soil depth in mm
      if(WTD[i]<D) {
        double T = ((K*D*0.001)/n)*pow(1.0-(WTD[i]/D),n); //Transmissivity in m2
        IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[i]);
        //water table slope between target and neighbours
        for(int j=0;j<ni.size();j++) {
          double tanBeta = (WaterTableElevation[i]-WaterTableElevation[ni[j]-1])/cellWidth;
          if(tanBeta>0.0) {
            if((lct[ni[j]-1]=="wildland") || (lct[ni[j]-1]=="agriculture")) { //Only flows to other wildland or agriculture cells
              double qn = tanBeta*T*cellWidth; //flow in m3
              inflow[ni[j]-1] += qn;
              outflow[i] += qn;
            }
          }
        }
      }
    }
  }
  //3. Apply changes in soil moisture to each cell
  for(int i=0;i<nX;i++){
    if((lct[i]=="wildland") || (lct[i]=="agriculture") ) {
      double deltaS = 1000.0*((inflow[i]-outflow[i])/cellArea); //change in moisture in mm (L/m2)
      if(deltaS != 0.0) {
        // Rcout<<deltaS<<"_";
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
        for(int l=(nlayers-1);l>=0;l--) {
          if(dVec[l]>0) {
            double Wn = W[l]*Water_FC[l] + deltaS; //Update water volume
            deltaS = std::max(Wn - Water_SAT[l],0.0); //Update deltaS, using the excess of water over saturation
            W[l] = std::max(0.0,std::min(Wn, Water_SAT[l])/Water_FC[l]); //Update theta (this modifies 'soil') here no upper
          }
        }
        if(deltaS>0) { //If soil is completely saturated increase Runon (return flow) to be processed with vertical flows
          Runon[i] += deltaS;
        }
        // Rcout<<WTD[i]<<"/"<<waterTableDepth(soil,soilFunctions)<<"\n";
      }
    }
  }
  // Rcout<<"\n";

  //B. Vertical and surface fluxes
  for(int i=0;i<nX;i++) {
    //get next cell in order
    int iCell = waterO[i]-1; //Decrease index!!!!
    if((lct[iCell]=="wildland") || (lct[iCell]=="agriculture") ) {
      List x = Rcpp::as<Rcpp::List>(xList[iCell]);
      List soil = Rcpp::as<Rcpp::List>(soilList[iCell]);
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
      Runoff[iCell] =  Runon[iCell]+precVec[iCell];
      double ri = Runoff[iCell];
      if(ri>0.0) {
        IntegerVector ni = Rcpp::as<Rcpp::IntegerVector>(queenNeigh[iCell]);
        NumericVector qi = Rcpp::as<Rcpp::NumericVector>(waterQ[iCell]);
        if(ni.size()>0) {
          for(int j=0;j<ni.size();j++) Runon[ni[j]-1] += qi[j]*ri;//decrease index
        } else {
          runoffExport += ri; //If no suitable neighbours add ri to landscape export via runoff
        }
      }
    } else if(lct[iCell]=="static") {
      // static cells receive water from other cells or Precipitation
      // but do not export to the atmosphere contribute nor to other cells.
      // Hence, water balance over the landscape is achieved by
      // adding this water to the landscape export via landscape runoff.
      runoffExport += Runon[iCell] + precVec[iCell];
    }
  }


  DataFrame waterBalance = DataFrame::create(_["Rain"] = Rain, _["Snow"] = Snow, _["NetRain"] = NetRain, _["Runon"] = Runon, _["Infiltration"] = Infiltration,
                                             _["Runoff"] = Runoff, _["DeepDrainage"] = DeepDrainage,
                                             _["SoilEvaporation"] = SoilEvaporation, _["Transpiration"] = Transpiration);
  return(List::create(_["WaterBalance"] = waterBalance,
                      _["RunoffExport"] = runoffExport));
}


