// [[Rcpp::depends(medfate,meteoland)]]
#include <numeric>
#include <Rcpp.h>
#include <meteoland.h>
#include <medfate.h>
using namespace Rcpp;
using namespace medfate;
using namespace meteoland;

const int WBCOM_MinTemperature = 0;
const int WBCOM_MaxTemperature = 1;
const int WBCOM_PET = 2;
const int WBCOM_Precipitation = 3;
const int WBCOM_Rain = 4;
const int WBCOM_Snow = 5;
const int WBCOM_Snowmelt = 6;
const int WBCOM_NetRain = 7;
const int WBCOM_Infiltration = 8;
const int WBCOM_Runoff = 9;
const int WBCOM_Runon = 10;
const int WBCOM_InfiltrationExcess = 11;
const int WBCOM_SaturationExcess = 12;
const int WBCOM_DeepDrainage = 13;
const int WBCOM_CapillarityRise = 14;
const int WBCOM_SoilEvaporation = 15;
const int WBCOM_Transpiration = 16;
const int WBCOM_HerbTranspiration = 17;
const int WBCOM_AquiferExfiltration = 18;
const int WBCOM_DeepAquiferLoss = 19;
const int WBCOM_InterflowInput = 20;
const int WBCOM_InterflowOutput = 21;
const int WBCOM_InterflowBalance = 22;
const int WBCOM_BaseflowInput = 23;
const int WBCOM_BaseflowOutput = 24;
const int WBCOM_BaseflowBalance = 25;
const int WBCOM_ChannelExport = 26;
const int WBCOM_WatershedExport = 27;
const int WBCOM_Interception = 28;

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
      if((ni[j]==iCell) && (qi[j]>0.0)) {
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
      x["snowpack"] = snowpack[i];
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
      snowpack[i] = x["snowpack"];
    }
  }
}
// [[Rcpp::export(".copyStateFromResults")]]
void copyStateFromResults(List y, List localResults) {
  CharacterVector lct = y["land_cover_type"];
  List xList = y["state"];
  int nX = xList.size();
  for(int i=0;i<nX;i++){
    if((lct[i]=="wildland") || (lct[i]=="agriculture") ) {
      List resList = Rcpp::as<Rcpp::List>(localResults[i]);
      xList[i] = resList["final_state"];
    }
  }
}

// [[Rcpp::export(".createDayOutput")]]
List createDayOutput(int nX) {

  List localResults(nX);
  

  int ncol = 29;
  List out(ncol);
  CharacterVector colnames(ncol);
  for(int i = 0; i<ncol; i++) out[i] = NumericVector(nX, 0.0);
  colnames[WBCOM_MinTemperature] = "MinTemperature";
  colnames[WBCOM_MaxTemperature] = "MaxTemperature";
  colnames[WBCOM_PET] = "PET";
  colnames[WBCOM_Precipitation] = "Precipitation";
  colnames[WBCOM_Rain] = "Rain";
  colnames[WBCOM_Snow] = "Snow";
  colnames[WBCOM_Snowmelt] = "Snowmelt";
  colnames[WBCOM_Interception] = "Interception";
  colnames[WBCOM_NetRain] = "NetRain";
  colnames[WBCOM_Infiltration] = "Infiltration";
  colnames[WBCOM_Runoff] = "Runoff";
  colnames[WBCOM_Runon] = "Runon";
  colnames[WBCOM_InfiltrationExcess] = "InfiltrationExcess";
  colnames[WBCOM_SaturationExcess] = "SaturationExcess";
  colnames[WBCOM_DeepDrainage] = "DeepDrainage";
  colnames[WBCOM_CapillarityRise] = "CapillarityRise";
  colnames[WBCOM_SoilEvaporation] = "SoilEvaporation";
  colnames[WBCOM_Transpiration] = "Transpiration";
  colnames[WBCOM_HerbTranspiration] = "HerbTranspiration";
  colnames[WBCOM_AquiferExfiltration] = "AquiferExfiltration";
  colnames[WBCOM_DeepAquiferLoss] = "DeepAquiferLoss";
  colnames[WBCOM_InterflowInput] = "InterflowInput";
  colnames[WBCOM_InterflowOutput] = "InterflowOutput";
  colnames[WBCOM_InterflowBalance] = "InterflowBalance";
  colnames[WBCOM_BaseflowInput] = "BaseflowInput";
  colnames[WBCOM_BaseflowOutput] = "BaseflowOutput";
  colnames[WBCOM_BaseflowBalance] = "BaseflowBalance";
  colnames[WBCOM_ChannelExport] = "ChannelExport";
  colnames[WBCOM_WatershedExport] = "WatershedExport";

  out.attr("names") = colnames;

  DataFrame waterBalance(out);
  return(List::create(_["WatershedWaterBalance"] = waterBalance,
                      _["LocalResults"] = localResults));
}

// [[Rcpp::export(".resetWaterBalanceDayOutput")]]
void resetWaterBalanceDayOutput(DataFrame outWB) {
  int nc = outWB.ncol();
  int nr = outWB.nrow();
  for(int i=0;i<nc;i++) {
    NumericVector v = outWB[i];
    for(int j=0;j<nr;j++) v[j] = 0.0;
  }
}

// [[Rcpp::export(".fcpp_landunit_day")]]
List fcpp_landunit_day(List xi, String model, CharacterVector date, List internalCommunication) {
  List res;
  List x = xi["x"];
  List control  = x["control"];
  String transpirationMode = control["transpirationMode"];
  CharacterVector classString = x.attr("class");
  NumericVector meteovec = xi["meteovec"];
  bool result_cell = xi["result_cell"];
  double latitude = xi["latitude"];
  NumericVector lateralFlows = xi["lateralFlows"];
  double waterTableDepth = xi["waterTableDepth"];
  double runon = xi["runon"];
  double elevation = xi["elevation"];
  double slope = xi["slope"];
  double aspect = xi["aspect"];
  if(Rf_inherits(x, "aspwbInput")) {
    res = medfate::aspwb_day_inner(internalCommunication, x, date, meteovec,
                                   latitude, elevation, slope, aspect, 
                                   runon, lateralFlows, waterTableDepth, 
                                   true);
  } else {
    if(model=="spwb") {
      medfate::spwb_day_inner(internalCommunication, x, date, meteovec,
                              latitude, elevation, slope, aspect, 
                              runon, lateralFlows, waterTableDepth, 
                              true);
    } else if(model=="growth") {
      medfate::growth_day_inner(internalCommunication, x, date, meteovec,
                                  latitude, elevation, slope, aspect, 
                                  runon, lateralFlows, waterTableDepth, 
                                  true);
    } 
    if(result_cell) {
      res = medfate::copy_model_output(internalCommunication, x, "spwb");
    } else if (transpirationMode=="Granier"){
      List spwbOut = internalCommunication["basicSPWBOutput"];
      res = List::create(_["WaterBalance"] = clone(as<NumericVector>(spwbOut["WaterBalance"])));
    } else {
      List spwbOut = internalCommunication["advancedSPWBOutput"];
      res = List::create(_["WaterBalance"] = clone(as<NumericVector>(spwbOut["WaterBalance"])));
    }
  }

  List out = List::create(_["final_state"] = x, 
                          _["simulation_results"] = res);
  return(out);
}

