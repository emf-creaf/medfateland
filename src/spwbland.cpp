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
const int WBCOM_NegativeAquiferCorrection = 29;

const int STCOM_LAI = 0;
const int STCOM_LAIherb = 1;
const int STCOM_LAIlive = 2;
const int STCOM_LAIexpanded = 3;
const int STCOM_LAIdead = 4;
const int STCOM_Cm = 5;
const int STCOM_LgroundPAR = 6;
const int STCOM_LgroundSWR = 7;

const int CBCOM_GrossPrimaryProduction = 0;
const int CBCOM_MaintenanceRespiration = 1;
const int CBCOM_SynthesisRespiration = 2;
const int CBCOM_NetPrimaryProduction = 3;

const int BBCOM_StructuralBalance = 0;
const int BBCOM_LabileBalance = 1;
const int BBCOM_PlantBalance = 2;
const int BBCOM_MortalityLoss = 3;
const int BBCOM_CohortBalance = 4;

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
List createDayOutput(int nX, 
                     bool standSummary, bool carbonBalanceSummary, bool biomassBalanceSummary) {

  int ncol = 30;
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
  colnames[WBCOM_NegativeAquiferCorrection] = "NegativeAquiferCorrection";
  
  out.attr("names") = colnames;

  DataFrame waterBalance(out);
  
  List localResults(nX);
  List l = List::create(_["LocalResults"] = localResults,
                        _["WatershedWaterBalance"] = waterBalance);
  if(standSummary) {
    int ncol_stand = 8;
    List out_stand(ncol_stand);
    CharacterVector colnames_stand(ncol_stand);
    for(int i = 0; i<ncol_stand; i++) out_stand[i] = NumericVector(nX, 0.0);
    colnames_stand[STCOM_LAI] = "LAI";
    colnames_stand[STCOM_LAIherb] = "LAIherb";
    colnames_stand[STCOM_LAIlive] = "LAIlive";
    colnames_stand[STCOM_LAIexpanded] = "LAIexpanded";
    colnames_stand[STCOM_LAIdead] = "LAIdead";
    colnames_stand[STCOM_Cm] = "Cm";
    colnames_stand[STCOM_LgroundPAR] = "LgroundPAR";
    colnames_stand[STCOM_LgroundSWR] = "LgroundSWR";
    out_stand.attr("names") = colnames_stand;
    DataFrame stand(out_stand);
    l.push_back(stand, "WatershedStand");
  }
  if(carbonBalanceSummary) {
    int ncol_cb = 4;
    List out_cb(ncol_cb);
    CharacterVector colnames_cb(ncol_cb);
    for(int i = 0; i<ncol_cb; i++) out_cb[i] = NumericVector(nX, 0.0);
    colnames_cb[CBCOM_GrossPrimaryProduction] = "GrossPrimaryProduction";
    colnames_cb[CBCOM_MaintenanceRespiration] = "MaintenanceRespiration";
    colnames_cb[CBCOM_SynthesisRespiration] = "SynthesisRespiration";
    colnames_cb[CBCOM_NetPrimaryProduction] = "NetPrimaryProduction";
    out_cb.attr("names") = colnames_cb;
    DataFrame cb(out_cb);
    l.push_back(cb, "WatershedCarbonBalance");
  }
  if(biomassBalanceSummary) {
    int ncol_bb = 5;
    List out_bb(ncol_bb);
    CharacterVector colnames_bb(ncol_bb);
    for(int i = 0; i<ncol_bb; i++) out_bb[i] = NumericVector(nX, 0.0);
    colnames_bb[BBCOM_StructuralBalance] = "StructuralBalance";
    colnames_bb[BBCOM_LabileBalance] = "LabileBalance";
    colnames_bb[BBCOM_PlantBalance] = "PlantBalance";
    colnames_bb[BBCOM_MortalityLoss] = "MortalityLoss";
    colnames_bb[BBCOM_CohortBalance] = "CohortBalance";
    out_bb.attr("names") = colnames_bb;
    DataFrame bb(out_bb);
    l.push_back(bb, "WatershedBiomassBalance");
  }
  return(l);
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
List fcpp_landunit_day(List xi, String model, CharacterVector date, List internalCommunication, 
                       bool standSummary, bool carbonBalanceSummary, bool biomassBalanceSummary) {
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
      if(model=="spwb") {
        res = medfate::copy_model_output(internalCommunication, x, "spwb");
      } else if(model=="growth") {
        res = medfate::copy_model_output(internalCommunication, x, "growth");
      }
    } else {
      List spwbOut;
      if (transpirationMode=="Granier"){
        spwbOut = internalCommunication["basicSPWBOutput"];
      } else {
        spwbOut = internalCommunication["advancedSPWBOutput"];
      }
      res = List::create(_["WaterBalance"] = clone(as<NumericVector>(spwbOut["WaterBalance"])));
      if(standSummary && spwbOut.containsElementNamed("Stand")) {
        res.push_back(clone(as<NumericVector>(spwbOut["Stand"])),"Stand");
      }
      if(model=="growth") {
        List growthOut;
        if (transpirationMode=="Granier"){
          growthOut = internalCommunication["basicGROWTHOutput"];
        } else {
          growthOut = internalCommunication["advancedGROWTHOutput"];
        }
        if(carbonBalanceSummary && growthOut.containsElementNamed("CarbonBalance")) {
          res.push_back(clone(as<NumericVector>(growthOut["CarbonBalance"])),"CarbonBalance");
        }
        if(biomassBalanceSummary && growthOut.containsElementNamed("PlantBiomassBalance")) {
          res.push_back(clone(as<DataFrame>(growthOut["PlantBiomassBalance"])),"PlantBiomassBalance");
        }
      }
    }
  }

  List out = List::create(_["final_state"] = x, 
                          _["simulation_results"] = res);
  return(out);
}

