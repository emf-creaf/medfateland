#include <Rcpp.h>

#ifndef SPWBLAND_H
#define SPWBLAND_H
#endif
using namespace Rcpp;


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

void copySnowpackToSoil(List y);
void copySnowpackFromSoil(List y);
void resetWaterBalanceDayOutput(DataFrame outWB);
List createDayOutput(int nX, 
                     bool standSummary, bool carbonBalanceSummary, bool biomassBalanceSummary);
List fcpp_landunit_day(List xi, String model, CharacterVector date, List internalCommunication, 
                       bool standSummary, bool carbonBalanceSummary, bool biomassBalanceSummary);