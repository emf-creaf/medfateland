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
const int WBCOM_WatershedExport = 26;
const int WBCOM_Interception = 27;

void copySnowpackToSoil(List y);
void copySnowpackFromSoil(List y);
List createDayOutput(List y);
List fcpp_landunit_day(List xi, String model, CharacterVector date, List internalCommunication);