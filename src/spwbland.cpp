// [[Rcpp::depends(medfate,meteoland)]]
#include <numeric>
#include <Rcpp.h>
#include <meteoland.h>
#include <medfate.h>
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


// [[Rcpp::export(".watershedDayTetis")]]
List watershedDayTetis(String localModel,
                  CharacterVector lct, List xList,
                  IntegerVector waterO, List queenNeigh, List waterQ,
                  NumericVector depth_to_bedrock, NumericVector bedrock_conductivity, NumericVector bedrock_porosity,
                  NumericVector aquifer, NumericVector snowpack,
                  List watershed_control,
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

  List tetis_parameters = watershed_control["tetis_parameters"];
  double R_interflow = tetis_parameters["R_interflow"];
  double R_baseflow = tetis_parameters["R_baseflow"];

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
      List soil = Rcpp::as<Rcpp::List>(x["soil"]);
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
      List x = Rcpp::as<Rcpp::List>(xList[i]);
      List soil = Rcpp::as<Rcpp::List>(x["soil"]);
      double D = soil["SoilDepth"]; //Soil depth in mm
      NumericVector clay = soil["clay"];
      NumericVector sand = soil["sand"];
      NumericVector om = soil["om"];
      double Ks1 = 0.01*medfate::soil_saturatedConductivitySX(clay[1], sand[1], om[1], false); //cm/day to m/day
      double Kinterflow = R_interflow*Ks1;
      if(WTD[i]<D) {
        double T = ((Kinterflow*D*0.001)/n)*pow(1.0-(WTD[i]/D),n); //Transmissivity in m2
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
    double Kbaseflow = R_baseflow*bedrock_conductivity[i]; //m/day
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
        List soil = Rcpp::as<Rcpp::List>(x["soil"]);
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
      List x = Rcpp::as<Rcpp::List>(xList[i]);
      List soil = Rcpp::as<Rcpp::List>(x["soil"]);
      double D = soil["SoilDepth"];
      if(DTAn<D){
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

    MinTemperature[iCell] = tminVec[iCell];
    MaxTemperature[iCell] = tmaxVec[iCell];
    
    if((lct[iCell]=="wildland") || (lct[iCell]=="agriculture")) {
      List x = Rcpp::as<Rcpp::List>(xList[iCell]);
      List soil = Rcpp::as<Rcpp::List>(x["soil"]);
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
        res = medfate::aspwb_day(x, date, meteovec,
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
      snowpack[iCell] = soil["SWE"]; //Copy back snowpack
      NumericVector DB = res["WaterBalance"];
      DataFrame SB = Rcpp::as<Rcpp::DataFrame>(res["Soil"]);
      Snow[iCell] = DB["Snow"];
      Snowmelt[iCell] = DB["Snowmelt"];
      PET[iCell] = DB["PET"];
      Rain[iCell] = DB["Rain"];
      Infiltration[iCell] = DB["Infiltration"];
      Runoff[iCell] = DB["Runoff"];
      DeepDrainage[iCell] = DB["DeepDrainage"];
      SoilEvaporation[iCell] = DB["SoilEvaporation"];
      NetRain[iCell] = DB["NetRain"];
      if(lct[iCell]=="wildland") {
        DataFrame PL = Rcpp::as<Rcpp::DataFrame>(res["Plants"]);
        NumericVector EplantCoh = Rcpp::as<Rcpp::NumericVector>(PL["Transpiration"]);
        Transpiration[iCell] = sum(EplantCoh);
      } else {
        Transpiration[iCell] = DB["Transpiration"];
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
        Transpiration[iCell] = 0.0;
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

// [[Rcpp::export(".initSerghei")]]
List initSerghei(NumericVector limits, int nrow, int ncol,
                 IntegerVector sf2cell, List xList,
                 String input_dir, String output_dir) {
  // "rock" should have a soil with all rock and zero Ksat
  // "artificial" should have a missing soil
  // "water", "agriculture" and "wildland" should have normal soil
  int nTargetCells = xList.size();
  int nGridCells = nrow*ncol;
  //Initialize Uptake and throughfall
  NumericVector throughfall(nGridCells, NA_REAL);
  List soilListSerghei(nGridCells);
  List uptake(nGridCells);
  for(int i=0;i<nTargetCells; i++) {
    List x = Rcpp::as<Rcpp::List>(xList[i]);
    if(!x.isNULL()) {
      if(x.containsElementNamed("soil")) {
        List soil = Rcpp::as<Rcpp::List>(x["soil"]);
        soilListSerghei[sf2cell[i] - 1] = clone(soil); //indices in R
        NumericVector W = soil["W"];
        // NumericVector dVec = soil["dVec"];
        // for(int l=0;l<dVec.size();l++) Rcout<< dVec[l]<<" ";
        // Rcout<<"\n";
        NumericVector vup(W.size(), NA_REAL);
        uptake[sf2cell[i] - 1] = vup; //indices in R
      }
    }
  }
  // Initialize SERGHEI (call to interface function)
  // Use input_dir and output_dir
  // initializeSerghei(soilListSerghei, uptake, throughfall);
  List serghei_interface = List::create(_["limits"] = limits,
                                        _["dim"] = IntegerVector::create(nrow,ncol),
                                        _["soilList"] = soilListSerghei,
                                        _["throughfall"] = throughfall,
                                        _["uptake"] = uptake);
  return(serghei_interface);
}

// [[Rcpp::export(".callSergheiDay")]]
void callSergheiDay(CharacterVector lct, List xList,
                    DataFrame gridMeteo, List localResults,
                    IntegerVector sf2cell, List serghei_interface) {
  int nX = xList.size();
  
  NumericVector precVec = gridMeteo["Precipitation"];
  
  //B. SERGHEI
  //B.0 - Recover pointers to SERGHEI interface
  List soilListSerghei = serghei_interface["soilList"];
  NumericVector throughfallSerghei = serghei_interface["throughfall"];
  List uptakeSerghei = serghei_interface["uptake"];
  
  //B.1 - Fill uptake and throughfall lists
  for(int i=0;i<nX;i++) {
    if((lct[i]=="wildland") || (lct[i]=="agriculture")) {
      NumericVector vup = Rcpp::as<Rcpp::NumericVector>(uptakeSerghei[sf2cell[i] - 1]); //indices in R
      // Rcout<<".";
      List res_i = localResults[i];
      NumericVector DB = res_i["WaterBalance"];
      DataFrame SB = Rcpp::as<Rcpp::DataFrame>(res_i["Soil"]);
      //Copy throughfall
      throughfallSerghei[sf2cell[i] - 1] = DB["NetRain"]; //indices in R
      //Adding soil evaporation and snowmelt to plant uptake
      double Snowmelt = DB["Snowmelt"];
      double Esoil = DB["SoilEvaporation"];
      NumericVector ExtractionVec = Rcpp::as<Rcpp::NumericVector>(SB["PlantExtraction"]);
      NumericVector EherbVec(ExtractionVec.size(), 0.0);
      if(lct[i]=="wildland") {
        EherbVec = Rcpp::as<Rcpp::NumericVector>(SB["HerbTranspiration"]);
      }
      for(int l=0;l<vup.size();l++) {
        vup[l] = -1.0*(EherbVec[l] + ExtractionVec[l]); // uptake from herbs and plants are negative flows (outflow)
      }
      vup[0] += Snowmelt - Esoil; //snowmelt is a positive flow (inflow) and soil evaporation a negative flow (outflow)
    } else {
      // Rcout<<"+";
      // No interception (i.e. throughfall = rain) in "rock", "artificial" or "water"
      throughfallSerghei[sf2cell[i] - 1] = precVec[i]; //indices in R
      // No uptake in "rock", "artificial" or "water"
      // for(int l=0;l<vup.size();l++) vup[l] = 0.0;
    }
    
  }
  
  // CALL SERGHEI (call to interface function)
  
  //B.3 - Recover new soil moisture state from SERGHEI, calculate the difference between 
  //SERGHEI and MEDFATE and apply the differences to the soil moisture (overall or water pools)
  for(int i=0;i<nX;i++) {
    if((lct[i]=="wildland") || (lct[i]=="agriculture")) {
      // Rcout<<".";
      List soilSerghei = soilListSerghei[sf2cell[i] - 1];  //indices in R
      List x = Rcpp::as<Rcpp::List>(xList[i]);
      if(!x.isNULL()) {
        if(x.containsElementNamed("soil")) {
          List soil = Rcpp::as<Rcpp::List>(x["soil"]);
          NumericVector W_soil = soil["W"];
          NumericVector W_soilSerghei = soilSerghei["W"];
          NumericVector W_diff = W_soilSerghei - W_soil;
          for(int l=0; l<W_soil.size();l++) {
            //Add difference to overall soil
            W_soil[l] = W_soil[i] + W_diff[l];
          }
          //Update water pools if present
          if(x.containsElementNamed("belowLayers")) {
            List belowLayers = Rcpp::as<Rcpp::List>(x["belowLayers"]);
            if(belowLayers.containsElementNamed("Wpool")) {
              NumericMatrix W_mat = belowLayers["Wpool"];
              for(int c=0;c<W_mat.nrow();c++) {
                for(int l=0; l<W_soil.size();l++) {
                  W_mat(c,l) = W_mat(c,l) + W_diff[l];
                }
              }
            }
          }
        }
      }
    }
  }
}

