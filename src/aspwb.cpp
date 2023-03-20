#include <meteoland.h>
#include <medfate.h>
using namespace Rcpp;
using namespace medfate;
using namespace meteoland;

NumericVector agricultureSoilWaterInputs(List soil, 
                                         double prec, double tday, double rad, double elevation,
                                         double LgroundSWR, 
                                         double runon = 0.0,
                                         bool snowpack = true, bool modifySoil = true) {
  //Soil input
  double swe = soil["SWE"]; //snow pack
  
  //Snow pack dynamics
  double snow = 0.0, rain=0.0;
  double melt = 0.0;
  if(snowpack) {
    //Turn rain into snow and add it into the snow pack
    if(tday < 0.0) { 
      snow = prec; 
      swe = swe + snow;
    } else {
      rain = prec;
    }
    //Apply snow melting
    if(swe > 0.0) {
      melt = std::min(swe, medfate::hydrology_snowMelt(tday, rad, LgroundSWR, elevation));
      // Rcout<<" swe: "<< swe<<" temp: "<<ten<< " rad: "<< ren << " melt : "<< melt<<"\n";
      swe = swe-melt;
    }
  } else {
    rain = prec;
  }
  
  //Hydrologic input
  double NetRain = 0.0, Interception = 0.0;
  if(rain>0.0)  {
    NetRain = rain - Interception; 
  }
  if(modifySoil) {
    soil["SWE"] = swe;
  }
  NumericVector WI = NumericVector::create(_["Rain"] = rain, _["Snow"] = snow,
                                           _["Interception"] = Interception,
                                           _["NetRain"] = NetRain, 
                                           _["Snowmelt"] = melt,
                                           _["Runon"] = runon,
                                           _["Input"] = runon+melt+NetRain);
  return(WI);
}


// [[Rcpp::export(".aspwbInput")]]
List aspwbInput(double crop_factor, List control, List soil) {
  List input = List::create(_["crop_factor"] = crop_factor, 
                            _["control"] = clone(control),
                            _["soil"] = clone(soil));
  input.attr("class") = CharacterVector::create("aspwbInput","list");
  return(input);
}

List aspwb_day_internal(List x, NumericVector meteovec, 
              double elevation, double slope, double aspect, 
              double runon=0.0, bool verbose=false) {
  
  double crop_factor = x["crop_factor"];
  List control = x["control"];
  List soil = x["soil"];
  bool snowpack = control["snowpack"];
  bool rockyLayerDrainage = control["rockyLayerDrainage"];
  String soilFunctions = control["soilFunctions"];
  
  int nlayers = Rcpp::as<Rcpp::NumericVector>(soil["dVec"]).size();
  
  //Weather input
  double tday = meteovec["tday"];
  double pet = meteovec["pet"]; 
  double prec  = meteovec["prec"];
  double rad = NA_REAL; 
  if(meteovec.containsElementNamed("rad")) rad = meteovec["rad"];

  // Assume 100% SWR
  double LgroundSWR = 100.0;
  
  //Snow pack dynamics and hydrology input
  NumericVector hydroInputs = agricultureSoilWaterInputs(soil, prec,
                                                         tday, rad, elevation,
                                                         LgroundSWR, runon,
                                                         snowpack, true);

  //Soil infiltration and percolation
  NumericVector infilPerc = medfate::hydrology_soilInfiltrationPercolation(soil, soilFunctions, 
                                                             hydroInputs["Input"],
                                                             rockyLayerDrainage, true);
  
  //Evaporation from bare soil (if there is no snow)
  NumericVector EsoilVec = medfate::hydrology_soilEvaporation(soil, soilFunctions, pet, LgroundSWR, true);
  
  // Transpiration is the product of PET and CROP FACTOR. HOWEVER, it is reduced with 
  double transp_max = pet*crop_factor; 
  //Calculate current soil water potential for transpiration
  NumericVector psiVec = medfate::soil_psi(soil, soilFunctions); 
  NumericVector Water_FC = medfate::soil_waterFC(soil, soilFunctions);
  double transp = transp_max * exp(-0.6931472*pow(std::abs(psiVec[0]/(-2.0)),3.0)); //Reduce transpiration when soil is dry
  NumericVector Ws = soil["W"];
  // Rcout << pet << " "<< psiVec[0] <<" "<< transp<< "\n";
  Ws[0] = Ws[0] - (transp/Water_FC[0]); 
  
  //Recalculate current soil water potential for output
  psiVec = medfate::soil_psi(soil, soilFunctions); 
  
  NumericVector DB = NumericVector::create(_["PET"] = pet, 
                                           _["Rain"] = hydroInputs["Rain"], _["Snow"] = hydroInputs["Snow"], 
                                           _["NetRain"] = hydroInputs["NetRain"], _["Snowmelt"] = hydroInputs["Snowmelt"],
                                           _["Runon"] = hydroInputs["Runon"], 
                                           _["Infiltration"] = infilPerc["Infiltration"], _["Runoff"] = infilPerc["Runoff"], _["DeepDrainage"] = infilPerc["DeepDrainage"],
                                           _["SoilEvaporation"] = sum(EsoilVec), _["Transpiration"] = transp);
  
  DataFrame SB = DataFrame::create(_["SoilEvaporation"] = EsoilVec, 
                                   _["psi"] = psiVec);
  List l = List::create(_["WaterBalance"] = DB, 
                        _["Soil"] = SB);
  l.attr("class") = CharacterVector::create("aspwb_day","list");
  return(l);
}

// [[Rcpp::export(".aspwb_day")]]
List aspwb_day(List x, 
               CharacterVector date, 
               double tmin, double tmax, double rhmin, double rhmax, double rad, double wind, 
               double latitude, double elevation, double slope, double aspect,
               double prec, double runon=0.0, bool modifyInput = true) {
  
  List control = x["control"];
  bool verbose = control["verbose"];
  
  
  std::string c = as<std::string>(date[0]);
  int J = meteoland::radiation_julianDay(std::atoi(c.substr(0, 4).c_str()),std::atoi(c.substr(5,2).c_str()),std::atoi(c.substr(8,2).c_str()));
  double delta = meteoland::radiation_solarDeclination(J);
  double solarConstant = meteoland::radiation_solarConstant(J);
  double latrad = latitude * (M_PI/180.0);
  if(NumericVector::is_na(aspect)) aspect = 0.0;
  if(NumericVector::is_na(slope)) slope = 0.0;
  double asprad = aspect * (M_PI/180.0);
  double slorad = slope * (M_PI/180.0);
  double tday = meteoland::utils_averageDaylightTemperature(tmin, tmax);
  double pet = meteoland::penman(latrad, elevation, slorad, asprad, J, tmin, tmax, rhmin, rhmax, rad, wind);
  
  
  //Will not modify input x 
  if(!modifyInput) {
    x = clone(x);
  }
  
  NumericVector meteovec = NumericVector::create(
    Named("tday") = tday, 
    Named("prec") = prec,
    Named("tmin") = tmin, 
    Named("tmax") = tmax,
    Named("rhmin") = rhmin, 
    Named("rhmax") = rhmax, 
    Named("rad") = rad, 
    Named("wind") = wind, 
    Named("pet") = pet);
  
  return(aspwb_day_internal(x, meteovec,
                  elevation, slope, aspect, 
                  runon, verbose));
}