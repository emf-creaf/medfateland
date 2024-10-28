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

