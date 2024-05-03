// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// genros
double genros(double phi, double theta, double a1, double a2, double b, double n1, double n2, double c);
RcppExport SEXP _medfateland_genros(SEXP phiSEXP, SEXP thetaSEXP, SEXP a1SEXP, SEXP a2SEXP, SEXP bSEXP, SEXP n1SEXP, SEXP n2SEXP, SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< double >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< double >::type a1(a1SEXP);
    Rcpp::traits::input_parameter< double >::type a2(a2SEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type n1(n1SEXP);
    Rcpp::traits::input_parameter< double >::type n2(n2SEXP);
    Rcpp::traits::input_parameter< double >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(genros(phi, theta, a1, a2, b, n1, n2, c));
    return rcpp_result_gen;
END_RCPP
}
// ellipseROS
NumericVector ellipseROS(NumericVector phi, double theta, double vws, double ros);
RcppExport SEXP _medfateland_ellipseROS(SEXP phiSEXP, SEXP thetaSEXP, SEXP vwsSEXP, SEXP rosSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< double >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< double >::type vws(vwsSEXP);
    Rcpp::traits::input_parameter< double >::type ros(rosSEXP);
    rcpp_result_gen = Rcpp::wrap(ellipseROS(phi, theta, vws, ros));
    return rcpp_result_gen;
END_RCPP
}
// doubleEllipseROS
NumericVector doubleEllipseROS(NumericVector phi, double theta, double vws, double ros);
RcppExport SEXP _medfateland_doubleEllipseROS(SEXP phiSEXP, SEXP thetaSEXP, SEXP vwsSEXP, SEXP rosSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< double >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< double >::type vws(vwsSEXP);
    Rcpp::traits::input_parameter< double >::type ros(rosSEXP);
    rcpp_result_gen = Rcpp::wrap(doubleEllipseROS(phi, theta, vws, ros));
    return rcpp_result_gen;
END_RCPP
}
// fireBrandFallingHeight
double fireBrandFallingHeight(double initialHeight, double timeFalling, double Dp);
RcppExport SEXP _medfateland_fireBrandFallingHeight(SEXP initialHeightSEXP, SEXP timeFallingSEXP, SEXP DpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type initialHeight(initialHeightSEXP);
    Rcpp::traits::input_parameter< double >::type timeFalling(timeFallingSEXP);
    Rcpp::traits::input_parameter< double >::type Dp(DpSEXP);
    rcpp_result_gen = Rcpp::wrap(fireBrandFallingHeight(initialHeight, timeFalling, Dp));
    return rcpp_result_gen;
END_RCPP
}
// totalFirebrandLoftingTime
double totalFirebrandLoftingTime(double z, double z0, double zF, double Dp);
RcppExport SEXP _medfateland_totalFirebrandLoftingTime(SEXP zSEXP, SEXP z0SEXP, SEXP zFSEXP, SEXP DpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type z0(z0SEXP);
    Rcpp::traits::input_parameter< double >::type zF(zFSEXP);
    Rcpp::traits::input_parameter< double >::type Dp(DpSEXP);
    rcpp_result_gen = Rcpp::wrap(totalFirebrandLoftingTime(z, z0, zF, Dp));
    return rcpp_result_gen;
END_RCPP
}
// totalGasFlowPersistenceTime
double totalGasFlowPersistenceTime(double z, double t0, double zF);
RcppExport SEXP _medfateland_totalGasFlowPersistenceTime(SEXP zSEXP, SEXP t0SEXP, SEXP zFSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type t0(t0SEXP);
    Rcpp::traits::input_parameter< double >::type zF(zFSEXP);
    rcpp_result_gen = Rcpp::wrap(totalGasFlowPersistenceTime(z, t0, zF));
    return rcpp_result_gen;
END_RCPP
}
// findFireBrandLoftedHeight
double findFireBrandLoftedHeight(double t0, double z0, double zF, double Dp);
RcppExport SEXP _medfateland_findFireBrandLoftedHeight(SEXP t0SEXP, SEXP z0SEXP, SEXP zFSEXP, SEXP DpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type t0(t0SEXP);
    Rcpp::traits::input_parameter< double >::type z0(z0SEXP);
    Rcpp::traits::input_parameter< double >::type zF(zFSEXP);
    Rcpp::traits::input_parameter< double >::type Dp(DpSEXP);
    rcpp_result_gen = Rcpp::wrap(findFireBrandLoftedHeight(t0, z0, zF, Dp));
    return rcpp_result_gen;
END_RCPP
}
// willBurnWhenHitFloor
bool willBurnWhenHitFloor(double zIni, double Dp);
RcppExport SEXP _medfateland_willBurnWhenHitFloor(SEXP zIniSEXP, SEXP DpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type zIni(zIniSEXP);
    Rcpp::traits::input_parameter< double >::type Dp(DpSEXP);
    rcpp_result_gen = Rcpp::wrap(willBurnWhenHitFloor(zIni, Dp));
    return rcpp_result_gen;
END_RCPP
}
// fireBrandBurningTimeFromCanopyStructure
double fireBrandBurningTimeFromCanopyStructure(double LAIc);
RcppExport SEXP _medfateland_fireBrandBurningTimeFromCanopyStructure(SEXP LAIcSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type LAIc(LAIcSEXP);
    rcpp_result_gen = Rcpp::wrap(fireBrandBurningTimeFromCanopyStructure(LAIc));
    return rcpp_result_gen;
END_RCPP
}
// fireBrandFlameHeightFromCanopyStructure
double fireBrandFlameHeightFromCanopyStructure(double crownLength, double LAIc);
RcppExport SEXP _medfateland_fireBrandFlameHeightFromCanopyStructure(SEXP crownLengthSEXP, SEXP LAIcSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type crownLength(crownLengthSEXP);
    Rcpp::traits::input_parameter< double >::type LAIc(LAIcSEXP);
    rcpp_result_gen = Rcpp::wrap(fireBrandFlameHeightFromCanopyStructure(crownLength, LAIc));
    return rcpp_result_gen;
END_RCPP
}
// drainageCells
IntegerVector drainageCells(List queenNeigh, List waterQ, int iCell);
RcppExport SEXP _medfateland_drainageCells(SEXP queenNeighSEXP, SEXP waterQSEXP, SEXP iCellSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type queenNeigh(queenNeighSEXP);
    Rcpp::traits::input_parameter< List >::type waterQ(waterQSEXP);
    Rcpp::traits::input_parameter< int >::type iCell(iCellSEXP);
    rcpp_result_gen = Rcpp::wrap(drainageCells(queenNeigh, waterQ, iCell));
    return rcpp_result_gen;
END_RCPP
}
// getTrackSpeciesTranspiration
NumericVector getTrackSpeciesTranspiration(NumericVector trackSpecies, NumericVector Eplant, DataFrame x);
RcppExport SEXP _medfateland_getTrackSpeciesTranspiration(SEXP trackSpeciesSEXP, SEXP EplantSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type trackSpecies(trackSpeciesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Eplant(EplantSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(getTrackSpeciesTranspiration(trackSpecies, Eplant, x));
    return rcpp_result_gen;
END_RCPP
}
// getTrackSpeciesDDS
NumericVector getTrackSpeciesDDS(NumericVector trackSpecies, NumericVector DDS, DataFrame x);
RcppExport SEXP _medfateland_getTrackSpeciesDDS(SEXP trackSpeciesSEXP, SEXP DDSSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type trackSpecies(trackSpeciesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type DDS(DDSSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(getTrackSpeciesDDS(trackSpecies, DDS, x));
    return rcpp_result_gen;
END_RCPP
}
// copySnowpackToSoil
void copySnowpackToSoil(List y);
RcppExport SEXP _medfateland_copySnowpackToSoil(SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type y(ySEXP);
    copySnowpackToSoil(y);
    return R_NilValue;
END_RCPP
}
// tetisModifyKsat
void tetisModifyKsat(List y, List watershed_control, bool reverse);
RcppExport SEXP _medfateland_tetisModifyKsat(SEXP ySEXP, SEXP watershed_controlSEXP, SEXP reverseSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type y(ySEXP);
    Rcpp::traits::input_parameter< List >::type watershed_control(watershed_controlSEXP);
    Rcpp::traits::input_parameter< bool >::type reverse(reverseSEXP);
    tetisModifyKsat(y, watershed_control, reverse);
    return R_NilValue;
END_RCPP
}
// copySnowpackFromSoil
void copySnowpackFromSoil(List y);
RcppExport SEXP _medfateland_copySnowpackFromSoil(SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type y(ySEXP);
    copySnowpackFromSoil(y);
    return R_NilValue;
END_RCPP
}
// copyStateFromResults
void copyStateFromResults(List y, List localResults);
RcppExport SEXP _medfateland_copyStateFromResults(SEXP ySEXP, SEXP localResultsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type y(ySEXP);
    Rcpp::traits::input_parameter< List >::type localResults(localResultsSEXP);
    copyStateFromResults(y, localResults);
    return R_NilValue;
END_RCPP
}
// tetisWatershedFlows
DataFrame tetisWatershedFlows(List y, IntegerVector waterO, List queenNeigh, List waterQ, List watershed_control, double patchsize);
RcppExport SEXP _medfateland_tetisWatershedFlows(SEXP ySEXP, SEXP waterOSEXP, SEXP queenNeighSEXP, SEXP waterQSEXP, SEXP watershed_controlSEXP, SEXP patchsizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type y(ySEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type waterO(waterOSEXP);
    Rcpp::traits::input_parameter< List >::type queenNeigh(queenNeighSEXP);
    Rcpp::traits::input_parameter< List >::type waterQ(waterQSEXP);
    Rcpp::traits::input_parameter< List >::type watershed_control(watershed_controlSEXP);
    Rcpp::traits::input_parameter< double >::type patchsize(patchsizeSEXP);
    rcpp_result_gen = Rcpp::wrap(tetisWatershedFlows(y, waterO, queenNeigh, waterQ, watershed_control, patchsize));
    return rcpp_result_gen;
END_RCPP
}
// tetisApplyBaseflowChangesToAquifer
NumericVector tetisApplyBaseflowChangesToAquifer(List y, NumericVector baseflowBalance, double patchsize);
RcppExport SEXP _medfateland_tetisApplyBaseflowChangesToAquifer(SEXP ySEXP, SEXP baseflowBalanceSEXP, SEXP patchsizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type baseflowBalance(baseflowBalanceSEXP);
    Rcpp::traits::input_parameter< double >::type patchsize(patchsizeSEXP);
    rcpp_result_gen = Rcpp::wrap(tetisApplyBaseflowChangesToAquifer(y, baseflowBalance, patchsize));
    return rcpp_result_gen;
END_RCPP
}
// tetisApplyLocalFlowsToAquifer
void tetisApplyLocalFlowsToAquifer(List y, NumericVector CapillarityRise, NumericVector DeepDrainage);
RcppExport SEXP _medfateland_tetisApplyLocalFlowsToAquifer(SEXP ySEXP, SEXP CapillarityRiseSEXP, SEXP DeepDrainageSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type CapillarityRise(CapillarityRiseSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type DeepDrainage(DeepDrainageSEXP);
    tetisApplyLocalFlowsToAquifer(y, CapillarityRise, DeepDrainage);
    return R_NilValue;
END_RCPP
}
// tetisOverlandFlows
NumericVector tetisOverlandFlows(NumericVector Runoff, NumericVector AquiferExfiltration, NumericVector waterO, List queenNeigh, List waterQ);
RcppExport SEXP _medfateland_tetisOverlandFlows(SEXP RunoffSEXP, SEXP AquiferExfiltrationSEXP, SEXP waterOSEXP, SEXP queenNeighSEXP, SEXP waterQSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type Runoff(RunoffSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type AquiferExfiltration(AquiferExfiltrationSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type waterO(waterOSEXP);
    Rcpp::traits::input_parameter< List >::type queenNeigh(queenNeighSEXP);
    Rcpp::traits::input_parameter< List >::type waterQ(waterQSEXP);
    rcpp_result_gen = Rcpp::wrap(tetisOverlandFlows(Runoff, AquiferExfiltration, waterO, queenNeigh, waterQ));
    return rcpp_result_gen;
END_RCPP
}
// tetisSimulationNonSoilCells
DataFrame tetisSimulationNonSoilCells(List y, NumericVector tminVec, NumericVector tmaxVec, NumericVector precVec, NumericVector radVec, NumericVector waterO, List queenNeigh, List waterQ, List watershed_control);
RcppExport SEXP _medfateland_tetisSimulationNonSoilCells(SEXP ySEXP, SEXP tminVecSEXP, SEXP tmaxVecSEXP, SEXP precVecSEXP, SEXP radVecSEXP, SEXP waterOSEXP, SEXP queenNeighSEXP, SEXP waterQSEXP, SEXP watershed_controlSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tminVec(tminVecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tmaxVec(tmaxVecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type precVec(precVecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type radVec(radVecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type waterO(waterOSEXP);
    Rcpp::traits::input_parameter< List >::type queenNeigh(queenNeighSEXP);
    Rcpp::traits::input_parameter< List >::type waterQ(waterQSEXP);
    Rcpp::traits::input_parameter< List >::type watershed_control(watershed_controlSEXP);
    rcpp_result_gen = Rcpp::wrap(tetisSimulationNonSoilCells(y, tminVec, tmaxVec, precVec, radVec, waterO, queenNeigh, waterQ, watershed_control));
    return rcpp_result_gen;
END_RCPP
}
// initSerghei
List initSerghei(NumericVector limits, int nrow, int ncol, IntegerVector sf2cell, List xList, String input_dir, String output_dir);
RcppExport SEXP _medfateland_initSerghei(SEXP limitsSEXP, SEXP nrowSEXP, SEXP ncolSEXP, SEXP sf2cellSEXP, SEXP xListSEXP, SEXP input_dirSEXP, SEXP output_dirSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type limits(limitsSEXP);
    Rcpp::traits::input_parameter< int >::type nrow(nrowSEXP);
    Rcpp::traits::input_parameter< int >::type ncol(ncolSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type sf2cell(sf2cellSEXP);
    Rcpp::traits::input_parameter< List >::type xList(xListSEXP);
    Rcpp::traits::input_parameter< String >::type input_dir(input_dirSEXP);
    Rcpp::traits::input_parameter< String >::type output_dir(output_dirSEXP);
    rcpp_result_gen = Rcpp::wrap(initSerghei(limits, nrow, ncol, sf2cell, xList, input_dir, output_dir));
    return rcpp_result_gen;
END_RCPP
}
// callSergheiDay
void callSergheiDay(CharacterVector lct, List xList, DataFrame gridMeteo, List localResults, IntegerVector sf2cell, List serghei_interface);
RcppExport SEXP _medfateland_callSergheiDay(SEXP lctSEXP, SEXP xListSEXP, SEXP gridMeteoSEXP, SEXP localResultsSEXP, SEXP sf2cellSEXP, SEXP serghei_interfaceSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type lct(lctSEXP);
    Rcpp::traits::input_parameter< List >::type xList(xListSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type gridMeteo(gridMeteoSEXP);
    Rcpp::traits::input_parameter< List >::type localResults(localResultsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type sf2cell(sf2cellSEXP);
    Rcpp::traits::input_parameter< List >::type serghei_interface(serghei_interfaceSEXP);
    callSergheiDay(lct, xList, gridMeteo, localResults, sf2cell, serghei_interface);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_medfateland_genros", (DL_FUNC) &_medfateland_genros, 8},
    {"_medfateland_ellipseROS", (DL_FUNC) &_medfateland_ellipseROS, 4},
    {"_medfateland_doubleEllipseROS", (DL_FUNC) &_medfateland_doubleEllipseROS, 4},
    {"_medfateland_fireBrandFallingHeight", (DL_FUNC) &_medfateland_fireBrandFallingHeight, 3},
    {"_medfateland_totalFirebrandLoftingTime", (DL_FUNC) &_medfateland_totalFirebrandLoftingTime, 4},
    {"_medfateland_totalGasFlowPersistenceTime", (DL_FUNC) &_medfateland_totalGasFlowPersistenceTime, 3},
    {"_medfateland_findFireBrandLoftedHeight", (DL_FUNC) &_medfateland_findFireBrandLoftedHeight, 4},
    {"_medfateland_willBurnWhenHitFloor", (DL_FUNC) &_medfateland_willBurnWhenHitFloor, 2},
    {"_medfateland_fireBrandBurningTimeFromCanopyStructure", (DL_FUNC) &_medfateland_fireBrandBurningTimeFromCanopyStructure, 1},
    {"_medfateland_fireBrandFlameHeightFromCanopyStructure", (DL_FUNC) &_medfateland_fireBrandFlameHeightFromCanopyStructure, 2},
    {"_medfateland_drainageCells", (DL_FUNC) &_medfateland_drainageCells, 3},
    {"_medfateland_getTrackSpeciesTranspiration", (DL_FUNC) &_medfateland_getTrackSpeciesTranspiration, 3},
    {"_medfateland_getTrackSpeciesDDS", (DL_FUNC) &_medfateland_getTrackSpeciesDDS, 3},
    {"_medfateland_copySnowpackToSoil", (DL_FUNC) &_medfateland_copySnowpackToSoil, 1},
    {"_medfateland_tetisModifyKsat", (DL_FUNC) &_medfateland_tetisModifyKsat, 3},
    {"_medfateland_copySnowpackFromSoil", (DL_FUNC) &_medfateland_copySnowpackFromSoil, 1},
    {"_medfateland_copyStateFromResults", (DL_FUNC) &_medfateland_copyStateFromResults, 2},
    {"_medfateland_tetisWatershedFlows", (DL_FUNC) &_medfateland_tetisWatershedFlows, 6},
    {"_medfateland_tetisApplyBaseflowChangesToAquifer", (DL_FUNC) &_medfateland_tetisApplyBaseflowChangesToAquifer, 3},
    {"_medfateland_tetisApplyLocalFlowsToAquifer", (DL_FUNC) &_medfateland_tetisApplyLocalFlowsToAquifer, 3},
    {"_medfateland_tetisOverlandFlows", (DL_FUNC) &_medfateland_tetisOverlandFlows, 5},
    {"_medfateland_tetisSimulationNonSoilCells", (DL_FUNC) &_medfateland_tetisSimulationNonSoilCells, 9},
    {"_medfateland_initSerghei", (DL_FUNC) &_medfateland_initSerghei, 7},
    {"_medfateland_callSergheiDay", (DL_FUNC) &_medfateland_callSergheiDay, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_medfateland(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
