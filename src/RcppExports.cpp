// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// internal_combn_C
IntegerMatrix internal_combn_C(IntegerVector x);
RcppExport SEXP ojUtils_internal_combn_C(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    __result = Rcpp::wrap(internal_combn_C(x));
    return __result;
END_RCPP
}
// ifelseCNum
NumericVector ifelseCNum(LogicalVector test, NumericVector yes, NumericVector no);
RcppExport SEXP ojUtils_ifelseCNum(SEXP testSEXP, SEXP yesSEXP, SEXP noSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< LogicalVector >::type test(testSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type yes(yesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type no(noSEXP);
    __result = Rcpp::wrap(ifelseCNum(test, yes, no));
    return __result;
END_RCPP
}
// ifelseCLogic
LogicalVector ifelseCLogic(LogicalVector test, LogicalVector yes, LogicalVector no);
RcppExport SEXP ojUtils_ifelseCLogic(SEXP testSEXP, SEXP yesSEXP, SEXP noSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< LogicalVector >::type test(testSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type yes(yesSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type no(noSEXP);
    __result = Rcpp::wrap(ifelseCLogic(test, yes, no));
    return __result;
END_RCPP
}
// ifelseCInt
IntegerVector ifelseCInt(LogicalVector test, IntegerVector yes, IntegerVector no);
RcppExport SEXP ojUtils_ifelseCInt(SEXP testSEXP, SEXP yesSEXP, SEXP noSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< LogicalVector >::type test(testSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type yes(yesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type no(noSEXP);
    __result = Rcpp::wrap(ifelseCInt(test, yes, no));
    return __result;
END_RCPP
}
// ifelseCChar
CharacterVector ifelseCChar(LogicalVector test, CharacterVector yes, CharacterVector no);
RcppExport SEXP ojUtils_ifelseCChar(SEXP testSEXP, SEXP yesSEXP, SEXP noSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< LogicalVector >::type test(testSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type yes(yesSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type no(noSEXP);
    __result = Rcpp::wrap(ifelseCChar(test, yes, no));
    return __result;
END_RCPP
}
// seqC
NumericVector seqC(double x, double y, double by);
RcppExport SEXP ojUtils_seqC(SEXP xSEXP, SEXP ySEXP, SEXP bySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type by(bySEXP);
    __result = Rcpp::wrap(seqC(x, y, by));
    return __result;
END_RCPP
}
