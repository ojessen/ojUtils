#include <Rcpp.h>
using namespace Rcpp;

template <typename Vec>
Vec safe_ifelse( LogicalVector test, Vec yes, Vec no){
    int n_test = test.size(), n_yes = yes.size(), n_no = no.size();
    if(n_test != n_yes | n_yes != n_no) stop("Different length of input vectors");
    return ifelse(test, yes, no);
}

// [[Rcpp::export]]
NumericVector ifelseCNum(LogicalVector test, NumericVector yes, NumericVector no) {
  return safe_ifelse(test, yes, no) ;
}

// [[Rcpp::export]]
CharacterVector ifelseCChar(LogicalVector test, CharacterVector yes, CharacterVector no) {
  return safe_ifelse(test, yes, no) ;
}

// [[Rcpp::export]]
LogicalVector ifelseCLogic(LogicalVector test, LogicalVector yes, LogicalVector no) {
  return safe_ifelse(test, yes, no) ;
}

// [[Rcpp::export]]
IntegerVector ifelseCInt(LogicalVector test, IntegerVector yes, IntegerVector no) {
  return safe_ifelse(test, yes, no) ;
}