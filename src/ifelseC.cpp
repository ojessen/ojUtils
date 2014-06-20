#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector ifelseCNum(LogicalVector test, NumericVector yes, NumericVector no) {
  int n_test = test.size(), n_yes = yes.size(), n_no = no.size();
  if(n_test != n_yes | n_yes != n_no) stop("Different length of input vectors");
  
  NumericVector out(n_test);
  LogicalVector test_na = is_na(test);
  for(int i = 0; i< n_test; i++){
    if(test_na[i]){
      out[i] = NA_REAL;
    } else if(test[i]){
      out[i] = yes[i];
    } else {
      out[i] = no[i];
    }
  }
  
  return out;
}

// [[Rcpp::export]]
CharacterVector ifelseCChar(LogicalVector test, CharacterVector yes, CharacterVector no) {
  int n_test = test.size(), n_yes = yes.size(), n_no = no.size();
  if(n_test != n_yes | n_yes != n_no) stop("Different length of input vectors");
  
  CharacterVector out(n_test);
  LogicalVector test_na = is_na(test);
  for(int i = 0; i< n_test; i++){
    if(test_na[i]){
      out[i] = NA_STRING;
    } else if(test[i]){
      out[i] = yes[i];
    } else {
      out[i] = no[i];
    }
  }
  
  return out;
}

// [[Rcpp::export]]
LogicalVector ifelseCLogic(LogicalVector test, LogicalVector yes, LogicalVector no) {
  int n_test = test.size(), n_yes = yes.size(), n_no = no.size();
  if(n_test != n_yes | n_yes != n_no) stop("Different length of input vectors");
  
  LogicalVector out(n_test);
  LogicalVector test_na = is_na(test);
  for(int i = 0; i< n_test; i++){
    if(test_na[i]){
      out[i] = NA_LOGICAL;
    } else if(test[i]){
      out[i] = yes[i];
    } else {
      out[i] = no[i];
    }
  }
  
  return out;
}

// [[Rcpp::export]]
IntegerVector ifelseCInt(LogicalVector test, IntegerVector yes, IntegerVector no) {
  int n_test = test.size(), n_yes = yes.size(), n_no = no.size();
  if(n_test != n_yes | n_yes != n_no) stop("Different length of input vectors");
  
  IntegerVector out(n_test);
  LogicalVector test_na = is_na(test);
  for(int i = 0; i< n_test; i++){
    if(test_na[i]){
      out[i] = NA_INTEGER;
    } else if(test[i]){
      out[i] = yes[i];
    } else {
      out[i] = no[i];
    }
  }
  
  return out;
}