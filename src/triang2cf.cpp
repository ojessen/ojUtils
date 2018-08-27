#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector seqC(double x, double y, double by) {
  
  // length of result vector
  int nRatio = (y - x) / by;
  NumericVector anOut(nRatio + 1);
  
  // compute sequence
  int n;
  int i = x;
  for (n = 0; n < anOut.length(); n++) {
    anOut[n] = i;
    i += by;
  }
  
  return anOut;
}

// [[Rcpp::export]]
NumericVector triang2cf_C(NumericMatrix triang) {
  NumericVector out(triang.rows()-1);
  NumericVector idx_rows;
  NumericVector idx_cols;
  int i;
  int j;
  double tmp;
  for(i =0; i < out.length();i++){
    idx_rows = seqC(triang.rows()-1, i+1,-1);
    idx_cols = seqC(i+1,triang.rows()-1,1);
    tmp = 0;
    for(j = 0; j < idx_rows.length();j++){
      tmp = tmp + triang[idx_rows[j], idx_cols[j]];
    };
    out[i] = tmp;
  };
  return(out);
}


