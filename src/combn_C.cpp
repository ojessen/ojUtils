#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix internal_combn_C(IntegerVector x) {
  int m = 2;
  int n_x = x.size();
  IntegerVector a = seq_len(m);
  IntegerVector x0 = x;
  IntegerVector r = x[a];
  int n_r = r.size();
  int count = Rf_choose(n_x, m);
  IntegerMatrix out(n_r, count);
  for(int i = 0; i < count;i++){
    out( _, i) = r;
  }
  
  int x_max = max(x);
  int start_col = 0;
  for(int i = 1;i < x_max;i ++){
    int tmp_num_col = x_max - i;
    IntegerMatrix tmp_mat(m, tmp_num_col);
    tmp_mat(0,_) = rep(i, tmp_num_col);
    tmp_mat(1,_) = seq((i+1),x_max);
    int last_col = start_col + tmp_mat.ncol()-1;
    
    for(int cl = start_col; cl < last_col+1;cl ++){
      for(int rw = 0; rw < m+1; rw ++){
        out(rw, cl) = tmp_mat(rw,cl-start_col);    
      }
    }
    
    start_col = last_col + 1;
  }
  
  return out;
}