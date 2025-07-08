#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector count_leftovers(IntegerVector& x,
                              int min_value = 0){

  int n = x.size();

  IntegerVector out(n);

  out[0] = std::max(x[0], min_value);

  if (n == 1) return out;

  for (int i = 1; i < n; ++i) {
    out[i] = std::max(out[i - 1] + x[i], min_value);
  }

  return out;

}
