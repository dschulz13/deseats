#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]    
arma::mat JumpVarsBV41Cpp(arma::vec& jumpPoints, const int n) {
  
  const int m = jumpPoints.size();
  
  arma::mat jumpMat = arma::zeros<arma::mat>(n, m);
  
  for (int i = 0; i < m; ++i) {
    
    jumpMat.submat(jumpPoints(i) - 1, i, n - 1, i) = arma::ones<arma::vec>(n - jumpPoints(i) + 1);
    
  }
  
  return jumpMat;
  
}
 
