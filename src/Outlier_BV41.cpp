#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// Autocovariance calculation from given data
arma::vec acovCpp2(const arma::vec& Xt, const int lagMax) {
  const double meanX = arma::mean(Xt);
  arma::vec XtDM = Xt - meanX;
  arma::vec acovOut(lagMax + 1);
  const int n = Xt.size();
  for (int i = 0; i < lagMax + 1; ++i) {
    acovOut(i) = arma::accu(XtDM.subvec(0, n - 1 - i) % XtDM.subvec(i, n - 1));
  }
  arma::vec out = acovOut / n;
  return out;
}

// Functions written by Dominik Schulz, 28/04/2023
// [[Rcpp::export]]
arma::mat acovmatCpp(const arma::vec& xt, const int M) {
  
  arma::vec acov = acovCpp2(xt, M);
  arma::vec acov2 = arma::join_cols(arma::reverse(acov.subvec(1, M)), acov);
  
  arma::mat out = arma::zeros<arma::mat>(M + 1, M + 1);
  
  for (int i = 0; i < M + 1; ++i) {
    
    out.col(i) = acov2.subvec(M - i, 2 * M - i);
    
  }

  return out;

}
  
// [[Rcpp::export]]  
arma::vec kappaCpp(const arma::mat& acovmat) {
  
  arma::mat out = arma::inv(acovmat);
  
  return out.col(0);
  
}  
  
// [[Rcpp::export]]   
Rcpp::List calendarCondValsCpp(const arma::vec& xt, const int M, const double v, const arma::vec& kappa) {
  
  const int n = xt.size();
  
  arma::vec Exi = arma::zeros<arma::vec>(n);
  const arma::rowvec tkappaRev = arma::trans(arma::reverse(kappa));
  const arma::vec xtdm = xt - v;
  
  
  for (int i = M; i < n; ++i) {
  
    Exi.subvec(i, i) = tkappaRev.subvec(0, M - 1) * xtdm.subvec(i - M, i - 1);
    
  }   
  
  Exi = v - (1 / kappa(0)) * Exi;
  
  const double varxi = (1.0 / (n - M)) * arma::accu(arma::pow(xt.subvec(M, n - 1) - Exi.subvec(M, n - 1), 2.0));
  
  Rcpp::List Out = List::create(_["cond_mean"] = Exi,
                              _["cond_var"] = varxi);
  
  return Out;
  
}
  
arma::mat outlierdummies(arma::vec& outlier) {
  
  const int n = outlier.size();
  
  const int m = arma::accu(outlier);
  
  arma::mat outmat = arma::zeros<arma::mat>(n, m);
  
  int j = -1;
  
  for (int i = 0; i < n; ++i) {
    
    if (outlier(i) == 1) {
      
      j = j + 1;
      
      outmat.submat(i, j, i, j) = 1;
      
    }
    
  }
  
  return outmat;
  
}  
  
// [[Rcpp::export]]    
Rcpp::List outlierDetectBV41Cpp(arma::vec xt, const double tau = 3, const int M = 0, const int forward = 1) {
  
  const int n = xt.size();
  
  arma::vec yt;
  
  if (forward == 1) {
    
    yt = arma::reverse(xt);
    
  }
  
  const arma::vec kappa = kappaCpp(acovmatCpp(xt, M));
  const double v = arma::mean(xt);
  
  Rcpp::List condvals = calendarCondValsCpp(xt, M, v, kappa);
  arma::vec condmean = condvals["cond_mean"];
  double condvar = condvals["cond_var"];
  double condsd = std::sqrt(condvar);
  
  arma::vec outlier = arma::zeros<arma::vec>(n);
  arma::vec direc = arma::zeros<arma::vec>(n);
  
  bool test1;
  bool test2;
  
  for (int i = M; i < n; ++i) {
    
    test1 = (xt(i) < (condmean(i) - tau * condsd));
    test2 = (xt(i) > (condmean(i) + tau * condsd));
    
    if (test1 | test2) {
      
      outlier(i) = 1;
      
      if (test1) {
        
        xt(i) = condmean(i) - tau * condsd;
        direc(i) = -1.0;
        
      } else if (test2) {
        
        xt(i) = condmean(i) - tau * condsd;
        direc(i) = 1.0;        
        
      }
      
      condvals = calendarCondValsCpp(xt, M, v, kappa);
      arma::vec condmean = condvals["cond_mean"];
      condvar = condvals["cond_var"];
      condsd = std::sqrt(condvar);
      
    }
    

    
  }
  
  if (forward == 1) {
    
    Rcpp::List backw = outlierDetectBV41Cpp(yt, tau, M, 0);
    
    arma::vec out2 = backw["outlier"];
    arma::vec sign2 = backw["sign"];
    
    
    outlier = arma::clamp(outlier + arma::reverse(out2), 0, 1);
    
    direc = arma::clamp(direc + arma::reverse(sign2), -1, 1);
    
  }
  
  if (forward == 0) {
  
    return Rcpp::List::create(_["outlier"] = outlier, _["sign"] = direc);
  
  } else {
    
    arma::mat dummy = outlierdummies(outlier);
    
    return Rcpp::List::create(_["outlier_dummy"] = dummy, _["sign"] = direc);
    
  }
  
}
 
