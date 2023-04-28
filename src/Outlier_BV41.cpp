#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// Autocovariance calculation from given data
arma::vec acovCpp(const arma::vec& Xt, const int lagMax) {
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
  
  arma::vec acov = acovCpp(xt, M);
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
  
// [[Rcpp::export]]    
arma::vec outlierDetectBV41(arma::vec& xt, const double tau = 3, const int M, const int forward = 1) {
  
  arma::vec xtcopy = xt;
  
  // freq <- frequency(xt)
  // stopifnot(freq %in% c(4, 12))
  
  const int n = xt.size();
  
  // if (is.null(M)) {
  // 
  //   if (freq == 4) {
  //   
  //     M <- 8
  //   
  //   } else if (freq == 12) {
  //   
  //     M <- 24
  //   
  //   }
  //   
  // }
  
  
  
  const arma::vec kappa = kappaCpp(acovmatCpp(xt, M));
  const double v = arma::mean(xt);
  
  Rcpp::List condvals = calendarCondValsCpp(xt, M, v, kappa);
  arma::vec condmean = condvals[];
  double condsd = std::sqrt(condvals[]);
  
  arma::vec outlier = arma::zeros<arma::vec>(n);
  
  bool test1;
  bool test2;
  
  for (int i = M; i < n; ++i) {
    
    test1 = (xtcopy(i) < (condmean(i) - tau * condsd));
    test2 = (xtcopy(i) > (condmean(i) + tau * condsd));
    
    if (test1 | test2) {
      
      outlier(i) = 1;
      
      if (test1) {
        
        xtcopy(i) = condmean(i) - tau * condsd;
        
      } else if (test2) {
        
        xtcopy(i) = condmean(i) - tau * condsd;
        
      }
      
      condvals = calendarCondValsCpp(xtcopy, M, v, kappa);
      condmean = condvalues[];
      condsd = std::sqrt(condvalues[]);
      
    }
    

    
  }
  
  if (forward == 1) {
    
    arma::vec yt = arma::reverse(xt);
    
    arma::vec outlierbackw = arma::reverse(outlierDetectBV41(yt, tau, M, 0));
    
    outlier = arma::clamp(outlier + outlierbackw, 0, 1);
    
  }
  
  
  
  return outlier;
  
}
 
// [[Rcpp::export]] 

arma::mat outlierdummies(arma::vec& outlier) {
  
  const int n = outlier.size();
  
  const int m = arma::accu(outlier);
  
  arma::mat outmat = arma::zeros<arma::mat>(n, m);
  
  int j = -1;
  
  for (int i = 0; i < n; ++i) {
    
    if (outlier(i) == 1) {
      
      j = j + 1;
      
      outmat.submat(i, j, n - 1, j) = arma::ones<arma::vec>(n - i);
      
    }
    
  }
  
  return outmat;
  
}
    
  
/*** R
set.seed(123)
M <- 10
xt <- arima.sim(model = list(ar = 0.75), n = 1000, sd = 100)
test <- acovmatCpp(xt, M)
test2 <- kappaCpp(test)
me <- mean(xt)
test3 <- calendarCondValsCpp(xt, M, me, test2)
test3
*/  
  
