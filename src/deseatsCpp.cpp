#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// Functions written by Dominik Schulz, 12/05/2020

// Function to help obtain a sequence from 'from' to 'to' by step 1
// in C++
// [[Rcpp::export]]
arma::vec seqCpp(const int from, const int to){
  const int n = to - from + 1;
  arma::vec seqOut(n);
  for (int i = from; i < to + 1; ++i) {
    seqOut(i - from) = i;
  }
  return seqOut;
}

// Function to help obtain a sequence from 'from' to 'to' by step 1
// in C++ (returns a rowvec instead)
// [[Rcpp::export]]
arma::rowvec rseqCpp(const int from, const int to) {
  const int n = to - from + 1;
  arma::rowvec seqOut(n);
  for (int i = from; i < to + 1; ++i) {
    seqOut(i - from) = i;
  }
  return seqOut;
}

// Function to calculate the factorial of an 'int' in C++
// [[Rcpp::export]]
int factorialCpp(const int k) {
  int fac = 1;
  if (k > 1) {
    for (int i = 2; i < k + 1; ++i) {
      fac *= i;
    }
  }
  return fac;
}


// Fit ARMA
// [[Rcpp::export]]
Rcpp::List armaCpp(const arma::vec& Xt, const int p, const int q) {
  Rcpp::Environment pkg = Rcpp::Environment::namespace_env("deseats");
  // Rcpp::Function f = pkg["arima"];    
  Rcpp::Function f = pkg["arima_no_warn"];
  
  Rcpp::NumericVector order = Rcpp::NumericVector::create(p, 0, q);
  
  return f(Xt, order);
  
}

// [[Rcpp::export]]
double BICarmaCpp(const arma::vec& Xt, const int p, const int q) {
  
  Rcpp::List arma = armaCpp(Xt, p, q);
  const double llhood = arma["loglik"];
  int n = Xt.size();
  return -2.0 * llhood + std::log(n) * (p + q);
  
} 

// [[Rcpp::export]]
Rcpp::NumericVector selectOrderBIC(const arma::vec& Xt, const int pmax, const int qmax) {
  
  arma::mat bicmat = arma::zeros<arma::mat>(pmax + 1, qmax + 1);
  
  for (int p0 = 0; p0 < pmax + 1; ++p0) {
    for (int q0 = 0; q0 < qmax + 1; ++q0) {
      
      bicmat(p0, q0) = BICarmaCpp(Xt, p0, q0);
      
    }
  }
  
  arma::uvec ind = arma::find(bicmat == bicmat.min());
  
  int popt = (int)ind(0);  
  int qopt = popt / 4;

  for (int j = 0; popt >= 4; ++j) {
    popt = popt - 4;
  }

  return Rcpp::NumericVector::create(popt, 0, qopt);
  
}

// [[Rcpp::export]]
Rcpp::List armaoptCpp(const arma::vec& Xt) {
  
  Rcpp::NumericVector order = selectOrderBIC(Xt, 3, 3);
  
  Rcpp::Environment pkg = Rcpp::Environment::namespace_env("deseats");
  // Rcpp::Function f = pkg["arima"];    
  Rcpp::Function f = pkg["arima_no_warn"];
  
  Rcpp::List armaopt = f(Xt, order);
  
  arma::vec coefs = armaopt["coef"];
  
  return armaopt;  
  
}
  
// [[Rcpp::export]]  
double arma_sumacovCpp(Rcpp::List arma) {
  
  const arma::vec info = arma["arma"];
  
  const int p = info(0);
  const int q = info(1);
  
  const arma::vec coef = arma["coef"];
  const double sig2 = arma["sigma2"];
  
  arma::vec ar = arma::zeros<arma::vec>(1);
  arma::vec ma = arma::zeros<arma::vec>(1);  
  
  if (p >= 1) {
    ar = coef.subvec(0, p - 1);
  }
  if (q >= 1) {
    ma = coef.subvec(p, p + q - 1);
  }
  
  return std::pow((1 + arma::accu(ma)) / (1 - arma::accu(ar)), 2.0) * sig2;
  
}  
  
// Autocovariance calculation from given data
// [[Rcpp::export]]
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

// Calculate c1 (Buehlmann) in the global step
// [[Rcpp::export]]
double c1GlobalCpp(const arma::vec& autocovs) {
  const int n = autocovs.size();
  return (std::pow(autocovs(0), 2.0) + 2.0 * arma::accu(arma::pow(autocovs.subvec(1, n - 1), 2.0))) / (4.0 * M_PI);
}

// Calculate c2 (Buehlmann) in the global step
// [[Rcpp::export]]
double c2GlobalCpp(const arma::vec& w_autocovs) {
  return 3.0 * (2.0 * arma::accu(arma::pow(w_autocovs, 2.0))) / (2.0 * M_PI);
}

// Calculate c1 (Buehlmann) in the local step
// [[Rcpp::export]]
double c1LocalCpp(const arma::vec& w_autocovs) {
  return std::pow(2.0 * arma::accu(w_autocovs) - w_autocovs(0), 2.0) / (2.0 * M_PI);
}

// Calculate c2 (Buehlmann) in the local step
// [[Rcpp::export]]
double c2LocalCpp(const arma::vec& w_autocovs) {
  return 3.0 * std::pow(2.0 * arma::accu(w_autocovs), 2.0) / (2.0 * M_PI);
}

// Calculate L1 (Buehlmann)
// [[Rcpp::export]]
int L1Cpp(const int L1old, const int n) {
  return std::trunc(L1old / std::pow(n, 2.0 / 21.0)) + 1;
}

// Calculate weights in global step (Buehlmann)
// [[Rcpp::export]]
arma::vec wGCpp(const int L1) {
   arma::vec x1 = seqCpp(0, L1 - 1) / L1;
   return 1.0 - x1;
}

// Weights in the local step (Buehlmann)
// [[Rcpp::export]]
arma::vec wLCpp(const int L1) {
  arma::vec x1 = seqCpp(0, L1 - 1) / L1;
  return (1.0 + arma::cos(M_PI * x1)) / 2.0;
}

// Obtain weighted autocovariances in global step (Buehlmann)
// [[Rcpp::export]]
arma::vec w_acovG_Cpp(const arma::vec& autocovs, const int L1, const arma::vec& w1) {
  return seqCpp(0, L1 - 1) % autocovs.subvec(0, L1 - 1) % w1;
}

// Weighted autocovariances in the local step (Buehlmann)
// [[Rcpp::export]]
arma::vec w_autocov_L_Cpp(const arma::vec& autocovs, const int L1, const arma::vec& w) {
  return autocovs.subvec(0, L1 - 1) % w;
}

// Update global L (Buehlmann)
// [[Rcpp::export]]
int updateLGCpp(const int n, const double c1, const double c2) {
  return std::trunc(std::pow(n, 1.0 / 3.0) * std::pow(c2 / c1, 1.0 / 3.0)) + 1;
}

// Iterate through the global procedure (Buehlmann)
// [[Rcpp::export]]
int LGoptCpp(const arma::vec& autocovs) {
  
  const int n = autocovs.size();
  const int nit = 20;
  int runc = 1;
  arma::vec L(nit + 1);
  L(0) = std::trunc(n / 2.0 + 0.5);
  const double c1 = c1GlobalCpp(autocovs);
  double c2 = 0;
  int L1 = 0;
  int LGopt = 0;
  
  for(int i = 0; i < nit; i++) {
    if(runc == 1) {
      L1 = L1Cpp(L(i), n);
      arma::vec w1 = wGCpp(L1);
      arma::vec w_autocovs = w_acovG_Cpp(autocovs, L1, w1);
      c2 = c2GlobalCpp(w_autocovs);
      L(i + 1) = updateLGCpp(n, c1, c2);

      if (L(i + 1) == L(i)) {
        runc = 0;
        LGopt = L(i + 1);
      }
    }
  }
  if (runc == 1) {
    LGopt = L(nit);
  }  
  
  return LGopt;
  
}

// Calculate local L (Buehlmann)
// [[Rcpp::export]]
int LLcalcCpp(const int n, const double c1, const double c2) {
  return std::trunc(std::pow(n, 1.0 / 3.0) * std::pow((c2 / c1) / 2.0, 1.0 / 3.0)) + 1;
}

// Conduct the local procedure at frequency 0 (Buehlmann)
// [[Rcpp::export]]
int LLoptCpp(const arma::vec& autocovs, const int LGopt) {
  
  const int n = autocovs.size();
  
  const int L1 = L1Cpp(LGopt, n);
  
  arma::vec w1 = wGCpp(L1);
  arma::vec w_autocovs = w_acovG_Cpp(autocovs, L1, w1);
  const double c2L = c2LocalCpp(w_autocovs);
  const arma::vec w0 = wLCpp(L1);
  arma::vec ga0 = w_autocov_L_Cpp(autocovs, L1, w0);
  const double c1L = c1LocalCpp(ga0);
  
  return LLcalcCpp(n, c1L, c2L);
  
}


// Obtain final weights for autocovariances (Buehlmann)
// [[Rcpp::export]]
arma::vec weights_finalCpp(const int L0opt) {
  return arma::reverse(seqCpp(1, L0opt + 1)) / (L0opt + 1.0);
}

// Calculate cf0 given final weights (Buehlmann)
// [[Rcpp::export]]
double cf0_calc_Cpp(const arma::vec& autocovs_sub, const arma::vec& weights) {
  return 2.0 * arma::accu(autocovs_sub % weights) - autocovs_sub(0);
}

// Buehlmann algorithm for estimating cf0
// [[Rcpp::export]]
double cf0Cpp(const arma::vec& Xt) {
  
  const int n = Xt.size();
  const arma::vec ga = acovCpp(Xt, n - 1);

  const int LGopt = LGoptCpp(ga);
  const int LLopt = LLoptCpp(ga, LGopt);

  const arma::vec wacf = weights_finalCpp(LLopt);
  const arma::vec acfX = ga.subvec(0, LLopt);

  return cf0_calc_Cpp(acfX, wacf);
}

// Given a relative bandwidth, obtain the corresponding one-hand side absolute bandwidth
// [[Rcpp::export]]
int bwidthAbsCpp(const int n, const double bwidth) {
  return std::trunc(n * bwidth + 0.5);
}

// Given an absolute one-hand side bandwidth, obtain the total both-hand side absolute bandwidth
// [[Rcpp::export]]
int bwidthTotalCpp(const int bwidthAbs) {
  return 2 * bwidthAbs + 1;
}

// Set up a single empty matrix for either G, S and so on
// [[Rcpp::export]]
arma::mat setup_wsMat_single_Cpp(const int bwidthTotal) {
  return arma::zeros<arma::mat>(bwidthTotal, bwidthTotal);
}

// Set up wk
// [[Rcpp::export]]
arma::rowvec setup_wk_Cpp(const int bwidthTotal) {
  return arma::zeros<arma::rowvec>(bwidthTotal);
}

// Set up xt
// [[Rcpp::export]]
arma::mat setup_xt_Cpp(const int p, const int s, const int bwidthTotal) {
  return arma::zeros<arma::mat>((p + s), bwidthTotal);
}

// Set of lower indexes
// [[Rcpp::export]]
arma::vec lowIndexCpp(const int bwidthAbs, const arma::vec& seqhh) {
  return bwidthAbs - seqhh;
}

// Set of upper indexes
// [[Rcpp::export]]
arma::vec upIndexCpp(const int bwidthAbs, const arma::vec& bwidthRight) {
  return bwidthAbs + bwidthRight;
}

// Collection of right-hand side absolute bandwidths
// [[Rcpp::export]]
arma::vec bwidthRightCpp(const int bwidthAbs, const int bb, const arma::vec& allLowInd) {
  return bwidthAbs + bb * allLowInd;
}

// Collection of total absolute bandwidths
// [[Rcpp::export]]
arma::vec bwidthAllTotalCpp(const arma::vec& seqhh, const arma::vec& bwidthRight) {
  return seqhh + bwidthRight;
}

// Setup the data matrix for the polynomial time points and the harmonic function values
// [[Rcpp::export]]
arma::mat dataMatCpp(const int p, const int s, const int ntpoints, const int bwidthAbs, const arma::rowvec& all_tp) {
  
  const arma::rowvec all_tp_scaled = all_tp / bwidthAbs;  
  arma::mat dataMat = arma::ones<arma::mat>((p + s), ntpoints);
  
  dataMat.row(1) = all_tp_scaled;
  
  for (int i = 2; i < p + 1; ++i) {
    dataMat.row(i) = arma::pow(all_tp_scaled, i);
  }
  
  const double l1 = 2.0 * M_PI / s;
  
  for (int i = 1; i < std::trunc((s - 1.0) / 2) + 1; ++i) {
    dataMat.row(p + 2 * i - 1) = arma::cos(i * l1 * all_tp); //look here
    dataMat.row(p + 2 * i) = arma::sin(i * l1 * all_tp);
  }
  
  if ((s / 2.0) == std::trunc(s / 2.0)) {
    dataMat.row(p + s - 1) = arma::cos(M_PI * all_tp);
  }    
  
  return dataMat;
  
}

// Selector for the trend or its derivatives
// [[Rcpp::export]]
arma::rowvec derivSelectorCpp(const int p, const int s, const int deriv) {
  
  arma::rowvec ej = arma::zeros<arma::rowvec>(p + s);
  ej(deriv) = 1;
  
  return ej;
}

// Selector for the seasonality
// [[Rcpp::export]]
arma::rowvec seasonSelectorCpp(const int p, const int s) {
  
  arma::rowvec phi = arma::zeros<arma::rowvec>(p + s);
  for (int i = 1; i < s; i++) {
    if (i % 2 == 1) {
      phi(p + i) = 1;
    }
  }
  
  return phi;
}

// Adjustment of the weighting vector for mu = 0
// [[Rcpp::export]]
arma::rowvec wk_mu0_Cpp(const int l) {
  
  return arma::ones<arma::rowvec>(l);
}

// Adjustment of the weighting vector for mu = 1
// [[Rcpp::export]]
arma::rowvec wk_mu1_Cpp(const arma::rowvec& all_tp2_subset, const int bwidthRight2) {
  
  return 1.0 - all_tp2_subset / bwidthRight2;
  
}

// Adjustment of the weighting vector for mu > 1
// [[Rcpp::export]]
arma::rowvec wk_mu23_Cpp(const arma::rowvec& all_tp2_subset, const int bwidthRight2, const int mu) {
  
  return arma::pow(1.0 - all_tp2_subset / bwidthRight2, mu);

}

// Adjustment of the weighting vector generally
// [[Rcpp::export]]
arma::rowvec wkAdjCpp(const arma::rowvec& all_tp2_subset, const int bwidthRight2, const int mu) {
  
  const int l = all_tp2_subset.size();
  
  arma::rowvec wkAdj;
  
  if (mu == 0) {
    wkAdj = wk_mu0_Cpp(l);
  } else if (mu == 1) {
  
    wkAdj = wk_mu1_Cpp(all_tp2_subset, bwidthRight2);

  } else {
    wkAdj = wk_mu23_Cpp(all_tp2_subset, bwidthRight2, mu);
  }
  
  return wkAdj;
}

// Run regression for deseats
// [[Rcpp::export]]
arma::mat deseatsRegCpp(const arma::rowvec& wk, const arma::mat& xt) {
  
  arma::mat xw = xt * diagmat(wk);
  arma::mat xa = arma::solve(xw * xt.t(), xw);  
  
  return xa;
}

// Complete a weighting systems second half
// [[Rcpp::export]]
arma::mat completeWeightingCpp(const arma::mat& ws, const int bwidthAbs, const int bwidthTotal) {
  
  return arma::flipud(arma::fliplr(ws.submat(0, 0, bwidthAbs - 1, bwidthTotal - 1))); 

}

// Obtain left-boundary estimates
// [[Rcpp::export]]
arma::vec fitLeftCpp(const arma::vec& y, const arma::mat& ws, const int bwidthAbs, const int bwidthTotal) {
  
  arma::vec gr = ws.rows(0, bwidthAbs - 1) * y.subvec(0, bwidthTotal - 1);

  return gr;
}

// Obtain middle point estimates
// [[Rcpp::export]]
arma::vec fitMiddleCpp(const arma::vec& y, const arma::mat& ws, const int bwidthAbs, const int bwidthTotal, const int n) {
  
  arma::mat ym(bwidthTotal, n - bwidthTotal + 1);
  for (int i = bwidthAbs; i < n - bwidthAbs; ++i) {
    ym.col(i - bwidthAbs) = y.subvec(i - bwidthAbs, i + bwidthAbs);
  }
  
  arma::vec gr = (ws.row(bwidthAbs) * ym).t();

  return gr;
}

// Obtain right-boundary estimates
// [[Rcpp::export]]
arma::vec fitRightCpp(const arma::vec& y, const arma::mat& ws, const int bwidthAbs, const int bwidthTotal, const int n) {
  
  arma::vec gr = ws.rows(bwidthAbs + 1, bwidthTotal - 1) * y.subvec(n - bwidthTotal, n - 1);

  return gr;
}

// Obtain estimates based on weighting system
// [[Rcpp::export]]
arma::vec fitOverallCpp(const arma::vec& y, const arma::mat& ws, const int bwidthAbs, const int bwidthTotal, const int n) {
  
  arma::vec gr(n);
  
  gr.subvec(0, bwidthAbs - 1) = fitLeftCpp(y, ws, bwidthAbs, bwidthTotal);
  gr.subvec(bwidthAbs, n - bwidthAbs - 1) = fitMiddleCpp(y, ws, bwidthAbs, bwidthTotal, n);
  gr.subvec(n - bwidthAbs, n - 1) = fitRightCpp(y, ws, bwidthAbs, bwidthTotal, n);
  
  return gr;
}

// C++ version of deseats smoothing
// [[Rcpp::export]]
Rcpp::List fitteddeseatsCpp(const arma::vec& y, const int p,
  const int s, const int mu, const double b, const int bb) {

  const int n = y.size();
  const int bwidthAbs = bwidthAbsCpp(n, b);
  const int bwidthTotal = bwidthTotalCpp(bwidthAbs);

  arma::mat wsG = setup_wsMat_single_Cpp(bwidthTotal);
  arma::mat wsS = setup_wsMat_single_Cpp(bwidthTotal);
  arma::mat wsK = setup_wsMat_single_Cpp(bwidthTotal);

  arma::rowvec wk = setup_wk_Cpp(bwidthTotal);
  
  arma::mat xt = setup_xt_Cpp(p, s, bwidthTotal);
  arma::mat xa;

  const arma::vec seqhh = seqCpp(0, bwidthAbs);
  const arma::vec allLowInd = lowIndexCpp(bwidthAbs, seqhh);
  const arma::vec bwidthRight = bwidthRightCpp(bwidthAbs, bb, allLowInd);
  const arma::vec bwidthRight2 = arma::pow(bwidthRight + 1, 2.0);
  const arma::vec allbwidthTotal = bwidthAllTotalCpp(seqhh, bwidthRight);
  const arma::vec allUpInd = upIndexCpp(bwidthAbs, bwidthRight);

  const arma::rowvec all_tp = rseqCpp(-bwidthAbs, bwidthRight(0));
  const int ntpoints = all_tp.size();
  const arma::rowvec all_tp2 = arma::pow(all_tp, 2.0);

  arma::mat dataMat = dataMatCpp(p, s, ntpoints, bwidthAbs, all_tp);  

  arma::rowvec ej = derivSelectorCpp(p, s, 0);
  arma::rowvec phi = seasonSelectorCpp(p, s);

  int lowB;
  int upB;
  
  for (int i = 0; i < bwidthAbs + 1; ++i) {

    lowB = allLowInd(i);
    upB = allUpInd(i);
  
    wk.subvec(0, allbwidthTotal(i)) = wkAdjCpp(all_tp2.subvec(lowB, upB), bwidthRight2(i), mu);
    wsK.row(i) = wk;

    xt.submat(0, 0, p + s - 1, allbwidthTotal(i)) = dataMat.cols(lowB, upB);
    xa = deseatsRegCpp(wk, xt);

    wsG.row(i) = ej * xa;
    wsS.row(i) = phi * xa;
    
  }

  wsG.rows(bwidthAbs + 1, bwidthTotal - 1) = completeWeightingCpp(wsG, bwidthAbs, bwidthTotal);
  wsS.rows(bwidthAbs + 1, bwidthTotal - 1) = completeWeightingCpp(wsS, bwidthAbs, bwidthTotal);
  wsK.rows(bwidthAbs + 1, bwidthTotal - 1) = completeWeightingCpp(wsK, bwidthAbs, bwidthTotal);

  arma::mat ws = wsG + wsS;
  
  arma::vec grG = fitOverallCpp(y, wsG, bwidthAbs, bwidthTotal, n);
  arma::vec grS = fitOverallCpp(y, wsS, bwidthAbs, bwidthTotal, n);  
  
  arma::vec gr = grG + grS;
  arma::vec resid = y - gr;
  
  Rcpp::List listOut = List::create(_["Compl"] = gr,
                              _["Trend"] = grG,
                              _["Season"] = grS,
                              _["Residuals"] = resid,
                              _["wsG"] = wsG,
                              _["wsS"] = wsS,
                              _["ws"] = ws,
                              _["wsK"] = wsK);
  
  return listOut;
  
}

// C++ version of deseats smoothing
// [[Rcpp::export]]
arma::vec residdeseatsCpp(const arma::vec& y, const int p,
  const int s, const int mu, const double b, const int bb) {

  const int n = y.size();
  arma::vec gr(n);
  const int bwidthAbs = std::trunc(n * b + 0.5);
  const int bwidthMax = 2 * bwidthAbs + 1;
  
  arma::mat ws(bwidthMax, bwidthMax);

  arma::rowvec wk = arma::zeros<arma::rowvec>(bwidthMax);
  arma::mat xt = arma::zeros<arma::mat>((p + s), bwidthMax);
  
  arma::mat xw = xt;

  const arma::vec seqhh = seqCpp(0, bwidthAbs);
  const arma::vec allLowB = bwidthAbs - seqhh;
  const arma::vec hr = bwidthAbs + bb * allLowB;
  const arma::vec hr2 = arma::pow(hr + 1, 2.0);
  const arma::vec ht = seqhh + hr;
  arma::mat xa = xt;
  
  const arma::rowvec allsequ = rseqCpp(-bwidthAbs, hr(0));
  const int m = allsequ.size();
  const arma::rowvec allsequ2 = arma::pow(allsequ, 2.0);
  const arma::rowvec allsequhh = allsequ / bwidthAbs;
  
  arma::mat dataMat = arma::ones<arma::mat>((p + s), m);  
  dataMat.row(1) = allsequhh;
  
  for (int i = 2; i < p + 1; ++i) {
    dataMat.row(i) = arma::pow(allsequhh, i);
  }
  
  const double l1 = 2.0 * M_PI / s;  
  
  for (int i = 1; i < std::trunc((s - 1.0) / 2) + 1; ++i) {
    dataMat.row(p + 2 * i - 1) = arma::cos(i * l1 * allsequ); //look here
    dataMat.row(p + 2 * i) = arma::sin(i * l1 * allsequ);
  }
  
  if ((s / 2.0) == std::trunc(s / 2.0)) {
    dataMat.row(p + s - 1) = arma::cos(M_PI * allsequ);
  }  
  
  arma::rowvec ej = arma::zeros<arma::rowvec>(p + s);
  arma::rowvec phi = ej;  
  arma::rowvec prevec = ej;   
  
  ej(0) = 1;
  
  for (int i = 1; i < s; i++) {
    if (i % 2 == 1) {
      phi(p + i) = 1;
    }
  }
  
  prevec.subvec(0, p) = ej.subvec(0, p); 

  prevec.subvec(p + 1, p + s - 1) = phi.subvec(p + 1, p + s - 1);  
  
  const arma::vec allUpB = bwidthAbs + hr;

  int lowB;
  int upB;
  
  for (int i = 0; i < bwidthAbs + 1; ++i) {

    lowB = allLowB(i);
    upB = allUpB(i);
    
    if (mu == 0) {
      wk.subvec(0, ht(i)).fill(1.0);
    } else if (mu == 1) {
      wk.subvec(0, ht(i)) = 1.0 - allsequ2.subvec(lowB, upB) / hr2(i);
    } else {
      wk.subvec(0, ht(i)) = arma::pow(1.0 - allsequ2.subvec(lowB, upB) / hr2(i), mu);
    }
    
    xt.submat(0, 0, p + s - 1, ht(i)) = dataMat.cols(lowB, upB);
    xw = xt * diagmat(wk);
    xa = arma::solve(xw * xt.t(), xw);

    ws.row(i) = prevec * xa;
    
  }
  
  ws.rows(bwidthAbs + 1, bwidthMax - 1) = arma::flipud(arma::fliplr(ws.submat(0, 0, bwidthAbs - 1, bwidthMax - 1)));

  arma::mat ym(bwidthMax, n - bwidthMax + 1);
  for (int i = bwidthAbs; i < n - bwidthAbs; ++i) {
    ym.col(i - bwidthAbs) = y.subvec(i - bwidthAbs, i + bwidthAbs);
  }
  gr.subvec(0, bwidthAbs - 1) = ws.rows(0, bwidthAbs - 1) * y.subvec(0, bwidthMax - 1);
  gr.subvec(bwidthAbs, n - bwidthAbs - 1) = (ws.row(bwidthAbs) * ym).t();
  gr.subvec(n - bwidthAbs, n - 1) = ws.rows(bwidthAbs + 1, bwidthMax - 1) * y.subvec(n - bwidthMax, n - 1);
  
  arma::vec resid = y - gr;
  
  return resid;
  
}

// C++ version of deseats derivative smoothing
// [[Rcpp::export]]
arma::vec derivdeseatsCpp(const arma::vec& y, const int p,
  const int s, const int mu, const double b, const int bb, const int v) {

  const int n = y.size();
  arma::vec grG(n);
  const int bwidthAbs = std::trunc(n * b + 0.5);
  const int bwidthMax = 2 * bwidthAbs + 1;
  
  arma::mat wsG(bwidthMax, bwidthMax);
  arma::rowvec wk = arma::zeros<arma::rowvec>(bwidthMax);
  arma::mat xt = arma::zeros<arma::mat>((p + s), bwidthMax);
  
  arma::mat xw = xt;

  const arma::vec seqhh = seqCpp(0, bwidthAbs);
  const arma::vec allLowB = bwidthAbs - seqhh;
  const arma::vec hr = bwidthAbs + bb * allLowB;
  const arma::vec hr2 = arma::pow(hr + 1, 2.0);
  const arma::vec ht = seqhh + hr;
  arma::mat xa = xt;
  
  const arma::rowvec allsequ = rseqCpp(-bwidthAbs, hr(0));
  const int m = allsequ.size();
  const arma::rowvec allsequ2 = arma::pow(allsequ, 2.0);
  const arma::rowvec allsequhh = allsequ / bwidthAbs;
  
  arma::mat dataMat = arma::ones<arma::mat>((p + s), m);  
  dataMat.row(1) = allsequhh;
  
  for (int i = 2; i < p + 1; ++i) {
    dataMat.row(i) = arma::pow(allsequhh, i);
  }
  
  const double l1 = 2.0 * M_PI / s;  
  
  for (int i = 1; i < std::trunc((s - 1.0) / 2) + 1; ++i) {
    dataMat.row(p + 2 * i - 1) = arma::cos(i * l1 * allsequ); //look here
    dataMat.row(p + 2 * i) = arma::sin(i * l1 * allsequ);
  }
  
  if ((s / 2.0) == std::trunc(s / 2.0)) {
    dataMat.row(p + s - 1) = arma::cos(M_PI * allsequ);
  }  
  
  arma::rowvec ej = arma::zeros<arma::rowvec>(p + s);
  
  ej(v) = 1;
  
  const arma::vec allUpB = bwidthAbs + hr;

  int lowB;
  int upB;
  
  for (int i = 0; i < bwidthAbs + 1; ++i) {

    lowB = allLowB(i);
    upB = allUpB(i);
    
    if (mu == 0) {
      wk.subvec(0, ht(i)).fill(1.0);
    } else if (mu == 1) {
      wk.subvec(0, ht(i)) = 1.0 - allsequ2.subvec(lowB, upB) / hr2(i);
    } else {
      wk.subvec(0, ht(i)) = arma::pow(1.0 - allsequ2.subvec(lowB, upB) / hr2(i), mu);
    }
    
    xt.submat(0, 0, p + s - 1, ht(i)) = dataMat.cols(lowB, upB);
    xw = xt * diagmat(wk);
    xa = arma::solve(xw * xt.t(), xw);

    wsG.row(i) = ej * xa;
    
  }
  
  wsG.rows(bwidthAbs + 1, bwidthMax - 1) = arma::flipud(arma::fliplr(wsG.submat(0, 0, bwidthAbs - 1, bwidthMax - 1))) * std::pow(-1.0, v);
  wsG = factorialCpp(v) * wsG * std::pow((n / double(bwidthAbs)), v);
  
  arma::mat ym(bwidthMax, n - bwidthMax + 1);
  for (int i = bwidthAbs; i < n - bwidthAbs; ++i) {
    ym.col(i - bwidthAbs) = y.subvec(i - bwidthAbs, i + bwidthAbs);
  }
  grG.subvec(0, bwidthAbs - 1) = wsG.rows(0, bwidthAbs - 1) * y.subvec(0, bwidthMax - 1);
  grG.subvec(bwidthAbs, n - bwidthAbs - 1) = (wsG.row(bwidthAbs) * ym).t();
  grG.subvec(n - bwidthAbs, n - 1) = wsG.rows(bwidthAbs + 1, bwidthMax - 1) * y.subvec(n - bwidthMax, n - 1);
  
  return grG;
  
}

// [[Rcpp::export]]
Rcpp::List algorithmCpp(const arma::vec& yt, const int p, const int s, const int mu, const double bStart, const int CF, const int errors, const double cb, const int bb, const int errm, const double expo) {
  
  double bopt = bStart;
  double boptOld = -100;
  double boptOld2 = -100;
  const int n = yt.size();
  const int k = p + 1;
  
  double bmax = 0.45;       // The maximum bandwidth 
  double bmin = 0.01;       // The minimum bandwidth 

  const int v = k;
  const int pv = k + 1;

  const int n1 = std::trunc(n * cb);  
  
  const int m = 1000000;
  const arma::vec u = seqCpp(-m, m) / (m + 0.5);

  arma::vec wk = arma::pow((1 - arma::pow(u, 2.0)), mu);
  wk = wk / arma::accu(wk) * m;
  const double R2 = arma::accu(arma::pow(wk, 2.0)) / m;  
  
  arma::vec wkp;
  
  if (p == 1) {
    wkp = wk;
  } else if (p == 3) {
    if (mu == 0) {
      wkp = 3.0 / 8.0 * (3.0 - 5.0 * arma::pow(u, 2.0));
    } else if (mu == 1) {
      wkp = 15.0 / 32.0 * (3.0 - 10.0 * arma::pow(u, 2.0) + 7.0 * arma::pow(u, 4.0));      
    } else if (mu == 2) {
      wkp = 105.0 / 64.0 * (1.0 - 5.0 * arma::pow(u, 2.0) + 7.0 * arma::pow(u, 4.0) - 3.0 * arma::pow(u, 6.0));      
    } else if (mu == 3) {
      wkp = 315.0 / 512.0 * (3.0 - 20.0 * arma::pow(u, 2.0) + 42.0 * arma::pow(u, 4.0) - 36.0 * arma::pow(u, 6.0) + 11.0 * arma::pow(u, 8.0));      
    }
  }
  
  wkp = wkp / arma::accu(wkp) * m;
  
  const double Rp = arma::accu(arma::pow(wkp, 2.0)) / m;
  const double mukp = arma::accu(arma::pow(u, k) % wkp) / m;  
  
  const double c1 = std::pow(factorialCpp(k), 2.0) / (2 * k);
  const double c2 = (1.0 - 2.0 * cb) * (Rp + (s - 1.0) * R2) / std::pow(mukp, 2.0);  
  
  const int itMax = 20;
  
  int it = 1;
  double bv;
  
  arma::vec gv;
  double I2;
  
  double adjFactor = 1.0;

  if (CF == 1) {  
  
    if (s == 4) {

      if (p == 1) {
        if (mu == 0) {
          adjFactor = 1.3195;
        } else if (mu == 1) {
          adjFactor = 1.4310;
        } else if (mu == 2) {
          adjFactor = 1.4541;
        } else if (mu == 3) {
          adjFactor = 1.4640;
        }
      } else if (p == 3) {
        if (mu == 0) {
          adjFactor = 1.2599;
        } else if (mu == 1) {
          adjFactor = 1.3077;
        } else if (mu == 2) {
          adjFactor = 1.3188;
        } else if (mu == 3) {
          adjFactor = 1.3239;
        }
      }

    } else if (s == 12) {

      if (p == 1) {
        if (mu == 0) {
          adjFactor = 1.3195;
        } else if (mu == 1) {
          adjFactor = 1.4310;
        } else if (mu == 2) {
          adjFactor = 1.4541;
        } else if (mu == 3) {
          adjFactor = 1.4640;
        }
      } else if (p == 3) {
        if (mu == 0) {
          adjFactor = 1.2599;
        } else if (mu == 1) {
          adjFactor = 1.3140;
        } else if (mu == 2) {
          adjFactor = 1.3257;
        } else if (mu == 3) {
          adjFactor = 1.3307;
        }
      }
    }
  }  
  
  double bAdj;
  arma::vec resAdj;
  double cf0 = 0;
  double c3;
  arma::vec resAdj2;
  int stopreason = 0;
  const double diffvalue = 1.0 / n;
  arma::vec bwidths = arma::zeros<arma::vec>(itMax + 1);
  bwidths(0) = bopt;

  while (it < itMax + 1 && stopreason == 0) {
    
    bv = std::pow(bopt, expo);
    bv = std::max(bv, bmin);
    bv = std::min(bv, bmax);
    
    gv = derivdeseatsCpp(yt, pv, s, mu, bv, bb, v);
  
    I2 = arma::accu(arma::pow(gv.subvec(n1, n - n1 - 1), 2.0)) / (n - 2 * n1);
    
    bAdj = bopt * adjFactor;
    
    resAdj = residdeseatsCpp(yt, p, s, mu, bAdj, bb);
      
    if (errors == 1) {
      
      if (errm == 1) {

        cf0 = cf0Cpp(resAdj);

      } else if (errm == 0) {
        
        Rcpp::List arma = armaoptCpp(resAdj);
        cf0 = arma_sumacovCpp(arma);
        
      }
      
    } else if (errors == 0) {
      
      resAdj2 = arma::pow(resAdj, 2.0);
      cf0 = arma::mean(resAdj2);
      
    }
    
    c3 = cf0 / I2;
    
    boptOld2 = boptOld;
    boptOld = bopt;
    bopt = std::pow((c1 * c2 * c3), (1.0 / (2 * k + 1))) * std::pow(n, (-1.0 / (2 * k + 1)));
   
    bopt = std::max(bopt, bmin);
    bopt = std::min(bopt, bmax);   
   
    if (std::abs(bopt - boptOld) / bopt < diffvalue) {
      stopreason = 1;
    }
    
    if (std::abs(bopt - boptOld2) / bopt < diffvalue) {
      stopreason = 1;
      bopt = (bopt + boptOld) / 2.0;
    }  
    
    bwidths(it) = bopt;
    
    it += 1;
    
  }
  
  arma::vec bout = bwidths.subvec(0, it - 1);
  
  Rcpp::List out = Rcpp::List::create(_["bwidths"] = bout, _["Imk"] = I2, _["sum_autocov"] = cf0);
  
  return(out);

}
