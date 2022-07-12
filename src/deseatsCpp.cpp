#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// Functions written by Dominik Schulz, 12/05/2020

// Function to help obtain a sequence from 'from' to 'to' by step 1
// in C++
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
arma::rowvec rseqCpp(const int from, const int to) {
  const int n = to - from + 1;
  arma::rowvec seqOut(n);
  for (int i = from; i < to + 1; ++i) {
    seqOut(i - from) = i;
  }
  return seqOut;
}

// Function to calculate the factorial of an 'int' in C++
int factorialCpp(const int k) {
  int fac = 1;
  if (k > 1) {
    for (int i = 2; i < k + 1; ++i) {
      fac *= i;
    }
  }
  return fac;
}

// C++ version of deseats smoothing
// [[Rcpp::export]]
arma::mat gsmoothdeseatsCpp(const arma::vec& y, const int p,
  const int s, const int mu, const double b, const int bb) {

  const int n = y.size();
  arma::vec grG(n);
  arma::vec grS(n);
  const int bwidthAbs = std::trunc(n * b + 0.5);
  const int bwidthMax = 2 * bwidthAbs + 1;
  
  arma::mat ws(bwidthMax, bwidthMax);
  arma::mat wsG = ws;
  arma::mat wsS = ws;
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
  
  for (int i = 1; i < std::trunc((s - 1.0) / 2) + 1; ++i) {
    dataMat.row(p + 2 * i - 1) = arma::cos(2.0 * i * M_PI * allsequ / s);
    dataMat.row(p + 2 * i) = arma::sin(2.0 * i * M_PI * allsequ / s);
  }
  
  if ((s / 2.0) == std::trunc(s / 2.0)) {
    dataMat.row(s - 2) = arma::cos(M_PI * allsequ);
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
  
  prevec.subvec(p + 1, p + s - 1) = ej.subvec(p + 1, p + s - 1);  
  
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
    wsG.row(i) = ej * xa;
    wsS.row(i) = phi * xa;
    
  }
  
  ws.rows(bwidthAbs + 1, bwidthMax - 1) = arma::flipud(arma::fliplr(ws.submat(0, 0, bwidthAbs - 1, bwidthMax - 1)));
  wsG.rows(bwidthAbs + 1, bwidthMax - 1) = arma::flipud(arma::fliplr(wsG.submat(0, 0, bwidthAbs - 1, bwidthMax - 1)));
  wsS.rows(bwidthAbs + 1, bwidthMax - 1) = arma::flipud(arma::fliplr(wsS.submat(0, 0, bwidthAbs - 1, bwidthMax - 1)));  
  
  arma::mat ym(bwidthMax, n - bwidthMax + 1);
  for (int i = bwidthAbs; i < n - bwidthAbs; ++i) {
    ym.col(i - bwidthAbs) = y.subvec(i - bwidthAbs, i + bwidthAbs);
  }
  grG.subvec(0, bwidthAbs - 1) = wsG.rows(0, bwidthAbs - 1) * y.subvec(0, bwidthMax - 1);
  grG.subvec(bwidthAbs, n - bwidthAbs - 1) = (wsG.row(bwidthAbs) * ym).t();
  grG.subvec(n - bwidthAbs, n - 1) = wsG.rows(bwidthAbs + 1, bwidthMax - 1) * y.subvec(n - bwidthMax, n - 1);
  
  grS.subvec(0, bwidthAbs - 1) = wsS.rows(0, bwidthAbs - 1) * y.subvec(0, bwidthMax - 1);
  grS.subvec(bwidthAbs, n - bwidthAbs - 1) = (wsS.row(bwidthAbs) * ym).t();
  grS.subvec(n - bwidthAbs, n - 1) = wsS.rows(bwidthAbs + 1, bwidthMax - 1) * y.subvec(n - bwidthMax, n - 1);  
  
  arma::vec gr = grG + grS; 
  
  Rcpp::List listOut = List::create(_["Compl"] = gr,
                              _["Trend"] = grG,
                              _["Season"] = grS);
  
  return dataMat;
  
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R

*/
