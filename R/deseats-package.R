#'Deseasonalize Time Series
#' 
#'A library of decomposition methods for equidistant time series with 
#'trend and seasonality.
#'
#'@details
#'\code{deseats} is an R package for the decomposition of equidistant time 
#'series with trend and seasonality. First and foremost, an own algorithm 
#'for bandwidth selection in locally weighted regression of such time series 
#'(with short-range dependence) is implemented. For comparison,
#'a simplified version of the BV4.1 (Berlin Procedure 4.1, Speth, 2004), is 
#'implemented as well that allows to implement the BV4.1 base model (trend 
#'component + seasonality component + irregular component) without any 
#'of the additional BV4.1 components (such as the calendar component).
#'
#'@section Main Functions:
#'The main functions of the package are:
#'\describe{
#'\item{\code{\link{deseats}}:}{locally weighted regression with automatically 
#'selected bandwidth for decomposition,}
#'\item{\code{\link{BV4.1}}:}{BV4.1 base model for decomposition,}
#'\item{\code{\link{lm_decomp}}:}{ordinary least squares for decomposition,}
#'\item{\code{\link{llin_decomp}}:}{local linear regression for decomposition,}
#'\item{\code{\link{ma_decomp}}:}{moving averages for decomposition,}
#'\item{\code{\link{hamilton_filter}}:}{the time series filter by Hamilton.}
#'}
#'
#'@section Datasets:
#'The package includes a few datasets. Follow the corresponding links to the 
#'documentation of the datasets to find additional information including the 
#'sources.
#'\describe{
#'\item{\code{\link{CIVLABOR}}:}{civilian labor force level in the USA.}
#'\item{\code{\link{CONSUMPTION}}:}{real final consumption expenditure for Australia.}
#'\item{\code{\link{COVID}}:}{new COVID-19 cases in Germany.}
#'\item{\code{\link{DEATHS}}:}{recorded number of deaths in Germany.}
#'\item{\code{\link{EXPENDITURES}}:}{consumption expenditures in the USA.}
#'\item{\code{\link{GDP}}:}{GDP of the USA.}
#'\item{\code{\link{LIVEBIRTHS}}:}{recorded number of livebirths in Germany.}
#'\item{\code{\link{NOLABORFORCE}}:}{number of persons in the USA not belonging 
#'to the labor force.}
#'\item{\code{\link{RAINFALL}}:}{average amount of rain in Germany.}
#'\item{\code{\link{SAVINGS}}:}{savings of private households in Germany.}
#'\item{\code{\link{SUNSHINE}}:}{average hours of sunshine in Germany.}
#'\item{\code{\link{TEMPERATURE}}:}{average temperature in Germany.}
#'}
#'
#'@section License:
#'The package is distributed under the General Public License v3
#'([GPL-3](https://tldrlegal.com/license/gnu-general-public-license-v3-(gpl-3))).
#'
#'@references
#'Speth, H.-T. (2004). Komponentenzerlegung und Saisonbereinigung ökonomischer 
#'Zeitreihen mit dem Verfahren BV4.1. Methodenberichte 3. Statistisches 
#'Bundesamt.
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'\item Yuanhua Feng (Department of Economics, Paderborn
#'University), \cr
#'Author
#'}
#'
#'@importFrom stats as.formula frequency lm na.omit qnorm quantile
#'@importFrom stats rgeom time ts
#'@importFrom utils head tail
#'
#'@useDynLib deseats
#'@docType package
#'@name deseats-package
#'@aliases deseats-package
#'@useDynLib deseats
#'@importFrom Rcpp sourceCpp
NULL
