# deseats 1.1.3
- Fixed a bug in the `predict` method for `s_semiarma()` output that
  was experienced on machines with one machine core; it is now ensured
  that the number of cores used in the bootstrap procedure uses at least
  one core and cannot drop to zero (thanks to Henrik Begtsson for pointing
  this out).
- Added two arguments `ncores` and `progress_bar` in the `predict` method for
  `s_semiarma()` output to control the number of cores used in the bootstrap
  and whether or not to show a progress bar during the bootstrap procedure.
- Change in maintainer email address from dominik.schulz@uni-paderborn.de
  to dominik.schulz.r@gmail.com; the maintainer is, however, still the same
  person.
- Further updates to the general documentation of the package, so that it is
  easier to find the package's GitHub repository, to cite the package, etc.

# deseats 1.1.2
- `create.gain()` and `runDecomposition()` are now deprecated and replaced
  by `create_gain()` and `run_decomposition()`.
- `select_bwidth()`, `deseats()` and `s_semiarma()` now have an argument 
  `inf_criterion` that lets the user switch between `"bic"` (the default) and 
  `"aic"`; this selects the information criterion (BIC or AIC) to use for 
  the ARMA order selection wherever relevant within those functions.
- `select_bwidth()`, `deseats()` and `s_semiarma()` now warn the user, when
  an extremely small number of observations is being used, which makes
  the bandwidth selection highly unreliable.
- some internal functions have been made more robust, so that they do not
  throw an error under small numbers of observations; instead they warn
  the user (see previous point) and fall back to certain minimum / maximum
  settings that still work (but are unreliable).
- some defaults in `set_option()` have been adjusted, namely `order_poly = 3`
  instead of `order_poly = 1` as well as `boundary_method = "shorten"` instead
  of `boundary_method = "extend"`.
- a vignette was added to the package that introduces readers to the basic
  applications of the package with an example.
- the `animate()` method for output of `deseats()` now also shows proper
  results for the case with `boundary_method = "shorten"` in `set_options()`.
- the `predict()` method for S-Semi-ARMA models now allows for bias adjustment
  of point forecasts via `bias.adjust = TRUE`, if `expo = TRUE`.

# deseats 1.1.1
- fixed a bug in the forecasting of seasonal semiparametric ARMA models, where for the
  special case of an ARMA for the rest including a mean estimate the forecasting intervals
  under the normality assumption where too large by exactly that mean estimate
- fixed an issue, probably due to some updates in `ggplot2`, where in `autoplot`for
  forecasting objects the legend for multiple forecasting intervals was not displayed
  correctly anymore

# deseats 1.1.0
- the argument `correction_factor` in the functions `deseats`, `s_semiarma` and 
  `select_bwidth` is now set to `FALSE` by default.
- further arguments were added to `deseats`, `s_semiarma` and `select_bwidth` to
  allow for a fine-adjustments of the algorithm, when an ARMA model is assumed
  as the error process.
- the functions `seasonplot` and `seasonplot_gg` were added to allow for the
  creation of seasonal plots.
- the faulty link in the manual to the `RcppArmadillo` package was removed.
- the example in the documentation of `ts_conversion` was adjusted to use `file.path` for the path to the temporary data file.