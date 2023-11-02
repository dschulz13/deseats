## Test environments

- local Windows 10 install, R 4.3.1
- win-builder (devel, release and oldrelease)
- macOS-builder (release)
- R-hub-builder (Ubuntu Linux 20.04.1 LTS [R-release, GCC], Fedora Linux [R-devel, clang, gfortran], Debian Linux [R-devel, GCC ASAN/UBSAN])

## R CMD check results

0 errors | 0 warnings | 0 notes

## win-builder check results

0 errors | 0 warnings | 1 note

### Notes

* checking CRAN incoming feasibility ... [16s] NOTE
  Maintainer: 'Dominik Schulz <dominik.schulz@uni-paderborn.de>'

  New submission
  
This is the first time this brand-new package is submitted to CRAN.

### Others

* Possibly misspelled words in DESCRIPTION:
  Feng (23:3, 23:54)
  Gries (23:64)
  Speth (27:50)
  
These are spelled correctly.

* Found the following (possibly) invalid URLs:
  URL: https://cran.r-project.org/web/checks/check_results_deseats.html
    From: README.md
    Status: 404
    Message: Not Found
    
The README makes use of the ```badger``` package to display badges such as results from CRAN. Since this package is not yet published on CRAN, the URL does indeed not yet exist, however, it is expected that the URL becomes valid alongside the publication on CRAN.

## macOS-builder check results

0 errors | 0 warnings | 1 note

### Notes

* checking installed package size ... NOTE
  installed size is  6.3Mb
  sub-directories of 1Mb or more:
    libs   5.5Mb
    
The package makes heavy use of C++ via the ```Rcpp``` and ```RcppArmadillo``` packages in order to improve the performance of the implemented algorithms. As a consequence, the size of the package is a bit larger.

## R-hub-builder check results

### Ubuntu Linux 20.04.1 LTS [R-release, GCC]

#### PREPERROR

* ERROR: dependency ‘magick’ is not available for package ‘animation’
* ERROR: dependency ‘magick’ is not available for package ‘animation’
* ERROR: dependencies ‘credentials’, ‘openssl’ are not available for package ‘gert’
* ...

These are caused by the R-hub-builder itself.

### Fedora Linux [R-devel, clang, gfortran]

0 errors | 0 warnings | 1 note

#### Notes

* checking CRAN incoming feasibility ... [7s/36s] NOTE
  Maintainer: ‘Dominik Schulz <dominik.schulz@uni-paderborn.de>’

  New submission
  
Same as stated for the win-builder.

#### Others

* Possibly misspelled words in DESCRIPTION:
    Feng (23:3, 23:54)
    Gries (23:64)
    Speth (27:50)
    
Same as for the win-builder.

### Debian Linux [R-devel, GCC ASAN/UBSAN]

#### PREPERROR

* ERROR: dependency ‘magick’ is not available for package ‘animation’
* ERROR: dependency ‘magick’ is not available for package ‘animation’
* ERROR: dependencies ‘credentials’, ‘openssl’ are not available for package ‘gert’
* ...

These are caused by the R-hub-builder itself.
