# Test results for current package version 1.1.3

## Test environments
* local Windows install, R 4.6.1
* win-builder (oldrelease, release and devel)
* R-hub (linux, windows, ubuntu-next, ubuntu-release, m1-san, macos-arm64, macos)

## R CMD check results

0 errors | 0 warnings | 1 notes

* checking CRAN incoming feasibility ... [132s] NOTE
  Maintainer: 'Dominik Schulz <dominik.schulz.r@gmail.com>'

  New maintainer:
    Dominik Schulz <dominik.schulz.r@gmail.com>
  Old maintainer(s):
    Dominik Schulz <dominik.schulz@uni-paderborn.de>


This is a change in the maintainer's email address, as the old email address
is a corporate address that may cease to exist within the next twelve months.

# Test results for older package versions

## Test results for package version 1.1.2

## Test environments
* local Windows install, R 4.4.2
* win-builder (oldrelease, release and devel)
* macOS builder
* R-hub (linux, m1-san, macos, macos-arm64, windows, atlas, c23, clang-asan, clang-ubsan, clang16, clang17, clang18, clang19, donttest, gcc-asan, gcc13, gcc14, gcc15, mkl, nold, noremap, nosuggests, ubuntu-clang, ubuntu-gcc12, ubuntu-next, ubuntu-release)

## R CMD check results

0 errors | 0 warnings | 1 notes

* checking installed package size ... NOTE
  installed size is  6.0Mb
  sub-directories of 1Mb or more:
    libs   5.2Mb

This appears on macOS builder only; the package makes heavy use of C++ via the ```Rcpp``` and ```RcppArmadillo``` packages in order to improve the performance of the implemented algorithms. As a consequence, the size of the package is a bit larger. This was not a problem and fully accepted by CRAN when package
version 1.0.0 was submitted to CRAN.

## Test results for package version 1.1.0

## R CMD check results

0 errors | 0 warnings | 0 notes

## win-builder check results [oldrelease, release, devel]

0 errors | 0 warnings | 1 note

* Found the following (possibly) invalid URLs:
    URL: https://www.destatis.de/DE/Methoden/Saisonbereinigung/BV41-methodenbericht-Heft3_2004.pdf?__blob=publicationFile
      From: DESCRIPTION
      Status: 400
      Message: Bad Request

The URL is however valid.

## macOS-builder check results

0 errors | 0 warnings | 1 note

### Notes

* checking installed package size ... NOTE
  installed size is  5.7Mb
  sub-directories of 1Mb or more:
    libs   4.9Mb
    
The package makes heavy use of C++ via the ```Rcpp``` and ```RcppArmadillo``` packages in order to improve the performance of the implemented algorithms. As a consequence, the size of the package is a bit larger. This was not a problem and fully accepted by CRAN when package
version 1.0.0 was submitted to CRAN.

## Rhub check results

### ubuntu-gcc12 [Ubuntu 22.04.4 LTS, R-devel]

0 errors | 0 warnings | 0 notes

### gcc13 [Fedora Linux 38 (Container Image), R-devel]

0 errors | 0 warnings | 0 notes

### linux [ubuntu-latest on GitHub, R-* (any version)]

0 errors | 0 warnings | 1 note

#### Notes

* checking installed package size ... NOTE
  installed size is  5.7Mb
  sub-directories of 1Mb or more:
    libs   4.9Mb
    
The package makes heavy use of C++ via the ```Rcpp``` and ```RcppArmadillo``` packages in order to improve the performance of the implemented algorithms. As a consequence, the size of the package is a bit larger. This was not a problem and fully accepted by CRAN when package
version 1.0.0 was submitted to CRAN.

### macos-arm64 [macos-latest on GitHub, R-* (any version)]

0 errors | 0 warnings | 0 notes

### macos [macos-13 on GitHub, R-* (any version)]

0 errors | 0 warnings | 0 notes

### windows [windows-latest on GitHub, R-* (any version)]

0 errors | 0 warnings | 0 notes

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

* Found the following (possibly) invalid DOIs:
    DOI: 10.1080/02664763.2012.740626
      From: DESCRIPTION
      Status: Forbidden
      Message: 403
    DOI: 10.1080/10485252.2020.1759598
      From: DESCRIPTION
      Status: Forbidden
      Message: 403

These DOIs are valid. This appears also only on oldrelease-win-builder.

* Found the following (possibly) invalid URLs:
    URL: https://www.destatis.de/DE/Methoden/Saisonbereinigung/BV41-methodenbericht-Heft3_2004.pdf?__blob=publicationFile
      From: DESCRIPTION
      Status: 400
      Message: Bad Request

The URL is however valid.

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

0 errors | 0 warnings | 4 notes

#### Notes

* checking CRAN incoming feasibility ... [7s/36s] NOTE
  Maintainer: ‘Dominik Schulz <dominik.schulz@uni-paderborn.de>’

  New submission
  
Same as stated for the win-builder.

* checking installed package size ... NOTE
  installed size is  5.7Mb
  sub-directories of 1Mb or more:
    libs   4.9Mb
    
Same as stated for the macOS-builder.

* checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found
  Skipping checking math rendering: package 'V8' unavailable

This is unrelated to the package.

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  ‘Rtmpb232tI\ExampleFile.csv’

The example for ```read_ts()``` creates an example file in the temporary directory and subsequently reads in that data.

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

## Comments on first submission notes

These are comments on the notes given by a CRAN member on the first tried submission of ```deseats``` (November 03, 2023).

* Please always explain all acronyms in the description text. e.g.: TS.

The acronym TS in the package title is now explained in the description text.

* It seems as you have too many blank spaces in your description text. Probably because line breaks count as blank spaces too. Please omit this.

Unnecessary blank spaces at the end of lines in the description text have been removed, so that the line breaks work as individual blank spaces and not double blank spaces.

* Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)
  Missing Rd-tags:
     animate.Rd: \value
     expo.Rd: \value
     gain.Rd: \value
     order_poly.Rd: \value
     trend.Rd: \value
     
These Rd-\value-tags were added.

* \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user.
  Does not seem necessary.
  Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}.

\dontrun{} was either removed throughout or replaced with donttest{}.

* You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object.
  Instead of print()/cat() rather use message()/warning()  or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console.
  (except for print, summary, interactive functions).

```cat()``` in ```bwidth_confint()``` was removed. All other calls to ```cat()``` in the ```deseats``` package are either in ```show()``` methods or in an interactive context.

* Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies.
  Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir().
  
The example in ```read_ts()``` was fixed accordingly, so that it now writes to
```tempdir()```.

* You are setting options(warn=-1) in your function. This is not allowed.  e.g.: inst/shiny-apps/lwr/app.R

The line ```options(warn=-1)``` was removed from the app file.
