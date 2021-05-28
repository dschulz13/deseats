## Updated release (version 1.1.0)
This is an updated release that incorporates new elements.

## Test environments
* local Windows 10 install, R 4.0.5
* win-builder (devel, release and oldrelease)
* R-hub

## R CMD check results
There were no ERRORs or WARNINGs.

There were 3 NOTEs:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Dominik Schulz <schulzd@mail.uni-paderborn.de>'
  
There was a change of email address of the maintainer Dominik Schulz 
from <schulzd@mail.uni-paderborn.de> to 
<dominik.schulz@uni-paderborn.de>. While the former address is still 
valid, the latter one is preferred for future package versions. 
  
* checking compiled code ... NOTE
    Note: information on .o files for i386 is not available
    Note: information on .o files for x64 is not available
    File 'C:/Users/domin/DOCUME~1/02-WHB~1/Github/SMOOTS~1.RCH/smoots/libs/i386/smoots.dll':
      Found '_exit', possibly from '_exit' (C)
      Found 'abort', possibly from 'abort' (C), 'runtime' (Fortran)
      Found 'exit', possibly from 'exit' (C), 'stop' (Fortran)
      Found 'printf', possibly from 'printf' (C)
    File 'C:/Users/domin/DOCUME~1/02-WHB~1/Github/SMOOTS~1.RCH/smoots/libs/x64/smoots.dll':
      Found '_exit', possibly from '_exit' (C)
      Found 'abort', possibly from 'abort' (C), 'runtime' (Fortran)
      Found 'exit', possibly from 'exit' (C), 'stop' (Fortran)
      Found 'printf', possibly from 'printf' (C)
  
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor use Fortran I/O
    nor system RNGs. The detected symbols are linked into the code but
    might come from libraries and not actually be called.
  
    See 'Writing portable packages' in the 'Writing R Extensions' manual.  
  
This NOTE occurs only when using R CMD Check on my local Windows 10 computer. 
There is actually no C or Fortran code that was added. Following a quick research on the internet, e.g. <https://stackoverflow.com/questions/64402688/information-on-o-files-for-x64-is-not-available-note-on-r-package-checks-using>,
this appears to be an unexpected NOTE caused by R CMD check on Windows 10.
  
* checking installed package size ... NOTE
  installed size is  6.2Mb
  sub-directories of 1Mb or more:
    libs   5.4Mb

This NOTE occurs only for Ubuntu Linux. Presumably, it is caused by the 
inclusion of C++ code via the Rcpp package. The inclusion of C++ code, 
however, is needed to notably increase the smoots package's performance 
when applying its functions to datasets with a high number of observations.

There was another notification:

  Possibly mis-spelled words in DESCRIPTION:
    backtesting (11:58)

The spelling is, however, correct.

### Comments for older package versions

## Patch release (version 1.0.1)
This is a patch release based on the CRAN checks of version 1.0.0.

## Test environments
* local Windows 10 install, R 3.6.1
* win-builder (devel and release)
* R-hub

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Dominik Schulz <schulzd@mail.uni-paderborn.de>’

  Days since last update: 6

The initial release was accepted by CRAN 6 days ago. 
This patch is needed to fix the NOTE 

* Namespace in Imports field not imported from: ‘utils’
  All declared Imports should be used. 
  
that was returned by the CRAN checks of version 1.0.0. For this purpose, 
'utils' was deleted from and 'graphics' was added to the Imports field 
of the DESCRIPTION file. Please excuse the inconvenience.

Other notifications:

* Possibly mis-spelled words in DESCRIPTION:
  Feng (12:5)
  Gries (12:19)
  Nadaraya (9:51)

  The names are spelled correctly.
