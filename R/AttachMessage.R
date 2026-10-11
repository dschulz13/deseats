.onAttach <- function(libname, pkgname){
  packageStartupMessage("********************************************************************************")
  packageStartupMessage("                        Welcome to the package 'deseats'!")
  packageStartupMessage("********************************************************************************")
  packageStartupMessage("")  
  packageStartupMessage("Please report any possible errors and bugs to dominik.schulz.r@gmail.com or")
  packageStartupMessage("to https://github.com/dschulz13/deseats/issues.")
  packageStartupMessage("Thank you.")
  packageStartupMessage("")
  packageStartupMessage("********************************************************************************")
}

.onUnload <- function(libpath) {
  library.dynam.unload("deseats", libpath)
}