.onAttach <- function(libname, pkgname){
  packageStartupMessage("********************************************************************************")
  packageStartupMessage("                        Welcome to the package 'deseats'!")
  packageStartupMessage("********************************************************************************")
  packageStartupMessage("")  
  packageStartupMessage("Please report any possible errors and bugs to dominik.schulz.r@gmail.com.")
  packageStartupMessage("Thank you.")
  packageStartupMessage("")
  packageStartupMessage("********************************************************************************")
}

.onUnload <- function(libpath) {
  library.dynam.unload("deseats", libpath)
}