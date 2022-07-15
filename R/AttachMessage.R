.onAttach <- function(libname, pkgname){
  packageStartupMessage("********************************************************************************")
  packageStartupMessage("                        Welcome to the package 'DeSeaTS'!")
  packageStartupMessage("********************************************************************************")
  packageStartupMessage("")
  packageStartupMessage("This is just a pre-release version of the package and thus a work in progress.")
  packageStartupMessage("Do not share or distribute the code or any of the files included in this")  
  packageStartupMessage("package.")  
  packageStartupMessage("")    
  packageStartupMessage("Using this version of the package is not recommended for official publications,")   
  packageStartupMessage("as it is not stable yet and potentially prone to major changes in the near")    
  packageStartupMessage("future.")   
  packageStartupMessage("")  
  packageStartupMessage("Please report any possible errors and bugs to dominik.schulz@uni-paderborn.de.")
  packageStartupMessage("Thank you.")
  packageStartupMessage("")
  packageStartupMessage("********************************************************************************")
}

.onUnload <- function(libpath) {
  library.dynam.unload("DeSeaTS", libpath)
}