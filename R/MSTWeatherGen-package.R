# This file is part of MSTWeatherGen package.
#
# MSTWeatherGen is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later version.
#
# MSTWeatherGen is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
# without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with MSTWeatherGen.
# If not, see <https://www.gnu.org/licenses/>.


#' @encoding UTF-8
#' @title Multivariate Space-Time Weather Generator
#' @description Simulation model of multivariate spatio-temporal meteorological variables.
#' It provides tools for estimating the model parameters and generating
#` synthetic weather data that can be used for a variety of applications,
#` including climate research, agricultural or hydrological modeling.
#' @aliases MSTWeatherGen-package MSTWeatherGen
#'
#' @author Said Obakrim \email{saidobak@@gmail.com}
#' @author Jean-François Rey \email{jean-francois@@inrae.fr}
#' @author Ahmed Boualam \email{ahmed.boualam@@inrae.fr}
#' @author Denis Allard \email{denis.allard@@inrae.fr}
#' @author Lionel Benoit \email{lionel.benoit@@inrae.fr}
#'
#' Maintainer: Jean-FRançois Rey \email{jean-francois.rey@@inrae.fr}
#' @docType package
#' @name MSTWeatherGen-package
#' @details \tabular{ll}{
#'          Package: \tab MSTWeatherGen\cr
#'          Type: \tab Package\cr
#'          Version: \tab 1.0.0.9999\cr
#'          Date: \tab 2026-18-02\cr
#'          License: \tab GPL (>=3)\cr
#'          }
#'
#' @keywords internal
#' @references
#' ## When referencing the simulation model, please cite the following article:
#'
#'
#' ## When referencing the R package, please cite the following package:
#'
#' url: https://cran.r-project.org/package=MSTWeatherGen.
#' @examples
#' \dontrun{
#' library("MSTWeather")
#'
#' }

#' @import stats
#' @importFrom Matrix nearPD
#' @import lubridate
#' @import ggplot2
#' @importFrom dplyr group_by tally mutate case_when summarise
#' @importFrom VGAM pbinorm
#' @import crch
#' @import abind
#' @import viridis
#' @import stringr
#' @import PTAk
#' @import mclust
#' @importFrom utils packageVersion
#'
"_PACKAGE"

.pkgenv <- new.env(parent = emptyenv())

#' @title Get Package and System Information
#'
#' @description Displays information about the MSTWeatherGen package and system configuration.
#' This includes the package name, version, license, and the number of CPU cores 
#' available on your computer.
#' @seealso [setCores()], [getCores()] for configuring parallel processing cores.
#' 
getInfo <- function() {
  packageStartupMessage("Package: MSTWeatherGen | MST Weather Generator")
  packageStartupMessage("Version: ", appendLF = FALSE)
  packageStartupMessage(utils::packageVersion("MSTWeatherGen"))
  packageStartupMessage("License: GPL (>= 3)")
  total <- parallel::detectCores()
  packageStartupMessage(paste0("Your computer has ", total," cores"))
}

#' @title Get the information of number of Cores for Parallel Processing for the user.
#' @description Retrieves the current number of cores configured for parallel computations.
#' If no cores have been set yet, it automatically initializes them by calling 
#' setCores().
#' @return Integer. The number of cores currently configured for parallel processing.
#' @seealso setCores() to configure the number of cores.
#' 
getCores <- function() {
  if (is.null(.pkgenv$nbcores)) {
    setCores()
  }
  return(.pkgenv$nbcores)
}

#' @title Set Number of Cores for Parallel Processing
#' @description Function to set automatically or manually the number of cores
#' It sets automatically the numbers of core as follows : nb_of_cores available - 2
#' To change manually you need to call it and put the 
#' @param n The number of cores to use. If  NULL (default), 
#' automatically sets to total_cores - 2 (minimum 1). If specified, 
#' must be at least 1 and will be capped by the maximum which will be at the total number of 
#' available cores.
#' @examples 
#' #Automatic setting setCores()
#' #Manual setting to 4 cores : setCores(4)
#' @return Number of cores set (invisible)

setCores <- function(n = NULL) {
  total_cores <- parallel::detectCores()
  
  if (is.na(total_cores)) {
    warning("Could not detect cores. Defaulting to 1 core.")
    total_cores <- 1
  }
  
  if (is.null(n)) {
    .pkgenv$nbcores <- max(1, total_cores - 2)
    packageStartupMessage("Number of cores set to ", .pkgenv$nbcores, 
                          " (total: ", total_cores, ", reserved: 2)")
    packageStartupMessage("To change manually the number of cores, use: setCores(n)")
    packageStartupMessage("To check how many cores you are using, use: getCores()")
  } else {
    if (!is.numeric(n) || n < 1) {
      stop("Number of cores must be at least 1")
    }

    n  <- max(1, as.integer(n))
    
    if (n > total_cores) {
      warning("Requested ", n, " cores but only ", total_cores, 
              " available. Setting to ", total_cores)
      n <- total_cores
    }
    
    .pkgenv$nbcores <- n
    message("Number of cores manually set to ", .pkgenv$nbcores, " out of ", total_cores, " available")
  }
  
  invisible(.pkgenv$nbcores)
}
  
#' @title Things to do at package attach
#' @name .onAttach
#' @param libname a character string giving the library directory where
#'  the package defining the namespace was found.
#' @param pkgname a character string giving the name of the package.
#' @description Print package information and check dependencies
.onAttach <- function(libname, pkgname) {
    getInfo()
    setCores()

}

