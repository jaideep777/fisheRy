# fish.R

#' fish
#'
#' fish class
#' 
#' Imports
#' @useDynLib fisheRy, .registration=T
#' @export fish_module, population_module, fleet_module, simulator_module
#' @import Rcpp
"_PACKAGE"


Rcpp::loadModule(module="fish_module", what=T)
Rcpp::loadModule(module="population_module", what=T)
Rcpp::loadModule(module="fleet_module", what=T)
Rcpp::loadModule(module="simulator_module", what=T)


