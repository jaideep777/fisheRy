library(fisheRy)
library(tidyverse)

setwd("~/codes/fisheRy/vignettes")

source("../tests/ref/parameters.cod.R")
source("../tests/ref/simulator.7.R")


re_simulate = F
# if one of the files doesnt exist, re-run the simulations
re_simulate = re_simulate | !file.exists("res_ibm_jss_hxL_evol.Rdata")


params_file = "../params/cod_params.ini"
