library(fisheRy)
library(tidyverse)

rm(list=ls())

setwd("~/codes/fisheRy/vignettes")

source("../tests/ref/parameters.cod.R")
source("../tests/ref/simulator.7.R")

params_file = "../params/cod_params.ini"

re_simulate_global = T


h_sq = 0.5
T_sq = 5.61

#### FISH ####

fish = new(Fish, params_file)

#### Initial run to remove initial condition transient ####

pop = new(Population, fish)  
pop$par$n = 1e6  # Each superfish contains so many fish
pop$par$f_harvest_spg = 0.30
pop$set_harvestProp(0)
pop$init(1000, 1.93e3, T_sq)  # initialize the population with 1000 agents (superfish)
pop$verbose = T

cnames = pop$colnames
dat = data.frame(matrix(ncol=length(cnames), nrow=0))
colnames(dat) = cnames
for (i in 1:100){
  v = pop$update(T_sq) #temp[i])       # Update all fish over 1 year 
  dat[i,] = v
}

#### Small status quo run to remove demographic transient ####

pop$set_harvestProp(h_sq)

for (i in 101:200){
  v = pop$update(T_sq) #temp[i])       # Update all fish over 1 year 
  dat[i,] = v
}


plot(dat$ssb~seq(1,200), type="l", col="cyan3")
traits0 = colMeans(pop$get_traits())


#### Bring population upto current year (2003) under historical temperature increase ####

pop$readEnvironmentFile("../data/env_hist.csv")
pop$par$update_env = T
pop$current_year = 2003

for (y in 2003:2014){
  pop$updateEnv(pop$current_year)
  v = pop$update(pop$env$temperature) 
  dat[201 + y-2003,] = v
}

plot(dat$ssb~seq(1,212), type="l", col="cyan3")
traits0 = colMeans(pop$get_traits())

pop$verbose = T
pop$par$rmax = 2e10

fish$set_traits(as.numeric(colMeans(pop$get_traits())))

nsteps_jss=2100-2016+1

sim = new(Simulator, fish)
sim$equilibriateNaturalPopulation(1.93e3, pop$env$temperature, 1e6)
colMeans(sim$noFishingPop$get_traits())

hvec = c(0.1)
tvec = c(5.61)
lfvec = c(50)

pop$readEnvironmentFile("../data/env_ssp5.csv")

res_ibm_full = sim$simulate_multi_2d(pop, tvec, lfvec, hvec, nsteps_jss, 1.93e3, F)


ssb_vec = c(dat$ssb, res_ibm_full[,1,1,1,1])
years = seq(to=2100, by=1, length.out=length(ssb_vec))
# png(filename = paste0("../results_repoty/ppo_spinups_",str_replace_all(Sys.time(), ":", "."),".png"), width = 500*3, height = 400*3, res=300)
plot(y=ssb_vec/1e9,
     x= years,
     type="l", col="cyan3",
     ylab="SSB (MT)", xlab="Year")
abline(v=2001, col="pink")
abline(v=years[1]+100, col="grey")
# dev.off()
