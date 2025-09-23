rm(list=ls())
library(fisheRy)
library(tidyverse)

source("tests/ref/parameters.cod.R")
source("tests/ref/simulator.7.R")

fish = new(Fish)
fish$par$flag = 1
fish$par$s0 = 0.09637
fish$par$Bhalf_growth = 1e100
# fish$par$use_old_model_gro = F
fish$par$use_old_model_mat = T
# fish$par$use_old_model_mor = F
# fish$par$use_old_model_fec = F

# sim = new(Simulator)
# res_ibm = sim$simulate(pop, 45, 0, 200, T)

pop = new(Population, fish)

pop$par$n = 5e5
pop$par$Bhalf = 365426284/4.7
# h = 0
# pop$mort_fishing_mature = -log(1-h)
# pop$mort_fishing_immature = 0
pop$set_harvestProp(0)
pop$set_minSizeLimit(45)
# pop$set_superFishSize(1e6)
pop$init(1000)
pop$get_state()

nsteps = 200
nfish = numeric(nsteps)
nfish[1]=1000
ssb = numeric(nsteps)
dat = data.frame(ssb=0, yield=0, emp_sea=0, emp_shore=0, profit_sea=0, profit_shr=0, tsb=0)
for (i in 2:nsteps){
  v = pop$update()
  dat = rbind(dat, v)
  nfish[i] = pop$nfish()
}
dat = dat[-1,]

d = pop$get_state()
dist = table(d$age, d$length)

#K_ibm = pop$calcK()

par(mfrow = c(3,1), mar=c(5,5,1,1), oma=c(1,1,1,1), cex.lab=1.5, cex.axis=1.5)
plot(nfish~seq(1,nsteps,1), ylab="Number of superfish", xlab="Year")
image(x=as.numeric(rownames(dist)), y = as.numeric(colnames(dist)), z=log(1+3*log(dist)), col=scales::viridis_pal()(100), xlab="Age", ylab="Length")
#plot(K_ibm[-1], log="xy", xlab="Age", ylab="Frequency")

res = simulate(0, 45, F)
matplot(cbind(dat$ssb/1e9, res$summaries$SSB[1:199]/1e9), ylab="SSB (MT)", xlab="Year", col=c("cyan", "black"), lty=1, type=c("p","l"), pch=1)
# points(y=res$summaries$SSB/1e9, x=res$summaries$year, type="l")


K = matrix(nrow=10, ncol=10)
hvec = seq(0,1,length.out=10)
l50vec = seq(20,160,length.out=10)
for (ih in 1:length(hvec)){
  for (il50 in 1:length(l50vec)){
    pop$set_harvestProp(hvec[ih])
    pop$set_minSizeLimit(l50vec[il50])
    K[ih,il50] = pop$fishableBiomass()
  }
}

Bfishable_nonfished = K[1,1:10]/1e9
pop_noFish = pop

pop = new(Population, fish)
# h = 0
# pop$mort_fishing_mature = -log(1-h)
# pop$mort_fishing_immature = 0
pop$set_harvestProp(0.3)
pop$set_minSizeLimit(45)
pop_noFish$set_harvestProp(0.3)
pop_noFish$set_minSizeLimit(45)
pop$K = pop_noFish$fishableBiomass()
pop$set_superFishSize(500)
pop$init(1000)
pop$get_state()

nsteps = 200
nfish = numeric(nsteps)
nfish[1]=1000
ssb = numeric(nsteps)
dat = data.frame(ssb=0, yield=0, emp_sea=0, emp_shore=0, profit_sea=0, profit_shr=0)
for (i in 2:nsteps){
  v = pop$update()
  dat = rbind(dat, v)
  nfish[i] = pop$nfish()
}
dat = dat[-1,]

d = pop$get_state()
dist = table(d$age, d$length)

#K_ibm = pop$calcK()

par(mfrow = c(2,3), mar=c(4,4,1,1))
plot(nfish~seq(1,nsteps,1), ylab="Number of superfish", xlab="Year")
image(x=as.numeric(rownames(dist)), y = as.numeric(colnames(dist)), z=log(1+3*log(dist)), col=scales::viridis_pal()(100), xlab="", ylab="")
plot(dat$ssb/1e9, ylab="SSB (MT)", xlab="Year")
plot(dat$yield/1e9, ylab="Yield (MT)", xlab="Year", col="cyan3", type="l")
matplot(cbind(dat$emp_sea, dat$emp_shore), ylab="Employment at sea/shore (Person years)", xlab="Year", col=c("seagreen", "bisque3"), type="l", lty=1, lwd=2)
matplot(cbind(dat$profit_sea, dat$profit_shr)/1e9, ylab="Profit at sea/shore (Billion NOK)", xlab="Year", col=c("seagreen", "bisque3"), type="l", lty=1, lwd=2)

#plot(K_ibm[-1], log="xy", xlab="Age", ylab="Frequency")

K = matrix(nrow=10, ncol=10)
hvec = seq(0,1,length.out=10)
l50vec = seq(20,160,length.out=10)
for (ih in 1:length(hvec)){
  for (il50 in 1:length(l50vec)){
    pop$set_harvestProp(hvec[ih])
    pop$set_minSizeLimit(l50vec[il50])
    K[ih,il50] = pop$fishableBiomass()
  }
}

Bfishable_h0.1 = K[1,1:10]/1e9

# matplot(x = l50vec, y= cbind(Bfishable_nonfished, Bfishable_h0.1), col=c("black", "blue"), type="l", lty=1, ylab="Fishable Biomass (MT)", xlab="L50")

library(tidyverse)
library(fisheRy)



dat = read.csv(here::here("test_fisherysystem.csv"), header=F)
dat = dat |> setNames(c("i", "ssb", "tsb", "maturity", "quota", "yield", "effort")) |> 
  mutate(ssb = ssb/1e9, tsb = tsb/1e9, quota = quota/1e9, yield = yield/1e9, effort = effort)

dat |> head()

dat |> 
  ggplot(aes(x=quota, y=yield)) +
  geom_line() +
  geom_abline(slope=1, intercept=0, color="red")

dat |> 
  ggplot(aes(x=tsb, y=quota)) +
  geom_line() +
  geom_abline(slope=0.5, intercept=0, color="red")

dat |> 
  ggplot(aes(x=i, y=ssb)) +
  geom_line() 



library(fisheRy)

params_file_fleet = here::here("params/fleet_1_params.ini")
params_file_fish  = here::here("params/cod_params.ini")

fleet = new(Fleet)
fleet$readParams(params_file_fleet, T)
fleet$par$print();

fish = new(Fish, params_file_fish)

fishery = new(Fishery, params_file_fish, fish);
fishery$par$print();

fishery$set_harvestProp(0.5);
fishery$addFleet(params_file_fleet, T);

fishery$equilibriateNaturalPopulation(5.61, 2e6);

fishery$init(1000, 0, 5.61);

quota = fishery$calc_quota(5.61);
cat("Quota: ", quota, '\n')

fishery$simulate(45, 0.55, 200, 0, 5.61, T, here::here("fishery_output/age_dist_pred.csv"))

