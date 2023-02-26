library(fisheRy)
library(tidyverse)

setwd("~/codes/fisheRy/vignettes")

source("../tests/ref/parameters.cod.R")
source("../tests/ref/simulator.7.R")

params_file = "../params/cod_params.ini"

re_simulate_global = T

#### FISH ####

fish = new(Fish, params_file)
fish$trait_variances = c(1, 1, 1, 1, 1, 0)*(0.05)^2

#### Status quo run ####

pop = new(Population, fish)  
pop$par$n = 1e6  # Each superfish contains so many fish
pop$par$f_harvest_spg = 0.20
pop$set_harvestProp(0)
pop$init(1000, 1.93e3, 5.61)  # initialize the population with 1000 agents (superfish)

pop$verbose = T
nsteps = 10000             # Let's simulate for 200 years
nfish = numeric(nsteps)  # Let's keep track of the number of superfish
nfish[1]=1000            # Since we initialized the population with 1000 superfish

nt_dist = 500
skip = nsteps/nt_dist
cnames = pop$colnames
dat = data.frame(matrix(ncol=length(cnames), nrow=0))
colnames(dat) = cnames
trait_dist = array(dim = c(6, nt_dist, 100), data = NA)
trait_scalars = c(6.5, 0.09, 150, -6.6, 50, 0.06)
trait_breaks = seq(0,2.5,length.out=101)
trait_names = c("Growth capacity", "GSI", "PMRN intercept", "PMRN slope", "PMRN width", "s0")
trait_means = array(dim=c(6, nsteps))

for (i in 1:100){
  v = pop$update(5.61) #temp[i])       # Update all fish over 1 year 
}

pop$set_harvestProp(0.5)

for (i in 1:nsteps){
  v = pop$update(5.61) #temp[i])       # Update all fish over 1 year 
  dat[i,] = v    # some book-keeping
  traits = pop$get_traits()
  for (it in 1:6){
    if (i %% skip == 0){
      h = hist(traits[,it]/trait_scalars[it], breaks = trait_breaks, plot=F)
      trait_dist[it,i/skip,] = h$density
    }
    trait_means[it,i] = abs(mean(traits[,it]))
  }
  nfish[i] = pop$nfish()
  
  if (i %% 2000 == 0){
    par(mfrow = c(3,2), mar=c(4,4,1,1), oma=c(1,1,1,1), cex.lab=1.3, cex.axis=1.2)
    for (it in 1:6){
      image(y=(trait_breaks[-1]-diff(trait_breaks)/2)*abs(trait_scalars[it]), x=seq(1,nsteps,skip), z=log(1e-4+trait_dist[it,,]), col=scales::viridis_pal()(100), xlab="Time", ylab=trait_names[it]) 
      lines(y=trait_means[it,], x=1:nsteps, col="white", lwd=2)
    }
  }
  
}


d = pop$get_state()
dist = table(d$age, d$length)
traits = pop$get_traits()

par(mfrow = c(3,1), mar=c(5,5,1,1), oma=c(1,1,1,1), cex.lab=1.5, cex.axis=1.5)
plot(nfish~seq(1,nsteps,1), ylab="No. of superfish", xlab="Year")
image(x=as.numeric(rownames(dist)), y = as.numeric(colnames(dist)), z=log(1+3*log(dist)), col=scales::viridis_pal()(100), xlab="Age", ylab="Length")

res = simulate(0, 45, F)
matplot(cbind(dat$ssb/1e9), ylab="SSB (MT)", xlab="Year", col=c("cyan4", "black"), lty=1, type=c("p","l"), pch=1)

png(filename = "../results_repoty/trait_evol2_n1e6.png", width = 500*3, height = 500*3, res=300)
par(mfrow = c(3,2), mar=c(4,4,1,1), oma=c(1,1,1,1), cex.lab=1.3, cex.axis=1.2)
for (it in 1:6){
  image(y=(trait_breaks[-1]-diff(trait_breaks)/2)*abs(trait_scalars[it]), x=seq(1,nsteps,skip), z=log(1e-4+trait_dist[it,,]), col=scales::viridis_pal()(100), xlab="Years", ylab=trait_names[it], useRaster = T) 
  lines(y=trait_means[it,], x=1:nsteps, col="white", lwd=2)
}
dev.off()

pop$print_summary()

plot_traits_now = function(traits){
  par(mfrow = c(3,2), mar=c(5,5,5,1), oma=c(1,1,1,1), cex.lab=1.5, cex.axis=1.5)
  hist(traits$alpha1)
  hist(traits$gsi)
  hist(traits$pmrn_intercept)
  hist(traits$pmrn_slope)
  hist(traits$pmrn_width)
  hist(traits$s0)
}

plot_traits_now(pop$get_traits())

#### JSS runs ####


calc_jss = function(res_ibm_full, nsteps_jss){
  arr = res_ibm_full[,,,,1:4]
  d = sim$max_avg_utils_2d(c(4,length(tvec), length(lfvec),length(hvec),nsteps_jss), arr)
  utils = array(data=d, dim=c(length(hvec),length(lfvec),length(tvec), 4))
  
  for (i in 1:4){
    utils[,,,i][which(utils[,,,4]<0)]=NA
  }
  
  ss = sim$stakeholder_satisfaction_2d(c(4,length(tvec), length(lfvec),length(hvec),nsteps_jss), arr)
  scs = array(data=ss, dim=c(length(hvec),length(lfvec),length(tvec), 5))
  
  for (i in 1:5){
    scs[,,,i][which(is.na(utils[,,,4]))]=NA
  }
  
  JSS = array(dim=c(length(hvec),length(lfvec),length(tvec),3))
  
  JSS[,,,1] = apply(scs, c(1,2,3), min)
  JSS[,,,2] = apply(scs, c(1,2,3), mean)
  JSS[,,,3] = 1/apply(1/scs, c(1,2,3), mean)
  
  JSS  
}


my.image <- function(x,y,z,  zlim, col, na.color='gray95', outside.color='white', ...)
{
  newz.na <- zlim[2]+(zlim[2]-zlim[1])/length(col) # new z for NA
  newz.outside <- zlim[2]+2*(zlim[2]-zlim[1])/length(col) # new z for values outside zlim
  
  z[which(is.na(z>zlim[2]))] <- newz.na # we affect newz.outside
  #  z[which(z<zlim[1] | z>zlim[2])] <- newz.outside # same for newz.na
  
  
  zlim[2] <- zlim[2]+2*(zlim[2]-zlim[1])/length(col) # we finally extend the z limits to include the two new values 
  
  col <- c(col, na.color, outside.color) # we construct the new color range by including: na.color and outside.color
  
  image(x,y,z,  zlim=zlim, col=col, useRaster = T, ...) # we finally call image(...)
}

# Function to plot color bar
color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}



pop$verbose = F

fish$set_traits(as.numeric(colMeans(pop$get_traits())))

nsteps_jss=100

sim = new(Simulator, fish)
sim$equilibriateNaturalPopulation(1.93e3, 5.61, 1e6)
colMeans(sim$noFishingPop$get_traits())

hvec = seq(0, 0.8, length.out = 20)
lfvec = seq(10, 100, length.out = 20)
tvec = c(5.61)

saved_filename = paste0("res_ibm_hxL_T",tvec[1],"_evol_n",nsteps_jss,".Rdata")
re_simulate = re_simulate_global | !file.exists(saved_filename)

if (re_simulate){ 
  res_ibm_full = sim$simulate_multi_2d(pop, tvec, lfvec, hvec, nsteps_jss, 1.93e3, F)
  save(res_ibm_full, file = saved_filename)
} else{
  load(saved_filename)
}

JSS = calc_jss(res_ibm_full, nsteps_jss)


par(mfrow=c(2,2), mar=c(5,5,4,1), oma=c(1,1,1,1), cex.lab=1.25, cex.axis=1.2)
cols = scales::viridis_pal()(100)
for (i in 1:3){
  my.image(x=hvec, y=lfvec, z=JSS[,,1,i], col=cols, zlim=c(0,1), main=c("JSS (arithmatic mean)","JSS (harmonic mean)","JSS (minimum)")[i], 
           xlab="Harvest proportion", ylab=expression("Minimum size limit (L"[50]*")"))
}
color.bar(cols, 0,1, nticks = 5)


