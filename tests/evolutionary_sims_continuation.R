rm(list=ls())
library(fisheRy)
library(tidyverse)
setwd("~/codes/fisheRy/vignettes")

source("../tests/evolutionary_sims_sq.R")

h_trt = 0.6 # h_sq
T_trt = T_sq

pop$verbose = T
nsteps_t = 1000             # Let's simulate for 200 years
nfish_t = numeric(nsteps_t)  # Let's keep track of the number of superfish
nfish_t[1]=1000            # Since we initialized the population with 1000 superfish
pop$set_harvestProp(h_trt)

cnames = pop$colnames
dat_t = data.frame(matrix(ncol=length(cnames), nrow=0))
colnames(dat_t) = cnames
trait_dist_t = array(dim = c(6, nsteps_t, 100))
trait_means_t = array(dim=c(6, nsteps_t))

for (i in 1:nsteps_t){
  v = pop$update(T_trt) #temp[i])       # Update all fish over 1 year 
  dat_t[i,] = v    # some book-keeping
  traits = pop$get_traits()
  traits = pop$get_traits()
  for (it in 1:6){
    if (i %% skip == 0){
      h = hist(traits[,it]/trait_scalars[it], breaks = trait_breaks, plot=F)
      trait_dist_t[it,i/skip,] = h$density
    }
    trait_means_t[it,i] = abs(mean(traits[,it]))
  }
  nfish_t[i] = pop$nfish()
}

d = pop$get_state()
dist = table(d$age, d$length)

par(mfrow = c(3,1), mar=c(5,5,1,1), oma=c(1,1,1,1), cex.lab=1.5, cex.axis=1.5)
plot(nfish_t~seq(1,nsteps_t,1), ylab="No. of superfish", xlab="Year")
image(x=as.numeric(rownames(dist)), y = as.numeric(colnames(dist)), z=log(1+3*log(dist)), col=scales::viridis_pal()(100), xlab="Age", ylab="Length")

res = simulate(0, 45, F)
matplot(c(dat$ssb, dat_t$ssb)/1e9, ylab="SSB (MT)", xlab="Year", col=c("cyan4", "black"), lty=1, type=c("l"), pch=1)

png(filename = paste0("../results_repoty/evol_h_effect_",str_replace_all(Sys.time(), ":", "."),".png"), width = 500*3, height = 400*3, res=300)
par(mfrow = c(2,2), mar=c(4,4,1,1), oma=c(1,1,1,1))
for (it in 1:4){
  # image(y=(trait_breaks[-1]-diff(trait_breaks)/2)*abs(trait_scalars[it]), x=seq(1,nsteps+nsteps_t,length.out=), z=log(1e-4+rbind(trait_dist[it,,], trait_dist_t[it,,])), col=scales::viridis_pal()(100), xlab="Time", ylab=trait_names[it]) 
  plot(y=c(trait_means[it,],trait_means_t[it,])[-(1:500)], x=501:(nsteps+nsteps_t), col="cyan3", type="l", lwd=2,
       xlab="Time", ylab=trait_names[it])
  abline(v=nsteps, col="pink")
}
dev.off()

par(mfrow = c(3,4), mar=c(5,5,5,1), oma=c(1,1,1,1), cex.lab=1.5, cex.axis=1.5)
comp = as.data.frame(matrix(ncol=5, nrow=0))
colnames(comp) = c("trait", "control_int", "control_m", "trt_int", "trt_m")
for (it in 1:6){
  # cat(it, "\n")
  x1 = 101:(nsteps/2)
  y1 = trait_means[it,x1]
  mod1 = lm(y1~x1)
  m1 = summary(mod1)
  
  x2 = (nsteps/2+101):nsteps
  y2 = trait_means[it,x2]
  mod2 = lm(y2~x2)
  m2 = summary(mod2)
  
  barplot(c(c(m1$coefficients[1,1], m2$coefficients[1,1])), names.arg = c("control", "treatment"), main = paste(trait_names[it],"\nIntercept"))
  barplot(c(c(m1$coefficients[2,1], m2$coefficients[2,1])), names.arg = c("control", "treatment"), main = paste(trait_names[it],"\nSlope"))
  # abline(h=0, col="grey")
  # plot(c(y1,y2)~c(x1,x2))
  
  comp[it, 1] = trait_names[it]
  comp[it, 2:5] = c(m1$coefficients[,1], m2$coefficients[,1])
}
