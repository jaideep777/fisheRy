rm(list=ls())
library(fisheRy)
library(tidyverse)
setwd("~/codes/fisheRy/vignettes")

# source("../tests/evolutionary_sims_sq.R")
# 
# #### Bring population upto current year (2003) under historical temperature increase ####
# 
# pop$readEnvironmentFile("../data/env_hist.csv")
# pop$par$update_env = T
# pop$current_year = 2003
# 
# for (y in 2003:2014){
#   pop$updateEnv(pop$current_year)
#   v = pop$update(pop$env$temperature) 
# }
# 
# plot_traits_now(pop$get_traits())
# 

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
  par(mfrow=c(2,3), mar=c(5,5,4,1), oma=c(1,1,1,1), cex.lab=1.2)
  for (i in 1:5){
    my.image(x=hvec, y=lfvec, z=scs[,,2,i], col=scales::viridis_pal()(100), zlim=c(0,1), main=c("Industrial fishers", "Artisanal fishers", "Employment-prioritizing\npolicymakers", "Profit-prioritizing\npolicymakers", "Conservationists")[i], 
             xlab="Harvest proportion", ylab=expression("Minimum size limit (L"[50]*")"))
  }  
  
  JSS = array(dim=c(length(hvec),length(lfvec),length(tvec),3))
  
  JSS[,,,1] = apply(scs, c(1,2,3), min)
  JSS[,,,2] = apply(scs, c(1,2,3), mean)
  JSS[,,,3] = 1/apply(1/scs, c(1,2,3), mean)
  
  list(utils=utils, scs=scs, JSS=JSS)  
}


calc_jss_t = function(res_ibm_full1, nsteps_jss){
  arr = res_ibm_full1[,,,,1:4]

  d = sim$max_avg_utils_2d(c(4,length(tvec), length(lfvec),length(hvec),nsteps_jss), arr)
  utils = array(data=d, dim=c(length(hvec),length(lfvec),length(tvec), 4))
  
  for (i in 1:4){
    utils[,,,i][which(utils[,,,4]<0)]=NA
  }
  par(mfrow=c(2,2), mar=c(5,5,4,1), oma=c(1,1,1,1), cex.lab=1.2)
  for (i in 1:4){
    my.image(x=hvec, y=lfvec, z=utils[,,2,i], col=scales::viridis_pal()(100), zlim=c(0,1), main=c("Spawning stock biomass","Yield","Employment","Profit")[i], 
             xlab="Harvest proportion", ylab=expression("Minimum size limit (L"[50]*")"))
  }
  
  ss = sim$stakeholder_satisfaction_2d_t(c(4,length(tvec), length(lfvec),length(hvec),nsteps_jss), arr)
  scs = array(data=ss, dim=c(nsteps_jss, length(hvec),length(lfvec),length(tvec), 5))
  
  for (t in 1:nsteps_jss){
    for (i in 1:5){
      scs[t,,,,i][which(is.na(utils[,,,4]))]=NA
    }
  }
  par(mfrow=c(2,3), mar=c(5,5,4,1), oma=c(1,1,1,1), cex.lab=1.2)
  for (i in 1:5){
    scs_mean = apply(scs, c(2,3,4,5), mean)
    my.image(x=hvec, y=lfvec, z=scs_mean[,,2,i], col=scales::viridis_pal()(100), zlim=c(0,1), main=c("Industrial fishers", "Artisanal fishers", "Employment-prioritizing\npolicymakers", "Profit-prioritizing\npolicymakers", "Conservationists")[i], 
             xlab="Harvest proportion", ylab=expression("Minimum size limit (L"[50]*")"))
  }  
  
  JSS = array(dim=c(nsteps_jss, length(hvec),length(lfvec),length(tvec),3))
  
  JSS[,,,,1] = apply(scs, c(1,2,3,4), min, na.rm=T)
  JSS[,,,,2] = apply(scs, c(1,2,3,4), mean, na.rm=T)
  JSS[,,,,3] = 1/apply(1/scs, c(1,2,3,4), mean, na.rm=T)
  
  list(utils=utils, scs=scs, JSS=JSS)  
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



# #### JSS runs ####
# 
# re_simulate_global = F
# 
# pop$verbose = F
# pop$set_superFishSize(2e6)
# pop$par$rmax = 2e10
# 
# fish$set_traits(as.numeric(colMeans(pop$get_traits())))
# 
# sim = new(Simulator, fish)
# sim$equilibriateNaturalPopulation(1.93e3, pop$env$temperature, 1e6)
# colMeans(sim$noFishingPop$get_traits())
# 
# hvec = seq(0, 0.85, length.out = 20)
# lfvec = seq(10, 80, length.out = 20)
# tvec = c(1,2,3,4)
# 
# for (tf in c(2100)){
#   nsteps_jss=tf-2016+1
#   
#   for (ssp in c(5,1,2,3)){
#     pop$readEnvironmentFile(paste0("../data/env_ssp",ssp,".csv"))
#     
#     saved_filename = paste0("res_ibm_hxL_T_ssp",ssp,"_evol_tf",tf, "n2e6.Rdata")
#     re_simulate = re_simulate_global | !file.exists(saved_filename)
#     
#     if (re_simulate){ 
#       res_ibm_full = sim$simulate_multi_2d(pop, tvec[1], lfvec, hvec, nsteps_jss, 1.93e3, F)
#       save(res_ibm_full, file = saved_filename)
#     } else{
#       load(saved_filename)
#     }
#     
#     JSS = calc_jss(res_ibm_full, nsteps_jss)
#     
#     png(filename = paste0("../results_repoty/jss_n2e6_ssp",ssp,"_",str_replace_all(Sys.time(), ":", "."),".png"), width = 1000*3, height = 300*3, res=300)
#     par(mfrow=c(1,4), mar=c(5,5,4,1), oma=c(1,1,1,1), cex.lab=1.25, cex.axis=1.2)
#     cols = scales::viridis_pal()(100)
#     for (i in 1:3){
#       my.image(x=hvec, y=lfvec, z=JSS[,,1,i], col=cols, zlim=c(0,1), main=c("JSS (minimum)","JSS (arithmatic mean)","JSS (harmonic mean)")[i], 
#                xlab="Harvest proportion", ylab=expression("Minimum size limit (L"[50]*")"))
#     }
#     color.bar(cols, 0,1, nticks = 5)
#     dev.off()
#     
#   }  
# }
# 



##### Composite all scenarios ####
hvec = seq(0, 0.85, length.out = 20)
lfvec = seq(10, 80, length.out = 20)
tvec = c(1,2,3,4)
nsteps_jss = 2100-2016+1

res_ibm_composite = array(data=NA, dim=c(nsteps_jss, length(hvec),length(lfvec),length(tvec), 21))
issp=1
for (ssp in c(1,2,3,5)){
  saved_filename = paste0("res_ibm_hxL_T_ssp",ssp,"_evol_tf",2100, "n2e6.Rdata")
  load(saved_filename)

  par(mfrow=c(2,2), mar=c(5,5,4,1), oma=c(1,1,1,1), cex.lab=1.2)
  for(i in 1:4)
    image(x=hvec, y=lfvec, z=res_ibm_full[40,,,1,i], col=scales::viridis_pal()(100), main=c("ssb","yield","emp","profit")[i],
          xlab="Harvest proportion", ylab=expression("Minimum size limit (L"[50]*")"))

  res_ibm_composite[,,,issp,] = res_ibm_full[,,,1,]
  issp = issp+1
}

# Check that compositing succeeded by plotting SSB~year for 4 scenarios
png(filename = paste0("../results_repoty/timeseries_4x4_scenarioxcontrol_",str_replace_all(Sys.time(), ":", "."),".png"), width = 800*3, height = 800*3, res=300)
par(mfrow=c(2,2), mar=c(5,5,3,1), oma=c(1,1,1,1), cex.lab=1.2)
matplot(y=res_ibm_composite[,10,10,,1]/1e9, x=2016:2100, type="l", lty=1, col = scales::viridis_pal()(4), ylab="SSB (MT)", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f", hvec[10], lfvec[10]))
matplot(y=res_ibm_composite[,1,1,,1]/1e9, x=2016:2100, type="l", lty=1, col = scales::viridis_pal()(4), ylab="SSB (MT)", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f", hvec[1], lfvec[1]))
matplot(y=res_ibm_composite[,20,20,,1]/1e9, x=2016:2100, type="l", lty=1, col = scales::viridis_pal()(4), ylab="SSB (MT)", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f", hvec[20], lfvec[20]))
matplot(y=res_ibm_composite[,1,20,,1]/1e9, x=2016:2100, type="l", lty=1, col = scales::viridis_pal()(4), ylab="SSB (MT)", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f", hvec[1], lfvec[20]))
dev.off()

png(filename = paste0("../results_repoty/ssb_4years_ssp2_",str_replace_all(Sys.time(), ":", "."),".png"), width = 500*3, height = 800*3, res=300)
par(mfrow=c(3,2), mar=c(5,5,4,1), oma=c(1,1,1,1), cex.lab=1.2)
for(t in c(1,20,40,80)){
  cols = scales::viridis_pal()(100)
  my.image(x=hvec, y=lfvec, z=res_ibm_composite[t,,,2,1]/1e9, zlim=c(0,8), col=cols, main=paste("year =",2015+t),
           xlab="Harvest proportion", ylab="Minimum size limit (cm)")
}
color.bar(cols, 0,8, nticks = 5, title = "SSB (MT)")
dev.off()

png(filename = paste0("../results_repoty/normalized_timeavg_utils_ssp2_",str_replace_all(Sys.time(), ":", "."),".png"), width = 500*3, height = 800*3, res=300)
par(mfrow=c(3,2), mar=c(5,5,4,1), oma=c(1,1,1,1), cex.lab=1.2)
for (i in 1:4){
  cols = scales::viridis_pal()(100)
  avg_util = apply(res_ibm_composite[,,,2,i], c(2,3), mean)
  image(x=hvec, y=lfvec, z=avg_util/max(avg_util), zlim=c(0,1), col=cols, main=c("SSB","Employment","Yield","Profit")[i],
      xlab="Harvest proportion", ylab="Minimum size limit (cm)")
}
color.bar(cols, 0,1, nticks = 5)
dev.off()

##### Calculate composite with 20 year means ####

dim_20 = dim(res_ibm_composite)
dim_20[1] = 3
res_ibm_composite_20mean = array(data=NA, dim = dim_20)
i=1
for (iy in c(1,30,60)){
  res_ibm_composite_20mean[i,,,,] = apply(res_ibm_composite[iy:(iy+19),,,,], c(2,3,4,5), mean)
  i=i+1
}  

png(filename = paste0("../results_repoty/timeseries_20yrmeans_4x4_scenarioxcontrol_",str_replace_all(Sys.time(), ":", "."),".png"), width = 800*3, height = 800*3, res=300)
par(mfrow=c(2,2), mar=c(5,5,3,1), oma=c(1,1,1,1), cex.lab=1.2)
matplot(y=res_ibm_composite_20mean[,10,10,,1]/1e9, x=2016+10+c(1,30,60), type="o", pch=1, lty=1, col = scales::viridis_pal()(4), ylab="SSB", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f", hvec[10], lfvec[10]))
matplot(y=res_ibm_composite_20mean[,1,1,,1]/1e9, x=2016+10+c(1,30,60), type="o", pch=1, lty=1, col = scales::viridis_pal()(4), ylab="SSB", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f", hvec[1], lfvec[1]))
matplot(y=res_ibm_composite_20mean[,20,20,,1]/1e9, x=2016+10+c(1,30,60), type="o", pch=1, lty=1, col = scales::viridis_pal()(4), ylab="SSB", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f", hvec[20], lfvec[20]))
matplot(y=res_ibm_composite_20mean[,1,20,,1]/1e9, x=2016+10+c(1,30,60), type="o", pch=1, lty=1, col = scales::viridis_pal()(4), ylab="SSB", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f", hvec[1], lfvec[20]))
dev.off()

##### Calculate JSS timeseries ####

params_file = "../params/cod_params.ini"

fish = new(Fish, params_file)
sim = new(Simulator, fish)

JSS_tmean = calc_jss(res_ibm_composite, nsteps_jss)
JSS_t20 = calc_jss_t(res_ibm_composite_20mean, 3)

##### Plot JSS scenario x method ####

hvec = seq(0, 0.85, length.out = 20)
lfvec = seq(10, 80, length.out = 20)
tvec = c(1,2,3,4)

png(filename = paste0("../results_repoty/with_contour_jss_n2e6_ssp","all_tw85_",str_replace_all(Sys.time(), ":", "."),".png"), width = 1000*3, height = 1100*3, res=300)
par(mfrow=c(4,4), mar=c(5,5,4,1), oma=c(1,1,1,1), cex.lab=1.25, cex.axis=1.2)

for (issp in c(1,2,3,4)){
  cols = scales::viridis_pal()(100)
  cols = RColorBrewer::brewer.pal(11, "Spectral")
  cols = viridis::inferno(100)
  cols = paletteer::paletteer_c("ggthemes::Sunset-Sunrise Diverging", 100) 
  cols = paletteer::paletteer_c("ggthemes::Red-Blue-White Diverging", 30) 
  for (i in 1:3){
    m = JSS_tmean$JSS[,,issp,i]
    my.image(x=hvec, y=lfvec, z=m, col=cols, zlim=c(0,1), main=c("JSS (minimum)","JSS (arithmetic mean)","JSS (harmonic mean)")[i], 
             xlab="Harvest proportion", ylab="Minimum size limit (cm)")
    contour(x=hvec, y=lfvec, z=m, zlim=c(0,1), 
            levels = c(0.2,0.5,0.8,0.9), col=c("brown4", "red3", "pink", "white"), add=T)
    # points(x=0.5, y=45, pch=4, lwd=2, cex=1.5, col="white")
    m = oce::matrixSmooth(m)
    m = oce::matrixSmooth(m)
    idx = which(m == max(m, na.rm=T), arr.ind=T)
    points(x=hvec[idx[1,1]], y=lfvec[idx[1,2]], pch=4, lwd=2, cex=1.5, col="black")
  }
  color.bar(cols, 0,1, nticks = 5)
}
dev.off()


##### Plot JSS scenario x time ####

hvec = seq(0, 0.85, length.out = 20)
lfvec = seq(10, 80, length.out = 20)
tvec = c(1,2,3,4)

png(filename = paste0("../results_repoty/with_contour_jss_n2e6_ssp","all_hmeanxt_",str_replace_all(Sys.time(), ":", "."),".png"), width = 1000*3, height = 1100*3, res=300)
par(mfrow=c(4,4), mar=c(5,5,4,1), oma=c(1,1,1,1), cex.lab=1.25, cex.axis=1.2)

for (issp in c(1,2,3,4)){
  cols = scales::viridis_pal()(100)
  cols = RColorBrewer::brewer.pal(11, "Spectral")
  cols = viridis::inferno(100)
  cols = paletteer::paletteer_c("ggthemes::Sunset-Sunrise Diverging", 100) 
  cols = paletteer::paletteer_c("ggthemes::Red-Blue-White Diverging", 30) 
  for (iy in 1:3){
    m = JSS_t20$JSS[iy,,,issp,3]
    my.image(x=hvec, y=lfvec, z=m, col=cols, zlim=c(0,1), main=paste0("Year = ", 2016+10+c(1,30,60))[iy], 
             xlab="Harvest proportion", ylab="Minimum size limit (cm)")
    contour(x=hvec, y=lfvec, z=m, zlim=c(0,1), 
            levels = c(0.2,0.5,0.8,0.9), col=c("brown4", "red3", "pink", "white"), add=T)
    #points(x=0.5, y=45, pch=4, lwd=2, cex=1.5, col="white")
    m = oce::matrixSmooth(m)
    m = oce::matrixSmooth(m)
    idx = which(m == max(m, na.rm=T), arr.ind=T)
    points(x=hvec[idx[1,1]], y=lfvec[idx[1,2]], pch=4, lwd=2, cex=1.5, col="black")
  }
  color.bar(cols, 0,1, nticks = 5)
}
dev.off()


#### JSS and SOS timeseries ####

##### Calculate composite with 20 year means ####

dim_20_rolling = dim(res_ibm_composite)
dim_20_rolling[1] = dim_20_rolling[1] - 20 + 1
res_ibm_composite_20mean_rolling = array(data=NA, dim = dim_20_rolling)
for (iy in c(1:dim_20_rolling[1])){
  res_ibm_composite_20mean_rolling[iy,,,,] = apply(res_ibm_composite[iy:(iy+19),,,,], c(2,3,4,5), mean)
}  

nsteps_jss_rolling = dim_20_rolling[1]
tvec_rolling = 1:nsteps_jss_rolling

png(filename = paste0("../results_sysnthesis_paper/timeseries_20yrmeans_rolling_ssb_SQ_",str_replace_all(Sys.time(), ":", "."),".png"), width = 800*3, height = 800*3, res=300)
par(mfrow=c(2,2), mar=c(5,5,3,1), oma=c(1,1,1,1), cex.lab=1.2)
matplot(y=res_ibm_composite[10+tvec_rolling,10,10,,1]/1e9, x=2016+10+tvec_rolling, type="l", pch=1, lty=1, col = scales::viridis_pal()(4), ylab="SSB", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f\n", hvec[10], lfvec[10]))
matplot(y=res_ibm_composite_20mean_rolling[,10,10,,1]/1e9, x=2016+10+tvec_rolling, type="l", pch=1, lty=1, col = scales::viridis_pal()(4), ylab="SSB", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f\n(rollmean)", hvec[10], lfvec[10]))
matplot(y=res_ibm_composite[10+tvec_rolling,20,20,,1]/1e9, x=2016+10+tvec_rolling, type="l", pch=1, lty=1, col = scales::viridis_pal()(4), ylab="SSB", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f\n", hvec[20], lfvec[20]))
matplot(y=res_ibm_composite_20mean_rolling[,20,20,,1]/1e9, x=2016+10+tvec_rolling, type="l", pch=1, lty=1, col = scales::viridis_pal()(4), ylab="SSB", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f\n(rollmean)", hvec[20], lfvec[20]))
dev.off()

png(filename = paste0("../results_sysnthesis_paper/timeseries_20yrmeans_rolling_yield_SQ_",str_replace_all(Sys.time(), ":", "."),".png"), width = 800*3, height = 800*3, res=300)
par(mfrow=c(2,2), mar=c(5,5,3,1), oma=c(1,1,1,1), cex.lab=1.2)
matplot(y=res_ibm_composite[10+tvec_rolling,10,10,,2]/1e9, x=2016+10+tvec_rolling, type="l", pch=1, lty=1, col = scales::viridis_pal()(4), ylab="Yield", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f\n", hvec[10], lfvec[10]))
matplot(y=res_ibm_composite_20mean_rolling[,10,10,,2]/1e9, x=2016+10+tvec_rolling, type="l", pch=1, lty=1, col = scales::viridis_pal()(4), ylab="Yield", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f\n(rollmean)", hvec[10], lfvec[10]))
matplot(y=res_ibm_composite[10+tvec_rolling,20,20,,2]/1e9, x=2016+10+tvec_rolling, type="l", pch=1, lty=1, col = scales::viridis_pal()(4), ylab="Yield", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f\n", hvec[20], lfvec[20]))
matplot(y=res_ibm_composite_20mean_rolling[,20,20,,2]/1e9, x=2016+10+tvec_rolling, type="l", pch=1, lty=1, col = scales::viridis_pal()(4), ylab="Yield", xlab="Year",
        main=sprintf("h = %.2f, L = %.2f\n(rollmean)", hvec[20], lfvec[20]))
dev.off()


##### Calculate JSS timeseries ####

params_file = "../params/cod_params.ini"

fish = new(Fish, params_file)
sim = new(Simulator, fish)

JSS_t20_rolling = calc_jss_t(res_ibm_composite_20mean_rolling, nsteps_jss_rolling)


##### Plot JSS scenario x time ####

hvec = seq(0, 0.85, length.out = 20)
lfvec = seq(10, 80, length.out = 20)
tvec = c(1,2,3,4)

png(filename = paste0("../results_sysnthesis_paper/with_contour_jss_n2e6_ssp","all_hmeanxt_",str_replace_all(Sys.time(), ":", "."),".png"), width = 1000*6, height = 1100*3, res=300)
par(mfrow=c(4,6), mar=c(5,5,4,1), oma=c(1,1,1,1), cex.lab=1.25, cex.axis=1.2)
for (issp in c(1,2,3,4)){
  cols = scales::viridis_pal()(100)
  cols = RColorBrewer::brewer.pal(11, "Spectral")
  cols = viridis::inferno(100)
  cols = paletteer::paletteer_c("ggthemes::Sunset-Sunrise Diverging", 100) 
  cols = paletteer::paletteer_c("ggthemes::Red-Blue-White Diverging", 30) 
  for (iy in c(1,15,30,45,60)){
    m = JSS_t20_rolling$JSS[iy,,,issp,3]
    my.image(x=hvec, y=lfvec, z=m, col=cols, zlim=c(0,1), main=paste0("Year = ", 2016+10+tvec_rolling)[iy], 
             xlab="Harvest proportion", ylab="Minimum size limit (cm)")
    contour(x=hvec, y=lfvec, z=m, zlim=c(0,1), 
            levels = c(0.2,0.5,0.8,0.9), col=c("brown4", "red3", "pink", "white"), add=T)
    #points(x=0.5, y=45, pch=4, lwd=2, cex=1.5, col="white")
    m = oce::matrixSmooth(m)
    m = oce::matrixSmooth(m)
    idx = which(m == max(m, na.rm=T), arr.ind=T)
    points(x=hvec[idx[1,1]], y=lfvec[idx[1,2]], pch=4, lwd=2, cex=1.5, col="black")
  }
  color.bar(cols, 0,1, nticks = 5)
}
dev.off()

sos_size = apply(JSS_t20_rolling$JSS, MARGIN = c(1,4,5), FUN=function(x){length(which(x > 0.5))})
max_jss = apply(JSS_t20_rolling$JSS, MARGIN = c(1,4,5), FUN=function(x){max(x, na.rm = T)})

### T scenarios plot ###

temp_ssp1 = read.csv(paste0("../data/env_ssp",1,".csv"))
temp_ssp2 = read.csv(paste0("../data/env_ssp",2,".csv"))
temp_ssp3 = read.csv(paste0("../data/env_ssp",3,".csv"))
temp_ssp5 = read.csv(paste0("../data/env_ssp",5,".csv"))



png(filename = paste0("../results_sysnthesis_paper/","max_jss_sos_size_",str_replace_all(Sys.time(), ":", "."),".png"), width = 500*3, height = 500*3, res=300)
par(mfrow=c(2,2), mar=c(5,5,1,1), oma=c(1,1,1,1), cex.lab=1.2)
matplot(y=max_jss[,,3], x = 2016+10+tvec_rolling, type="l", lty=1, col = scales::viridis_pal()(4), ylab="Max JSS", xlab="Year")
matplot(y=sos_size[,,3]/sos_size[1,1,3], x = 2016+10+tvec_rolling, type="l", lty=1, col = scales::viridis_pal()(4), ylab="SOS size", xlab="Year")
matplot(x=temp_ssp1$year, 
        y=cbind(temp_ssp1$temp,
                temp_ssp2$temp,
                temp_ssp3$temp,
                temp_ssp5$temp),
        type="l", lty=1, col = scales::viridis_pal()(4), ylab="Temperature", xlab="Year")

plot(x=1,y=NA, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", frame.plot=F, xlab="", ylab="")
legend(x=0.25, y=1, legend=paste0("SSP", c(1,2,3,5)), col = scales::viridis_pal()(4), lty=1)
dev.off()

### Final data frame for submissin

jss_sos = data.frame(year = 2015+tvec_rolling, sos_size = (sos_size[,,3]/sos_size[1,1,3])[,2], max_jss = max_jss[,2,3])
ssb_y_sq = data.frame(year = 2016:2100, temp = temp_ssp2$temp, ssb=res_ibm_composite[,10,10,2,1]/1e9, yield = res_ibm_composite[,10,10,2,2]/1e9)
combined_df = ssb_y_sq %>% left_join(jss_sos)
combined_df %>% write.csv(file = paste0("../results_sysnthesis_paper/output_tseries_data.csv"))

# 
# ##### Plot from saved data ####
# 
# 
# setwd("~/codes/fisheRy/vignettes")
# 
# source("../tests/ref/parameters.cod.R")
# source("../tests/ref/simulator.7.R")
# 
# params_file = "../params/cod_params.ini"
# 
# fish = new(Fish, params_file)
# sim = new(Simulator, fish)
# 
# hvec = seq(0, 0.85, length.out = 20)
# lfvec = seq(10, 80, length.out = 20)
# tvec = c(5.61)
# 
# for (tf in c(2015, 2060)){
#   nsteps_jss=tf-2016+1
#   
#   png(filename = paste0("../results_repoty/with_contour_jss_n2e6_ssp","all","_tw",nsteps_jss,"_",str_replace_all(Sys.time(), ":", "."),".png"), width = 1000*3, height = 1200*3, res=300)
#   par(mfrow=c(4,4), mar=c(5,5,4,1), oma=c(1,1,1,1), cex.lab=1.25, cex.axis=1.2)
# 
#   for (ssp in c(1,2,3,5)){
#     saved_filename = paste0("res_ibm_hxL_T_ssp",ssp,"_evol_tf",2100, "n2e6.Rdata")
#     load(saved_filename)
#     
#     res_ibm_full_short = res_ibm_full[1:nsteps_jss,,,,,drop=F]
#     JSS = calc_jss(res_ibm_full_short, nsteps_jss)
#     
#     cols = scales::viridis_pal()(100)
#     cols = RColorBrewer::brewer.pal(11, "Spectral")
#     cols = viridis::inferno(100)
#     cols = paletteer::paletteer_c("ggthemes::Sunset-Sunrise Diverging", 100) 
#     cols = paletteer::paletteer_c("ggthemes::Red-Blue-White Diverging", 30) 
#     for (i in 1:3){
#       my.image(x=hvec, y=lfvec, z=JSS[,,1,i], col=cols, zlim=c(0,1), main=c("JSS (minimum)","JSS (arithmatic mean)","JSS (harmonic mean)")[i], 
#                xlab="Harvest proportion", ylab=expression("Minimum size limit (L"[50]*")"))
#       contour(x=hvec, y=lfvec, z=JSS[,,1,i], zlim=c(0,1), 
#                levels = c(0.2,0.5,0.8,0.9), col=c("brown4", "red3", "pink", "white"), add=T)
#       points(x=0.5, y=45, pch=4, lwd=2, cex=1.5, col="white")
#     }
#     color.bar(cols, 0,1, nticks = 5)
#   }
#   dev.off()
# }
# 


