params_file = "params/cod_params.ini"

load("vignettes/res_ibm_hxL_T5.61_evol_n50.Rdata")
res_in_full_base = res_ibm_full
load("vignettes/res_ibm_hxL_T5.61_evol_n100.Rdata")
res_in_full_T = res_ibm_full

fish = new(Fish, params_file)
sim = new(Simulator, fish)

hvec = seq(0, 0.8, length.out = 20)
lfvec = seq(10, 100, length.out = 20)
tvec = c(7.61)

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

JSS_base = calc_jss(res_in_full_base, 50)
JSS_T = calc_jss(res_in_full_T, 100)

JSS_diff = JSS_T - JSS_base
  

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


par(mfrow=c(2,2), mar=c(5,5,4,1), oma=c(1,1,1,1), cex.lab=1.2)
cols = scales::colour_ramp(colors = c("red", "white", "blue"))(seq(0,1,length.out=100))
for (i in 1:3){
  my.image(x=hvec, y=lfvec, z=JSS_diff[,,1,i], col=cols, zlim=c(-.3,.3), main=c("JSS (arithmatic mean)","JSS (harmonic mean)","JSS (minimum)")[i], 
           xlab="Harvest proportion", ylab=expression("Minimum size limit (L"[50]*")"))
}
color.bar(cols, -.3,.3, nticks = 5)
mtext(line=1, side=1, text = "JSS(N=100) - JSS(N=50)")
mtext(line=2.5, side=1, text = "T = 5.61")
