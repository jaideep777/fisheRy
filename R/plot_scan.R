plot_scan = function(dat_ibm, xname, xvec, dat_ref = NULL){
  par(mfrow = c(4,3), mar=c(7,7,1,1), oma=c(1,1,1,1), cex.lab=1.3, cex.axis=1.3)
  
  yvec = cbind(dat_ibm$ssb)/1e9
  if(!is.null(dat_ref)) yvec = cbind(yvec, dat_ref$ssb/1e9)
  matplot(ylab="Spawning stock\nbiomass (Mt)",y=yvec, x=xvec, type="l", col=c("cyan2", "black"), lwd=2:1, lty=1, xlab = xname)
  
  yvec = cbind(dat_ibm$tsb)/1e9
  if(!is.null(dat_ref)) yvec = cbind(yvec, dat_ref$tsb/1e9)
  matplot(ylab="Total stock\nbiomass (Mt)",y=yvec, x=xvec, type="l", col=c("cyan2", "black"), lwd=2:1, lty=1, xlab = xname)
  
  yvec = cbind(dat_ibm$yield)/1e9
  if(!is.null(dat_ref)) yvec = cbind(yvec, dat_ref$yield/1e9)
  matplot(ylab="Yield (Mt)",y=yvec, x=xvec, type="l", col=c("cyan2", "black"), lwd=2:1, lty=1, xlab = xname)
  
  yvec = cbind(dat_ibm$employment.sea)
  if(!is.null(dat_ref)) yvec = cbind(yvec, dat_ref$employment.sea)
  matplot(ylab="Employment at\nsea (person-yrs)",y=yvec, x=xvec, type="l", col=c("cyan2", "black"), lwd=2:1, lty=1, xlab = xname)
  
  yvec = cbind(dat_ibm$employment.shore)
  if(!is.null(dat_ref)) yvec = cbind(yvec, dat_ref$employment.shore)
  matplot(ylab="Employment on\nshore (person-yrs)",y=yvec, x=xvec, type="l", col=c("cyan2", "black"), lwd=2:1, lty=1, xlab = xname)
  
  yvec = cbind(dat_ibm$profit.sea)/1e9
  if(!is.null(dat_ref)) yvec = cbind(yvec, dat_ref$profit.sea/1e9)
  matplot(ylab=expression(atop("Profit at", "sea (10"^9*" NOK)")),y=yvec, x=xvec, type="l", col=c("cyan2", "black"), lwd=2:1, lty=1, xlab = xname)
  
  yvec = cbind(dat_ibm$profit.shore)/1e9
  if(!is.null(dat_ref)) yvec = cbind(yvec, dat_ref$profit.shore/1e9)
  matplot(ylab=expression(atop("Profit on", "shore (10"^9*" NOK)")),y=yvec, x=xvec, type="l", col=c("cyan2", "black"), lwd=2:1, lty=1, xlab = xname)
  
  matplot(ylab="Length %iles",y=cbind(dat_ibm$max_length, dat_ibm$length90), x=xvec, type="l", col=c("cyan2", "cyan4"), lwd=1, lty=1, xlab = xname)
  matplot(ylab="dl ratio,\nr ratio",y=cbind(dat_ibm$factor_dg, dat_ibm$factor_dr), x=xvec, type="l", col=c("cyan3", "magenta3"), lwd=1, lty=1, xlab = xname)
  matplot(ylab="Superfish",y=cbind(dat_ibm$nsuperfish), x=xvec, type="l", col=c("cyan2", "cyan4"), lwd=1, lty=1, xlab = xname)
  matplot(ylab="Survival prob,\nMaturity",y=cbind(dat_ibm$survival_mean, dat_ibm$maturity), x=xvec, type="l", col=c("cyan3", "magenta3"), lwd=1, lty=1, xlab = xname)
  matplot(ylab="Nrel_sea",y=cbind(dat_ibm$Nrel_sea), x=xvec, type="l", col=c("cyan3", "magenta3", "yellow3"), lwd=1, lty=1, xlab = xname)
}
