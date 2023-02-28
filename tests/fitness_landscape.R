library(fisheRy)
setwd("~/codes/fisheRy")

params_file = "params/cod_params.ini"

calc_fitness = function(pmrn_int, 
             ssb = 2.3692e9,
             tsb = 2.5136e9,
             temp = 5.61,
             N = 100,
             h = 0.5,
             plot = F){

  fitness = numeric(N)
  for (k in 1:N){
    fish = new(Fish, params_file)
    fish$par$pmrn_intercept = pmrn_int
    fish$init(0, 5.61)
    
    sf = 0.1222
    lf50 = 45
    
    am = 29
    years = 1:am
    length = numeric(am)
    M = numeric(am)
    R = numeric(am)
    for (i in years){
      length[i] = fish$length
      fish$updateMaturity(temp)
      fish$grow(tsb/1e6, temp)
      fish$set_age(fish$age+1)
      M[i] = fish$naturalMortalityRate(temp) + -log(1-h)*fishing_selectivity(fish$length, sf, lf50)
      R[i] = fish$produceRecruits(ssb, temp)
    }
    survival_prob = exp(-cumsum(M))
    
    if (plot){
      par(mfrow=c(3,3), mar=c(4,4,1,1), oma=c(1,1,1,1), cex.axis=1.3, cex.lab=1.3)
      plot(length~years, xlab="Age", ylab="Length", type="l", lwd=2)
      plot(M~years, xlab="Age", ylab="Natural mortality rate", type="l", lwd=2)
      plot(cumsum(M)~years, xlab="Age", ylab="Cumm mortality", type="l", lwd=2)
      plot(survival_prob~years, xlab="Age", ylab="Survival Prob", type="l", lwd=2)
      plot(R~years, xlab="Age", ylab="Recruits", type="l", lwd=2)
      plot(cumsum(R*survival_prob)~years, xlab="Age", ylab="Cumm Fitness", type="l", lwd=2)
    }
    
    fitness[k] = cumsum(R*survival_prob)[am]
  }
  
  mean(fitness)
}


png(filename = paste0("results_repoty/fitness_landscape_h_",str_replace_all(Sys.time(), ":", "."),".png"), width = 400*3, height = 600*3, res=300)
par(mfrow=c(3,2), mar=c(4,4,3,1), oma=c(1,1,1,1), cex.axis=1.3, cex.lab=1.3)
pmrn_int_vec = seq(0,150, 10)
for (h in seq(0.1, 0.6, 0.1)){
  fitness = sapply(X = pmrn_int_vec, FUN=calc_fitness, h=h, plot = F)
  plot(fitness~pmrn_int_vec, main=paste("h = ", h), type="l", col="cyan3", lwd=2,
       xlab = "PMRN intercept", ylab = "Fitness")
}
dev.off()

par(mfrow=c(3,3), mar=c(4,4,1,1), oma=c(1,1,1,1), cex.axis=1.3, cex.lab=1.3)
calc_fitness(pmrn_int = 100, h = 0.5, plot=T)
calc_fitness(pmrn_int = 0, h = 0.5, plot=T)

calc_fitness(pmrn_int = 100, h = 0, plot=T)
calc_fitness(pmrn_int = 0, h = 0, plot=T)

