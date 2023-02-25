library(fisheRy)

setwd("~/codes/fisheRy/vignettes/")
source("../tests/ref/parameters.cod.R")
source("../tests/ref/simulator.7.R")
params_file = "../params/cod_params.ini"


#### Status quo ####
# setwd("vignettes/")
fish = new(Fish, params_file)
fish$trait_variances = c(1, 1, 1, 1, 1, 0)*(0.05)^2

pop = new(Population, fish)  
pop$par$n = 4e6  # Each superfish contains so many fish
pop$par$f_harvest_spg = 0.20
pop$par$simulate_bio_only = T
pop$set_harvestProp(0)
pop$init(1000, 1.93e3, 5.61)  # initialize the population with 1000 agents (superfish)

pop$verbose = T
nsteps = 20000             # Let's simulate for 200 years
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

par(mfrow = c(3,1), mar=c(5,5,1,1), oma=c(1,1,1,1), cex.lab=1.5, cex.axis=1.5)
plot(nfish~seq(1,nsteps,1), ylab="No. of superfish", xlab="Year")
image(x=as.numeric(rownames(dist)), y = as.numeric(colnames(dist)), z=log(1+3*log(dist)), col=scales::viridis_pal()(100), xlab="Age", ylab="Length")

res = simulate(0, 45, F)
matplot(cbind(dat$ssb/1e9), ylab="SSB (MT)", xlab="Year", col=c("cyan4", "black"), lty=1, type=c("p","l"), pch=1)

png(filename = "../results_repoty/trait_evol3.png", width = 500*3, height = 500*3, res=300)
par(mfrow = c(3,2), mar=c(4,4,1,1), oma=c(1,1,1,1), cex.lab=1.3, cex.axis=1.2)
for (it in 1:6){
  image(y=(trait_breaks[-1]-diff(trait_breaks)/2)*abs(trait_scalars[it]), x=seq(1,nsteps,skip), z=log(1e-4+trait_dist[it,,]), col=scales::viridis_pal()(100), xlab="Years", ylab=trait_names[it], useRaster = T) 
  lines(y=trait_means[it,], x=1:nsteps, col="white", lwd=2)
}
dev.off()
