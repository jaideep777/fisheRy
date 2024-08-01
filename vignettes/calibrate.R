library(fisheRy)
library(tidyverse)
library(here)

source(here("tests/ref/parameters.cod.R"))
source(here("tests/ref/simulator.7.R"))

datraw = read.csv(here("data/environmental.csv"))
dat = datraw %>% filter(year >= 2010 & year <= 2020)

read_age_dist = function(file){
  read.csv(file, header=T) %>% 
    filter(Year_age >= 2010 & Year_age <= 2020) %>% 
    pivot_longer(-Year_age) %>% 
    group_by(name) %>% 
    summarize(value = mean(value)) %>% 
    filter(grepl("X", name)) %>% 
    mutate(name = strsplit(name, "X")) %>% 
    unnest_wider(name, names_sep = "_") %>% 
    rename(age=name_2,
           value_obs = value) %>% 
    mutate(age = as.numeric(ifelse(age == ".gp", yes=15, no=age))) %>% 
    drop_na() %>% 
    select(-name_1) %>% 
    arrange(age)
}


N_v_age_obs = read_age_dist(here("data/new_calibration_files/Nstock-AFWG2024.csv")) %>% 
  mutate(value_obs = value_obs*1000) %>%  # convert thousands to actual number
  rename(N = value_obs)

W_v_age_obs = read_age_dist(here("data/new_calibration_files/Wstock-AFWG2024.csv")) %>% 
  rename(weight = value_obs)

M_v_age_obs = read_age_dist(here("data/new_calibration_files/ogive-AFWG2024.csv")) %>% 
  rename(mat = value_obs)

age_dist_obs = N_v_age_obs %>% 
  full_join(W_v_age_obs) %>% 
  full_join(M_v_age_obs)


plot_timeseries = function(res_ibm, pop, h, max_nx=100){
  res = simulate(h, lf, F)
  
  d = pop$get_state()
  table(d$age)
  par(mfrow = c(3,4), mar=c(4,4,1,1))
  
  nyears = nrow(res_ibm)
  dat_t = (nyears-nrow(dat)+1) : nyears
  nsteps = min(nrow(res_ibm), max_nx)
  dat_i = (nrow(res_ibm)-nsteps+1) : nrow(res_ibm)
  
  res_ibm = res_ibm[dat_i,]
  
  cols = c("darkgreen", "darkgoldenrod1", "dodgerblue3", "coral1")
  
  ssb.max = max(c(res_ibm$ssb/1e9, res$summaries$SSB/1e9, dat$ssb*1e3/1e9), na.rm = T)
  plot(y=res_ibm$ssb/1e9, x=dat_i, ylab="SSB (MT)", xlab="Year", col="cyan3", type="l", ylim=c(0,ssb.max))
  points(y=res$summaries$SSB/1e9, x=res$summaries$year, type="l")
  points(y=dat$ssb*1e3/1e9, x=dat_t, col=cols[1], type="o", lwd=0.4, pch=20)
  
  tsb.max = max(c(res_ibm$tsb/1e9, res$summaries$TSB/1e9, dat$totb*1e3/1e9), na.rm=T)
  plot(y=res_ibm$tsb/1e9, x=dat_i, ylab="TSB (MT)", xlab="Year", col="cyan3", type="l", ylim=c(0,tsb.max))
  points(y=res$summaries$TSB/1e9, x=res$summaries$year, type="l")
  points(y=dat$totb*1e3/1e9, x=dat_t, col=cols[2], type="o", lwd=0.4, pch=20)
  
  yield.max = max(c(res_ibm$yield/1e9, res$summaries$Y/1e9))
  plot(y=res_ibm$yield/1e9, x=dat_i, ylab="Yield (MT)", xlab="Year", col="cyan3", type="l", ylim=c(0,yield.max))
  points(y=res$summaries$Y/1e9, x=res$summaries$year, type="l")
  points(y=dat$catch*1e3/1e9, x=dat_t, col=cols[3], type="o", lwd=0.4, pch=20)
  
  nr.max = max(c(res_ibm$nfish_ra/1e6), dat$recr*1e3/1e6, na.rm=T)
  plot(y=res_ibm$nfish_ra/1e6, x=dat_i, ylab="Recruits (Mn)", xlab="Year", col="cyan3", type="l", ylim=c(0,nr.max))
  points(y=dat$recr*1e3/1e6, x=dat_t, col=cols[4], type="o", lwd=0.4, pch=20)
  
  d = pop$get_state()
  d1 = d %>% group_by(age) %>% summarize(mat = length(which(isMature))/length(isMature))
  plot(mat, type="l")
  points(d1$mat~I(d1$age-1), type="o", col="cyan3", xlab = "age") # Decrement age to get the right maturation prob (see note above)
  
  dist = table(d$age, d$length)
  image(x=as.numeric(rownames(dist)), y = as.numeric(colnames(dist)), z=log(1+3*log(dist)), col=scales::viridis_pal()(100), xlab="Age", ylab="Length", xlim=c(0,15))
  
  dist_age = table(d$age) %>% enframe() %>%
    mutate(age=as.numeric(name),
           value = value * pop$par$n) 
  
  dists_combined = dist_age %>% 
    left_join(age_dist_obs) %>% 
    filter(age >= 3) %>% 
    drop_na()
  
  dists_combined %>% 
    # mutate(value=value/sum(value, na.rm=T),
    #        value_obs=value_obs/sum(value_obs, na.rm=T)) %>%
    with(matplot(x=age, y=cbind(value, value_obs),type=c("l","o"), lty=1, pch=20, col=c("cyan3", "coral"), log="y", ylab="Frequency", xlab="Age"))
  
  emd_pred_obs = emdist::emd(
    A=cbind(dists_combined$value, dists_combined$age), 
    B=cbind(dists_combined$value_obs, dists_combined$age)
  )
  
  plot_calib(res_ibm, dat, nsteps)
  
}

plot_calib = function(res_ibm, dat, nsteps){
  obs = numeric(4)
  obs[1] = mean((dat$ssb*1e3/1e9), na.rm=T)  # MT
  obs[2] = mean((dat$totb*1e3/1e9), na.rm=T) # MT
  obs[3] = mean((dat$catch*1e3/1e9), na.rm=T) # MT
  obs[4] = mean((dat$recr*1e3/1e9), na.rm=T) # recruits in billions
  
  pred = numeric(4)
  ids = (nsteps-nrow(dat)+1):nsteps
  pred[1] = mean((res_ibm$ssb[ids]/1e9), na.rm=T)
  pred[2] = mean((res_ibm$tsb[ids]/1e9), na.rm=T)
  pred[3] = mean((res_ibm$yield[ids]/1e9), na.rm=T)
  pred[4] = mean((res_ibm$nfish_ra[ids]/1e9), na.rm=T)
  
  # par(mfrow=c(1,2), mar=c(4,4,4,1), oma=c(1,1,1,1))
  cols = c("darkgreen", "darkgoldenrod1", "dodgerblue3", "coral1")
  plot(obs~pred, ylim=c(0, max(c(obs,pred))), xlim=c(0, max(c(obs,pred))), col=cols, pch=20, cex=2, cex.lab=1.2, ylab="Observed", xlab="Predicted")
  abline(0,1, col="grey")
  plot(1,NA, cex=0.01, xlab = "", ylab = "", axes = F, ylim=c(0,1))
  legend(x = 0.6, y = 0.9, legend = c("Spawning stock biomass", "Total stock biomass", "Yield", "Recruitment"), col = cols, pch=20, cex=1.1)
}


simulate_pop = function(pop, par, nsup = 10e6, verbose=F, nymax=50, params_file){
  fish = new(Fish, params_file)
  fish$par$s0 = par[1] #0.07
  if (length(par) > 1){
    fish$setMortalityParams(par[2], par[3], par[4])
  }
  
  #fish$par$pmrn_lp50 = par[2] #118.122779*1.15
  # fish$par$M0 = par[2] #118.122779*1.15
  
  sim = new(Simulator, fish)
  
  sim$equilibriateNaturalPopulation(params_file, 5.61, nsup)
  
  pop = new(Population, fish)
  pop$readParams(params_file, F)
  pop$set_superFishSize(nsup)
  pop$verbose = verbose
  pop$init(1000, 5.61)
  pop$noFishingEquilibriate(5.61)
  
  nsteps = 500
  h = 0.22
  lf = 45
  
  pop$verbose = verbose
  res_ibm = sim$simulate(pop, lf, h, nsteps, 1.93e3, 5.61, F)
  
  list(d=pop$get_state(), res_ibm=res_ibm)
}

error_fun = function(par, nsup = 10e6, bplot=F, nymax=50){
  
  cat("par = ", par, "\n")


  obs = numeric(4)
  obs[1] = mean((dat$ssb*1e3/1e9), na.rm=T)  # MT
  obs[2] = mean((dat$totb*1e3/1e9), na.rm=T) # MT
  obs[3] = mean((dat$catch*1e3/1e9), na.rm=T) # MT
  obs[4] = mean((dat$recr*1e3/1e9), na.rm=T) # recruits in billions
  
  pred = numeric(4)
  ids = (nsteps-nrow(dat)+1):nsteps
  pred[1] = mean((res_ibm$ssb[ids]/1e9), na.rm=T)
  pred[2] = mean((res_ibm$tsb[ids]/1e9), na.rm=T)
  pred[3] = mean((res_ibm$yield[ids]/1e9), na.rm=T)
  pred[4] = mean((res_ibm$nfish_ra[ids]/1e9), na.rm=T)
  
  weights = c(1,0,1,1)
  err_partial = weights * log(pred/obs)^2
  err = sum(err_partial)
  
  nrep=5
  ids1 = (nsteps-nrow(dat)*nrep+1):nsteps
  pred1 = (res_ibm[ids1,] %>% select(ssb, tsb, yield, nfish_ra))/1e9
  obs1 = (dat %>% select(ssb, totb, catch, recr))[rep(seq(1,nrow(dat)),nrep),]*1e3/1e9
  
  err1 = sum((pred1-obs1)^2)
  
  cat("par = ", par, " | ", obs, " / ", pred, " | ", err_partial, " | ", err, " |", err1, "\n")
  
  if (bplot) {
    plot_timeseries(res_ibm, pop, h, nymax)
  }
  
  err
}


error_fun_emd = function(par, nsup = 10e6, bplot=F, nymax=50){
  
  cat("par = ", par, "\n")
  fish = new(Fish, params_file)
  fish$par$s0 = par[1] #0.07
  if (length(par) > 1){
    fish$setMortalityParams(par[2], par[3], par[4])
  }
  
  #fish$par$pmrn_lp50 = par[2] #118.122779*1.15
  # fish$par$M0 = par[2] #118.122779*1.15
  
  sim = new(Simulator, fish)
  
  sim$equilibriateNaturalPopulation(params_file, 5.61, nsup)
  
  pop = new(Population, fish)
  pop$readParams(params_file, F)
  pop$set_superFishSize(nsup)
  pop$verbose = F
  pop$init(1000, 5.61)
  pop$noFishingEquilibriate(5.61)
  
  nsteps = 500
  h = 0.22
  lf = 45
  
  pop$verbose = F
  res_ibm = sim$simulate(pop, lf, h, nsteps, 1.93e3, 5.61, F)
  
  d = pop$get_state()
  
  dist = table(d$age, d$length)
  
  dist_age = table(d$age) %>% enframe() %>%
    mutate(age=as.numeric(name),
           value = value * pop$par$n) 
  
  dists_combined = dist_age %>% 
    left_join(age_dist_obs) %>% 
    filter(age >= 3) %>% 
    drop_na()
  
  emd_pred_obs = emdist::emd(
    A=cbind(dists_combined$value, dists_combined$age), 
    B=cbind(dists_combined$value_obs, dists_combined$age)
  )
  
  if (bplot) {
    plot_timeseries(res_ibm, pop, h, nymax)
  }
  
  emd_pred_obs
}


l = simulate_pop(par = c(0.02, 0.0275, 0.05, 1), params_file= here("params/cod_params.ini")
, nsup=1e6, verbose=T)

p1 = l$d %>% 
  group_by(age) %>%
  summarize(weight=mean(weight),
            mat=mean(isMature),
            N = log10(n()*1e6)) %>%
  pivot_longer(-age, values_to="pred") %>% 
  left_join(age_dist_obs %>% 
              mutate(N = log10(N)) %>% 
              pivot_longer(-age, values_to="obs")) %>% 
  ggplot(aes(x=age)) +
  geom_line(aes(y=pred, col="pred"), linewidth=1)+
  geom_point(aes(y=obs, col="obs"), shape=1, size=2, stroke=1)+
  facet_wrap(~name, scales="free_y", strip.position = "left")+
  scale_color_manual(values = c(obs="black", pred="cyan3"))+
  theme_bw()+
  theme(strip.placement = "outside",
        strip.background = element_blank())+
  labs(y="")

p2 = l$res_ibm %>% 
  mutate(recruits = nfish_ra) %>% 
  select(ssb, tsb, yield, recruits) %>% 
  tail(nrow(dat)) %>% 
  colMeans() %>% 
  enframe(value="pred") %>% 
  mutate(pred = pred/1e9) %>% 
  left_join(
    dat %>% select(ssb, totb, catch, recr) %>% 
      rename(tsb=totb, yield=catch, recruits=recr) %>% 
      colMeans() %>% 
      enframe(value = "obs") %>% 
      mutate(obs= obs*1000/1e9)
    ) %>% 
  ggplot(aes(y=obs, x=pred, col=name))+
  geom_point(size=2)+
  scale_colour_manual(values = 
    c(ssb="darkgreen", 
      tsb="darkgoldenrod1",
      yield="dodgerblue3", 
      recruits="coral1")
  )+
  geom_abline(slope=1, intercept = 0, col="grey")+
  expand_limits(y=0, x=0)+
  theme_bw()

library(patchwork)
p1/p2 + plot_layout(guides="collect", widths = c(4,1))

dist_age = table(l$d$age) %>% enframe() %>%
  mutate(age=as.numeric(name),
         value = value * pop$par$n) 

dists_combined = dist_age %>% 
  left_join(age_dist_obs) %>% 
  filter(age >= 3) %>% 
  drop_na()

dists_combined %>% 
  # mutate(value=value/sum(value, na.rm=T),
  #        value_obs=value_obs/sum(value_obs, na.rm=T)) %>%
  with(matplot(x=age, y=cbind(value, N),type=c("l","o"), lty=1, pch=20, col=c("cyan3", "coral"), log="y", ylab="Frequency", xlab="Age"))


plot_timeseries(l$res_ibm, l$pop, h=0.22)

par = c(0.02, 0.025, 0.1, 1)
error_fun_emd(par, ns=1e6, bplot=T)

