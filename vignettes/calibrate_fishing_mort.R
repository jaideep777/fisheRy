library(tidyverse)
library(fisheRy)

params_file = here::here("params/cod_params.ini")
fleet_params_file = here::here("params/fleet_1_params.ini")

## Get growth trajectories
growth_trajectories = function(beta1=NULL, beta2=NULL, N=50, temp_sd = 3, tsb_min = 0, tsb_max=1.93e3, growth_noise_sd = NULL){
  names = c("t_birth",	"age",	"isMature",	"isAlive",	"length",	"weight", "mort", "temp", "fec", "ssb")
  dat_full = data.frame(data=matrix(nrow=0, ncol=length(names)))
  colnames(dat_full) = names
  plot(x=1, y=NA, xlim=c(0,31), ylim=c(0,300), xlab="Age", ylab = "Length")
  for (f in 1:N){
    fish = new(Fish, params_file)
    fish$setMortalityCurveEmpirical(here::here("data/naturalmort.spline.csv"))
    if (!is.null(growth_noise_sd)) fish$par$growth_noise_sd = growth_noise_sd
    if (!is.null(beta1)) fish$par$beta1 = beta1
    if (!is.null(beta2)) fish$par$beta2 = beta2
    
    dat = data.frame(data=matrix(nrow=0, ncol=length(names)))
    colnames(dat) = names
    
    temp0 = rnorm(1, mean=5.61, sd=temp_sd)
    tsb0 = runif(1, min = tsb_min, max = tsb_max)
    fish$init(tsb0, temp0)
    dat[1,] = c(fish$get_state(), fish$naturalMortalityRate(5.61), 5.61, 0, 0.8*tsb0) # Assuming SSB is 80% of TSB, since we are not simulating a population
    
    for (i in 2:30){
      temp = rnorm(1, mean=5.61, sd=temp_sd)
      tsb = runif(1, min = tsb_min, max = tsb_max)
      fish$updateMaturity(temp)
      fish$grow(tsb, temp)
      fish$set_age(fish$age+1)
      dat[i,] = c(fish$get_state(), fish$naturalMortalityRate(temp), temp, fish$produceRecruits(0.8*tsb*1e6, temp), 0.8*tsb) # Assuming SSB is 80% of TSB, since we are not simulating a population
    }
    points(dat$length~dat$age, type="l", col=scales::alpha(rainbow(N)[f], 0.3))
    dat_full = rbind(dat_full, dat)
  }
  dat_full
}

dat_full3 = growth_trajectories(N=50, temp_sd=0, growth_noise_sd = 0.2347)

dat_full3 |>
  filter(age <= 15) |>
  ggplot(aes(x=length, group=age, color=age))+
  geom_density()

length_chars_by_age = dat_full3 |>
  filter(age <= 15) |>
  group_by(age) |> 
  summarize(mean_l = mean(length),
            sd_l = sd(length))

get_weights = function(nl){
  d = expand_grid(age=3:14, length=seq(9,160,length.out=nl)) |>
    left_join(length_chars_by_age) |> 
    rowwise() |> 
    mutate(weight = purrr::pmap_dbl(list(x=length, mean=mean_l, sd=sd_l), ~dnorm(x=..1, mean=..2, sd=..3)))
    
  wsum = d |>   
    group_by(age) |> 
    summarize(wsum = sum(weight))
  
  w = d |> 
    left_join(wsum) |> 
    mutate(weight_norm = weight/(wsum+1e-12)) 
    
  w |> 
    group_by(age) |> 
    summarize(wsum = sum(weight), wnorm_sum = sum(weight_norm))

  w  
}

# Visualize weights mat with finer length resolution
get_weights(100) |>  
  ggplot(aes(x=length, y=weight_norm, group=age, col=age))+
  geom_line()

get_weights(100) |>  
  ggplot(aes(y=length, x=age, fill=weight_norm))+
  geom_raster()+
  ylim(c(0,160))

# Visualize weights at normal resolution
get_weights(12) |>  
  ggplot(aes(y=length, x=age, fill=weight_norm))+
  geom_raster()+
  ylim(c(0,160))

  
weights_mat = get_weights(12) |>  
  select(age, length, weight_norm) |> 
  pivot_wider(names_from=length, values_from=weight_norm) |> 
  select(-age) |> 
  as.matrix()


### BASED ON LINEAR SYSTEM

F_age1 = read.csv(here::here("data/new_calibration_files/F-AFWG2024.csv")) |> 
  select(X3:X14) |>
  colMeans()

F_age2 = read.csv(here::here("data/F.at.age.csv")) |> 
  filter(age >=3 & age <=14) |> 
  pull(F)

tibble(F_age1, F_age2) |> 
  ggplot(aes(x=3:14)) + 
  geom_line(aes(y=F_age1), col="grey40")+
  geom_line(aes(y=F_age2))
  

F_length1 = solve(weights_mat, F_age1)

read.csv(here::here("data/selection.spline.reduced.csv")) |> 
  ggplot() +
  geom_line(aes(y=F, x=length)) + 
  geom_point(data=tibble(f=F_length1, length=as.numeric(names(F_length1))) |> 
               filter(length > 40 & length < 140),
             aes(x=length, y=f),
             col="cyan3")

F_length2 = solve(weights_mat, F_age2)

read.csv(here::here("data/selection.spline.reduced.csv")) |> 
  ggplot() +
  geom_line(aes(y=F, x=length)) + 
  geom_point(data=tibble(f=F_length2, length=as.numeric(names(F_length2))) |> 
               filter(length > 40 & length < 140),
             aes(x=length, y=f),
             col="cyan3")


  

tibble(f=F_length, length=as.numeric(names(F_length))) |> 
  filter(length > 40 & length < 140) |> 
  with(points(f~length, col="red"))
  

### BASED ON REGRESSION

F_age_df = read.csv(here::here("data/new_calibration_files/F-AFWG2024.csv")) |> 
  select(Year_age, X3:X14) |>
  pivot_longer(cols = starts_with("X"),
               names_to = "age",
               values_to = "F") |>
  mutate(age = as.numeric(gsub("X","", age)))

weights_long = get_weights(12)

design_df = F_age_df |>
  inner_join(weights_long, by = "age")

design_wide = design_df |>
  mutate(length = paste0("L", length)) |>
  select(Year_age, age, "F", length, weight_norm) |>
  pivot_wider(names_from = length, values_from = weight_norm) 

X = as.matrix(design_wide |> select(starts_with("L")))
y = design_wide$F

fit = lm(y ~ X)

F_length = coef(fit)
length_Flength = as.numeric(gsub(pattern = "XL",replacement = "", x = names(coef(fit))))

read.csv(here::here("data/selection.spline.reduced.csv")) |> 
  ggplot() +
  geom_line(aes(y=F, x=length)) + 
  geom_point(data=tibble(f=F_length, length=length_Flength) |> 
               filter(length > 40 & length < 140),
             aes(x=length, y=f),
             col="red")


