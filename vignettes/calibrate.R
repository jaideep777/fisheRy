library(fisheRy)
library(tidyverse)
library(here)

source(here("tests/ref/parameters.cod.R"))
source(here("tests/ref/simulator.7.R"))

#### Read observations data #-------------------

datraw = read.csv(here("data/environmental.csv"))
dat = datraw %>% filter(year >= 2010 & year <= 2020)

read_age_dist = function(file){
  read.csv(file, header=T) %>% 
    filter(Year_age >= 2010 & Year_age <= 2020) %>% 
    pivot_longer(-Year_age) %>% 
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

cN_v_age_obs = read_age_dist(here("data/new_calibration_files/Ncatch-AFWG2024.csv")) %>% 
  mutate(value_obs = value_obs*1000) %>%  # convert thousands to actual number
  rename(catch_N = value_obs)

cW_v_age_obs = read_age_dist(here("data/new_calibration_files/Wcatch-AFWG2024.csv")) %>% 
  rename(catch_weight = value_obs)

age_dist_obs = N_v_age_obs %>% 
  full_join(W_v_age_obs) %>% 
  full_join(M_v_age_obs) %>% 
  full_join(cN_v_age_obs) %>% 
  full_join(cW_v_age_obs)
  

#### Function to simulate population given parameters vector #-------------------
# params_file_fish = here("params/cod_params.ini")

simulate_pop = function(par, nsup = 1e6, verbose=F, nsteps=500, nymax=50, params_file_fish, params_file_fleet, out_file = "", using_empirical_mort = TRUE){
  h = 0.22
  lf = 45
  
  fish = new(Fish, params_file_fish)
  
  fish$par$s0 = par[1] #0.07
  if (length(par) > 1){
    if (using_empirical_mort){
      fish$natural_mort_scalar = par[2]  # Can be set from par 
      fish$setMortalityCurveEmpirical(here::here("data/naturalmort.spline.csv"))
      fish$setMortalityParams(0, par[3], 0)
    } else {
      fish$setMortalityParams(par[2], par[3], par[4])
    }
  }
    
  fleet = new(Fleet)
  fleet$readParams(params_file_fleet, FALSE)
  # fleet$par$print();
  fleet$debug = FALSE
  cat(">>> Fleet initialized --------------\n")

  fishery = new(Fishery, params_file_fish, fish);
  fishery$debug = FALSE
  # fishery$par$print();
  cat(">>> Fishery Created --------------\n")

  fishery$set_harvestProp(h)
  fishery$set_minSizeLimit(lf)
  fishery$addFleet(params_file_fleet, FALSE);
  fishery$pop$debug = FALSE
  cat(">>> Fleet added to fishery --------------\n")

  v = fishery$equilibriateNaturalPopulation(5.61, 2e6, 200);

  fishery$init(1000, 0, 5.61);
  cat(">>> Fishery init with 1000 fish --------------\n")
  v2 = fishery$equilibriateWithoutFishing(5.61, 200)
  cat(">>> Stock equilibriated --------------\n")
  # print(fishery$pop$get_state())

  res_ibm <- fishery$simulate(lf, h, nsteps, 1.93e3, 5.61, F, out_file)
  cat(">>> Stock Simulated with fishing --------------\n")
  res_ibm |> 
    mutate(t = 1:n()) |>
    ggplot(aes(x=t, y=ssb/1e9)) +
    geom_line()
  
  list(d=fishery$pop$get_state(), res_ibm=res_ibm)
}

#### Error function using sum-sqaured Earth-mover distances ###-------------------

error_fun_emd = function(par, nsteps = 200, nsup = 5e6, bplot=F, nymax=50){
  
  cat("par = ", par, "\n")

  l = simulate_pop(par = par, 
               params_file= here("params/cod_params.ini"), 
               nsup=nsup, 
               nsteps=nsteps,
               verbose=F, 
               out_file = here("fishery_output/age_dists_pred.csv"))
  
  age_dists_pred = readr::read_csv(here("fishery_output/age_dists_pred.csv"), progress = F, show_col_types = FALSE)

  dists_pred_obs = 
    suppressMessages(
      age_dists_pred %>% filter(Year > max(Year)-50) %>%
      pivot_longer(-c(age, Year)) %>%
      # Filter out non-existent age classes in avergaed quantities, but retain everything in summed quantities (N and catch_N)
      filter(case_when(
        (name == "N" | name == "catch_N") ~ value > -Inf,
        .default = value > 0
      )) %>%
      filter(age > 0 & age < 20) %>%
      group_by(age, name) %>%
      summarize(value = mean(value)) %>%
      ungroup() %>% 
      pivot_wider() %>%
      mutate(N = log10(N),
             catch_N = log10(catch_N)) %>%
      pivot_longer(-age, values_to="pred") %>%
      full_join(age_dist_obs %>%
                  mutate(N = log10(N),
                         catch_N = log10(catch_N)) %>%
                  pivot_longer(-c(age, Year_age), values_to="obs")) %>%
      drop_na() %>%
      filter(!is.infinite(obs)) %>%
      filter(!is.infinite(pred)) %>%
      ungroup()
    )
  
  df_emd = tibble(name = unique(dists_pred_obs$name)) %>%
    mutate(emd = purrr::map_dbl(
      .x = name,
      .f = ~emdist::emd2d(
              A=dists_pred_obs %>% filter(name == .x) %>% select(age, Year_age, pred) %>% pivot_wider(names_from=Year_age, values_from = pred) %>% select(-age) %>% as.matrix(),
              B=dists_pred_obs %>% filter(name == .x) %>% select(age, Year_age, obs)  %>% pivot_wider(names_from=Year_age, values_from = obs)  %>% select(-age) %>% as.matrix()
            )
        )
      )

  sum(df_emd$emd^2)
}

#### Test and calibrate ## -----------------------
# error_fun_emd(par = c(0.02, 0.0275, 0.06, 1), 
#               nsup = 5e6, nsteps=200)

# opt = optim(par = c(0.02, 0.06, 0.16, 2.45),
#             fn = error_fun_emd, 
#             nsup = 5e6, nsteps=200,
#             control=list(parscale=c(0.02,0.05,0.1,2), 
#                          maxit=500)) #, method = "Brent", lower=0.00000001, upper=0.2)
# print(opt)
# par_opt = opt$par

##### Run and plot @@@ ---------------------------
setwd(here("vignettes"))

fmort = read.csv(here::here("data/selection.spline.reduced.csv"))

# par_opt = c(0.02, 0.0275, 0.06, 1)
par_opt = c(0.02924969, 0.03239047, 0.17911014, 1.7)
par_opt_empirical = c(0.01924969, 0.2, 0.25)
# par_opt = c(0.01924969, 0.062994, 0.07911014, 2.455715)
l = simulate_pop(par = par_opt_empirical, 
                 params_file_fish = here("params/cod_params.ini"), 
                 params_file_fleet = here("params/fleet_1_params.ini"), 
                 nsup=1e6, 
                 nsteps=200,
                 verbose=T, 
                 out_file = here("fishery_output/age_dists_pred.csv"),
                 using_empirical_mort = TRUE
                 )

l$d |> 
  ggplot(aes(x=age, y=length)) +
  geom_point() + 
  geom_point(data=fmort, aes(x=F*10, y=length), col="red")

pt <- l$res_ibm |>
  mutate(t=1:n()) |>
  pivot_longer(-t) |>
  ggplot(aes(x=t, y=value)) +
  geom_line(col="seagreen") +
  facet_wrap(~name, scales="free_y")

print(pt)

age_dists_pred = readr::read_csv(here("fishery_output/age_dists_pred.csv")) 

# age_dists_pred |> 
#   select(age, len) |>
#   filter(age == 6) 

pa = age_dists_pred %>% filter(Year > max(Year)-50) %>% 
  pivot_longer(-c(age, Year)) %>% 
  # Filter out non-existent age classes in avergaed quantities, but retain everything in summed quantities (N and catch_N)
  filter(case_when(
    (name == "N" | name == "catch_N") ~ value > -Inf,
    .default = value > 0
  )) %>% 
  filter(age > 0 & age < 20) %>% 
  group_by(age, name) %>% 
  summarize(value = mean(value)) %>% 
  pivot_wider() %>% 
  # mutate(N = log10(N),
  #        catch_N = log10(catch_N)) %>% 
  pivot_longer(-age, values_to="pred") %>%  
  full_join(age_dist_obs %>% 
              # mutate(N = log10(N),
              #        catch_N = log10(catch_N)) %>% 
              pivot_longer(-c(age, Year_age), values_to="obs")) %>% 
  drop_na() %>% 
  ggplot(aes(x=age)) +
  geom_point(aes(y=obs, col="obs"), alpha=0.5)+
  geom_line(aes(y=pred, col="pred"), linewidth=1)+
  facet_wrap(~name, scales="free_y", strip.position = "left", nrow=1)+
  scale_color_manual(values = c(obs="seagreen", pred="black"))+
  theme_bw()+
  theme(strip.placement = "outside",
        strip.background = element_blank())+
  labs(y="")

# cairo_pdf(here::here("fishery_output/age_dists_calibrated_params.pdf"), width = 10, height=5)
print(pa)
# dev.off()

# p1 = l$d %>% 
#   group_by(age) %>%
#   summarize(weight=mean(weight),
#             mat=mean(isMature),
#             N = log10(n()*1e6)) %>%
#   pivot_longer(-age, values_to="pred") %>% 
#   left_join(age_dist_obs %>% 
#               mutate(N = log10(N)) %>% 
#               pivot_longer(-age, values_to="obs")) %>% 
#   ggplot(aes(x=age)) +
#   geom_line(aes(y=pred, col="pred"), linewidth=1)+
#   geom_point(aes(y=obs, col="obs"), shape=1, size=2, stroke=1)+
#   facet_wrap(~name, scales="free_y", strip.position = "left", nrow=1)+
#   scale_color_manual(values = c(obs="black", pred="cyan3"))+
#   theme_bw()+
#   theme(strip.placement = "outside",
#         strip.background = element_blank())+
#   labs(y="")

# p2 = l$res_ibm %>% 
#   # mutate(recruits = nfish_ra) %>% 
#   select(ssb, tsb, yield) %>% #, recruits) %>% 
#   tail(nrow(dat)) %>% 
#   colMeans() %>% 
#   enframe(value="pred") %>% 
#   mutate(pred = pred/1e9) %>% 
#   left_join(
#     dat %>% select(ssb, totb, catch, recr) %>% 
#       rename(tsb=totb, yield=catch, recruits=recr) %>% 
#       colMeans() %>% 
#       enframe(value = "obs") %>% 
#       mutate(obs= obs*1000/1e9)
#     ) %>% 
#   ggplot(aes(y=obs, x=pred, col=name))+
#   geom_point(size=2)+
#   scale_colour_manual(values = 
#     c(ssb="darkgreen", 
#       tsb="darkgoldenrod1",
#       yield="dodgerblue3", 
#       recruits="coral1")
#   )+
#   geom_abline(slope=1, intercept = 0, col="grey")+
#   expand_limits(y=0, x=0)+
#   theme_bw()
# p2

# p2all = l$res_ibm %>% 
#   mutate(recruits = nfish_ra) %>% 
#   select(ssb, tsb, yield, recruits) %>% 
#   tail(nrow(dat)) %>% 
#   mutate(year = dat$year) %>% 
#   # colMeans() %>% 
#   pivot_longer(-year, values_to="pred") %>% 
#   mutate(pred = pred/1e9) %>% 
#   left_join(
#     dat %>% select(year, ssb, totb, catch, recr) %>% 
#       rename(tsb=totb, yield=catch, recruits=recr) %>% 
#       pivot_longer(-year, values_to="obs") %>% 
#       mutate(obs= obs*1000/1e9)
#   ) %>% 
#   ggplot(aes(y=obs, x=pred, col=name))+
#   geom_point(size=2)+
#   scale_colour_manual(values = 
#                         c(ssb="darkgreen", 
#                           tsb="darkgoldenrod1",
#                           yield="dodgerblue3", 
#                           recruits="coral1")
#   )+
#   geom_abline(slope=1, intercept = 0, col="grey")+
#   expand_limits(y=0, x=0)+
#   theme_bw()

# # Timeseries plots
# p3_ts = l$res_ibm %>% 
#   mutate(ssb=ssb/1e9,
#          tsb=tsb/1e9, 
#          yield=yield/1e9,
#          quota_fgf=quota_fgf/1e9) %>% 
#   mutate(Year=1:n()) %>% 
#   pivot_longer(-Year) %>% 
#   ggplot(aes(y=value, x=Year, col=name))+
#   geom_line()+
#   facet_wrap(~name, scales="free_y", strip.position="left", nrow=1)+
#   scale_x_continuous(n.breaks = 3)+
#   theme_bw()+
#   theme(strip.placement = "outside",
#       strip.background = element_blank())+
#   labs(y="")

# cairo_pdf(here::here("fishery_output/timeseries_calibrated_params.pdf"), width = 10, height=3)
# p3_ts
# dev.off()

# p3 = l$res_ibm %>% select(ssb:profit) %>% 
#   mutate(ssb=ssb/1e9, 
#          yield=yield/1e9,
#          profit=profit/1e9,
#          employment=employment/1000) %>% 
#   mutate(Year=1:n()) %>% 
#   pivot_longer(-Year) %>% 
#   ggplot(aes(y=value, x=Year, col=name))+
#   geom_line()+
#   facet_wrap(~name, scales="free_y", strip.position="left", nrow=1)+
#   scale_colour_manual(values = 
#                         c(ssb="darkgreen", 
#                           tsb="darkgoldenrod1",
#                           yield="dodgerblue3", 
#                           recruits="coral1",
#                           employment="skyblue2",
#                           profit = "purple")
#   )+
#   scale_x_continuous(n.breaks = 3)+
#   theme_bw()+
#   theme(strip.placement = "outside",
#       strip.background = element_blank())+
#   labs(y="")

  
# library(patchwork)
# cairo_pdf(here::here("figures/calibration.pdf"), width = 10, height=5)
# q1 = p3+p2all + plot_layout(widths=c(5.5,1))
# print(
# pa/q1 + plot_layout(widths=c(5,1))
# )
# dev.off()


# for (i in 1:100){
#   cat(i, "-----------------------------------------\n")
#   l = simulate_pop(par = par_opt_empirical, 
#                   params_file_fish = here("params/cod_params.ini"), 
#                   params_file_fleet = here("params/fleet_1_params.ini"), 
#                   nsup=1e6, 
#                   nsteps=200,
#                   verbose=T, 
#                   out_file = here("fishery_output/age_dists_pred.csv"),
#                   using_empirical_mort = TRUE
#                   )  
# }
