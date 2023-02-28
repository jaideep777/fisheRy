library(tidyverse)

extract_yearly = function(filename){
  dat = read.csv(filename, skip = 1)
  colnames(dat) = c("time", "dt")
  # dat %>% with(plot(dt~time))
  dat$year = floor(dat$time)
  dat_yearly = dat %>% group_by(year) %>% summarize(dt = mean(dt))  
  dat_yearly
}

dat = extract_yearly("~/Downloads/wpd_datasets(4).csv")
dat_hist = dat %>% filter(year < 2015)
dat_ssp5 = dat %>% filter(year >= 2015)

dat_ssp5 %>% with(plot(dt~year, type="l", col="red"))
abline(h=0, col="grey")
abline(v=2001, col="pink")

dat_ssp3 = extract_yearly("~/Downloads/wpd_datasets(3).csv")
dat_ssp3 %>% with(points(dt~year, type="l", col="orange"))
abline(h=0, col="grey")
abline(v=2001, col="pink")

dat_ssp2 = extract_yearly("~/Downloads/wpd_datasets(2).csv")
dat_ssp2 %>% with(points(dt~year, type="l", col="green2"))
abline(h=0, col="grey")
abline(v=2001, col="pink")

dat_ssp1 = extract_yearly("~/Downloads/wpd_datasets(1).csv")
dat_ssp1 %>% with(points(dt~year, type="l", col="skyblue2"))
abline(h=0, col="grey")
abline(v=2001, col="pink")
abline(v=2005, col="skyblue")

dt_2001 = dat_ssp5 %>% filter(year==2001) %>% select(dt)

abs_hist = dat_hist %>% mutate(temp = 5.61 + dt, recr = 1) %>% select(-dt)
abs_ssp1 = dat_ssp1 %>% mutate(temp = 5.61 + dt, recr = 1) %>% select(-dt)
abs_ssp2 = dat_ssp2 %>% mutate(temp = 5.61 + dt, recr = 1) %>% select(-dt)
abs_ssp3 = dat_ssp3 %>% mutate(temp = 5.61 + dt, recr = 1) %>% select(-dt)
abs_ssp5 = dat_ssp5 %>% mutate(temp = 5.61 + dt, recr = 1) %>% select(-dt)

abs_hist %>% write.csv("~/codes/fisheRy/data/env_hist.csv", row.names = F)
abs_ssp1 %>% write.csv("~/codes/fisheRy/data/env_ssp1.csv", row.names = F)
abs_ssp2 %>% write.csv("~/codes/fisheRy/data/env_ssp2.csv", row.names = F)
abs_ssp3 %>% write.csv("~/codes/fisheRy/data/env_ssp3.csv", row.names = F)
abs_ssp5 %>% write.csv("~/codes/fisheRy/data/env_ssp5.csv", row.names = F)


