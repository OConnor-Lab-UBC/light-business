## data exploration and visualization


library(tidyverse)
library(lubridate)
library(broom)


ldata <- read_csv("data-processed/TT_cells.csv")

str(ldata)
ldata2 <- ldata %>% 
	mutate(light_level = NA) %>% 
	mutate(light_level = ifelse(replicate %in% c("11", "1", "6", "3"), "high_light", light_level)) %>% 
	mutate(light_level = ifelse(replicate %in% c("2", "4", "7", "10"), "med_high_light", light_level)) %>% 
	mutate(light_level = ifelse(replicate %in% c("5", "12", "8", "9"), "med_low_light", light_level)) %>%
	mutate(light_level = ifelse(replicate %in% c("14", "13", "16", "15"), "low_light", light_level)) %>% 
	mutate(start_time = ymd_hms(start_time)) %>% 
	mutate(light = NA) %>% 
	mutate(light = ifelse(light_level == "high_light", 140, light)) %>% 
	mutate(light = ifelse(light_level == "med_high_light", 80, light)) %>% 
	mutate(light = ifelse(light_level == "med_low_light", 50, light)) %>%
	mutate(light = ifelse(light_level == "low_light", 30, light)) %>% 
	separate(start_time, into = c("date", "time"), sep = " ", remove = FALSE) %>% 
	mutate(date = ymd(date))

## get the days in

ldata2$start.time <- ymd("2017-04-30")
ldata2$time_since_innoc <- interval(ldata2$start.time, ldata2$date)


ldata3 <- ldata2 %>% 
	mutate(time_since_innoc_days = time_since_innoc/ddays(1)) %>% 
	mutate(time_since_innoc_hours = time_since_innoc/dhours(1))



ldata3 %>% 
	ggplot(aes(x = start_time, y = cell_density, color = light_level, group = replicate)) + geom_point(size = 3) +
	facet_wrap( ~ light) + theme_bw() + ylab("population abundance (cells/ml)") + xlab("date") + geom_line()


growth <- ldata3 %>% 
	group_by(light_level, replicate) %>% 
	do(tidy(nls(cell_density ~ 1272.4* (1+a)^(time_since_innoc_days),
							data= .,  start=list(a=0.01),
							control = nls.control(maxiter=100, minFactor=1/204800000)))) 



growth %>%
	mutate(light = NA) %>% 
	mutate(light = ifelse(light_level == "high_light", 140, light)) %>% 
	mutate(light = ifelse(light_level == "med_high_light", 80, light)) %>% 
	mutate(light = ifelse(light_level == "med_low_light", 50, light)) %>%
	mutate(light = ifelse(light_level == "low_light", 30, light)) %>% 
	ggplot(aes(x = light, y = estimate)) + geom_point(size = 4) + theme_bw() + ylab("growth rate (r) / day") + xlab("light intensity (umols/m2/s)") +
theme(text = element_text(size=18))
