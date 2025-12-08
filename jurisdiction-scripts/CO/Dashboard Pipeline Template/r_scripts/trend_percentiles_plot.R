

p_load(tidyverse,lubridate,janitor)

alpha_set=.05

recentdata_utility_bsts_fit_forecast <-
  list.files(path = "output/trends/bsts_output") %>%
  data.frame() %>%
  `colnames<-`("filename") %>%
  mutate(extension = xfun::file_ext(filename)) %>%
  filter(extension %in% "csv") %>%
  filter(grepl("utility_bsts_fit_forecast", filename, fixed = TRUE)) %>%
  filter(grepl("_21d", filename, fixed = TRUE)) %>%
  dplyr::mutate(date = str_remove(filename, "utility_bsts_fit_forecast") %>%
                  str_remove("_21d.csv") %>%
                  ymd()) %>%
  filter(date == max(date)) %>%
  select(filename) %>%
  unlist() %>%
  paste0("output/trends/bsts_output/",
          .) %>%
  read_csv()

recentdata_utility_bsts_lm <-
  list.files(path = "output/trends/bsts_output") %>%
  data.frame() %>%
  `colnames<-`("filename") %>%
  mutate(extension = xfun::file_ext(filename)) %>%
  filter(extension %in% "csv") %>%
  filter(grepl("utility_bsts_lm", filename, fixed = TRUE)) %>%
  filter(grepl("_21d", filename, fixed = TRUE)) %>%
  dplyr::mutate(date = str_remove(filename, "utility_bsts_lm_") %>%
                  str_remove("_21d.csv") %>%
                  ymd()) %>%
  filter(date == max(date)) %>%
  select(filename) %>%
  unlist() %>%
  paste0("output/trends/bsts_output/",
         .) %>%
  read_csv(col_types = cols(lpha3 = col_character())) #fixes issue with lpha3 turning into boolean and giving a warning

viral_data <- read.csv("output/working data files/WW_Dashboard_Data.csv") %>%
  select(Utility, Date, pcr_target, flow_rate, sample_type) %>%
  filter(pcr_target == "sars-cov-2") %>%
  select(-pcr_target) %>%
  rename(utility = Utility,
         measure_date = Date) %>%
  mutate(measure_date = as.Date(measure_date)) %>%
  filter(utility %in% recentdata_utility_bsts_lm$utility & measure_date %in% recentdata_utility_bsts_lm$measure_date)

#add in metadata for flow_rate and sample_type  
recentdata_utility_bsts_lm <- recentdata_utility_bsts_lm %>%
  left_join(viral_data, by = c("utility", "measure_date"), relationship = "many-to-many") %>%
  distinct()

#combine datasets for plot
raw_dat <- recentdata_utility_bsts_fit_forecast %>%
  select(utility,measure_date,level=trend) %>%
  inner_join(recentdata_utility_bsts_lm %>%
               select(utility,measure_date,slope,p_val, flow_rate, sample_type), relationship = "many-to-many") %>%
  distinct(utility,measure_date,.keep_all = T) %>%
  drop_na(slope)



plot_dat <- raw_dat %>%
  group_by(utility) %>%
  mutate(level_tile = percent_rank(level),
         level_scale = scale(level)) %>%
  ungroup()

SystemData_plotdat <- SystemData %>%
  select(-sample_type)

plot_dat <- plot_dat %>%
  left_join(SystemData_plotdat, by = c("utility" = "wwtp_name")) %>%
  select(c(utility, measure_date, level, slope, p_val, level_tile, level_scale, population_served, capacity_mgd, flow_rate, sample_type)) %>%
  mutate(population_served_flow = (flow_rate * 1000000) / 70) %>% # (flow rate * 1000000 g / mg / 70 gd/capita)
  mutate(sig = case_when(sig= p_val <= 0.05 ~ "Significant", TRUE ~ "Not Significant"))


write.csv(plot_dat, paste0("dashboards/InternalDashboard/data/trend_percentile_data.csv"))



