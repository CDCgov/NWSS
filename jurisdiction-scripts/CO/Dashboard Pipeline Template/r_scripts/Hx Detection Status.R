print("### Running Hx Detection Status ###")

sentinel_sites <- unique(PFD_masterlist_FluA$wwtp_name)
sentinel_sites_IDB <- unique(IDB_masterlist_FluA$wwtp_name)
sentinel_sites_IDB_Hx <- unique(IDB_masterlist_FluA_Hx$wwtp_name)


hx_OutputForSaving <- read.csv(paste0("output/working data files/flu_hx_scriptinput.csv")) %>%
  select(-X) %>%
  mutate(test_result_date =  as.Date(test_result_date, format = '%Y-%m-%d')) %>%
  mutate(sample_collect_date =  as.Date(sample_collect_date, format = '%Y-%m-%d')) %>%
  select(-c(pcr_target_avg_conc, pcr_target_std_error, pcr_target_cl_95_lo, pcr_target_cl_95_up)) %>%
  rename(pcr_target_avg_conc = pcr_target_avg_conc_adjusted,
         pcr_target_std_error = pcr_target_std_error_adjusted,
         pcr_target_cl_95_lo = pcr_target_cl_95_lo_adjusted,
         pcr_target_cl_95_up = pcr_target_cl_95_up_adjusted) %>%
  group_by(pcr_target, wwtp_name, sample_collect_date, sample_id, lab_phase) #to match grouping like outputforsaving in ww data compiler script


##################################
###      IDB HX Data           ###
##################################

##Read in values from WW Data Compiler##
flu_a_multiplex <- hx_OutputForSaving %>% 
  filter(pcr_target == "FLUAV" & lab_phase == "Lab Phase 3") %>% 
  filter(wwtp_name %in% sentinel_sites_IDB)
h5_detections <- hx_OutputForSaving %>% 
  filter(pcr_target == "H5") %>% 
  filter(wwtp_name %in% sentinel_sites_IDB_Hx)
h3_detections <- hx_OutputForSaving %>% 
  filter(pcr_target == "H3") %>% 
  filter(wwtp_name %in% sentinel_sites_IDB_Hx)
h1_detections <- hx_OutputForSaving %>% 
  filter(pcr_target == "H1") %>% 
  filter(wwtp_name %in% sentinel_sites_IDB_Hx)


##Detection Calculation for the H variants##
h5_detections_merged <- flu_a_multiplex %>% 
  left_join(h5_detections, by = 'sample_id') %>% 
  select(sample_id, wwtp_name.x, pcr_target.x, pcr_target.y, sample_collect_date.x, 
         pcr_target_avg_conc.x,pcr_target_below_lod.x, pcr_target_avg_conc.y, pcr_target_below_lod.y) %>% 
  rename(wwtp_name = wwtp_name.x,
         flu_pcr_target = pcr_target.x, 
         subclassification = pcr_target.y,
         sample_collect_date = sample_collect_date.x,
         flu_pcr_target_avg_conc = pcr_target_avg_conc.x,
         flu_below_lod = pcr_target_below_lod.x,
         h5_pcr_target_avg_conc = pcr_target_avg_conc.y,
         h5_below_lod = pcr_target_below_lod.y)

h5_detections_merged <- h5_detections_merged %>%
  mutate(h5_pcr_target_avg_conc = ifelse(h5_below_lod == 'yes', 600, h5_pcr_target_avg_conc),
         h5_detected = ifelse(h5_pcr_target_avg_conc > 1200, 'Detected', 'Not Detected'),
         h5_detected = ifelse(sample_collect_date >= "2024-07-11" & is.na(h5_pcr_target_avg_conc), "Insufficient FluA Concentration", h5_detected),
         h5_detected = ifelse(h5_detected == "Insufficient FluA Concentration" & flu_pcr_target_avg_conc > 600, "Missing Sample", h5_detected)) 

h5_detections_merged <- h5_detections_merged %>%
  mutate(h5_detected = case_when(flu_below_lod == 'yes'~ 'Not Detected', TRUE ~ h5_detected)) %>%
  filter(!is.na(h5_detected)) %>%
  select(wwtp_name, sample_id, sample_collect_date, flu_pcr_target, flu_pcr_target_avg_conc, h5_pcr_target_avg_conc, h5_detected) 



h3_detections_merged <- flu_a_multiplex %>% 
  left_join(h3_detections, by = 'sample_id') %>% 
  select(sample_id, wwtp_name.x, pcr_target.x, pcr_target.y, sample_collect_date.x, 
         pcr_target_avg_conc.x, pcr_target_below_lod.x, pcr_target_avg_conc.y, pcr_target_below_lod.y) %>% 
  rename(wwtp_name = wwtp_name.x,
         flu_pcr_target = pcr_target.x, 
         subclassification = pcr_target.y,
         sample_collect_date = sample_collect_date.x,
         flu_below_lod = pcr_target_below_lod.x,
         flu_pcr_target_avg_conc = pcr_target_avg_conc.x,
         h3_pcr_target_avg_conc = pcr_target_avg_conc.y,
         h3_below_lod = pcr_target_below_lod.y)

h3_detections_merged <- h3_detections_merged %>%
  mutate(h3_pcr_target_avg_conc = ifelse(h3_below_lod == 'yes', 600, h3_pcr_target_avg_conc),
         h3_detected = ifelse(h3_pcr_target_avg_conc > 1200, 'Detected', 'Not Detected'),
         h3_detected = ifelse(sample_collect_date >= "2024-07-11" & is.na(h3_pcr_target_avg_conc), "Insufficient FluA Concentration", h3_detected),
         h3_detected = ifelse(h3_detected == "Insufficient FluA Concentration" & flu_pcr_target_avg_conc > 600, "Missing Sample", h3_detected))

h3_detections_merged <- h3_detections_merged %>%
  mutate(h3_detected = case_when(flu_below_lod == 'yes'~ 'Not Detected', TRUE ~ h3_detected))  %>%
  filter(!is.na(h3_detected)) %>%
  select(wwtp_name, sample_id, sample_collect_date, flu_pcr_target, flu_pcr_target_avg_conc, h3_pcr_target_avg_conc, h3_detected) 


h1_detections_merged <- flu_a_multiplex %>% 
  left_join(h1_detections, by = 'sample_id') %>% 
  select(sample_id, wwtp_name.x, pcr_target.x, pcr_target.y, sample_collect_date.x, 
         pcr_target_avg_conc.x, pcr_target_below_lod.x, pcr_target_avg_conc.y, pcr_target_below_lod.y) %>% 
  rename(wwtp_name = wwtp_name.x,
         flu_pcr_target = pcr_target.x, 
         subclassification = pcr_target.y,
         sample_collect_date = sample_collect_date.x,
         flu_pcr_target_avg_conc = pcr_target_avg_conc.x,
         flu_below_lod = pcr_target_below_lod.x,
         h1_pcr_target_avg_conc = pcr_target_avg_conc.y,
         h1_below_lod = pcr_target_below_lod.y)

h1_detections_merged <- h1_detections_merged %>%
  mutate(h1_pcr_target_avg_conc = ifelse(h1_below_lod == 'yes', 600, h1_pcr_target_avg_conc),
         h1_detected = ifelse(h1_pcr_target_avg_conc > 1200, 'Detected', 'Not Detected'),
         h1_detected = ifelse(sample_collect_date >= "2024-07-11" & is.na(h1_pcr_target_avg_conc), "Insufficient FluA Concentration", h1_detected),
         h1_detected = ifelse(h1_detected == "Insufficient FluA Concentration" & flu_pcr_target_avg_conc > 600, "Missing Sample", h1_detected))

h1_detections_merged <- h1_detections_merged %>%
  mutate(h1_detected = case_when(flu_below_lod == 'yes'~ 'Not Detected', TRUE ~ h1_detected)) %>%
  filter(!is.na(h1_detected)) %>%
  select(wwtp_name, sample_id, sample_collect_date, flu_pcr_target, flu_pcr_target_avg_conc, h1_pcr_target_avg_conc, h1_detected) 


###Merging the detection statuses together##
hx_detections <- h5_detections_merged %>% 
  full_join(h3_detections_merged, by = c("wwtp_name", "sample_id", "sample_collect_date", "flu_pcr_target", "flu_pcr_target_avg_conc")) %>%
  full_join(h1_detections_merged, by = c("wwtp_name", "sample_id", "sample_collect_date", "flu_pcr_target", "flu_pcr_target_avg_conc"))

hx_missing_samples <- hx_detections %>% 
  filter(h1_detected == "Missing Sample" | h3_detected == "Missing Sample" | h5_detected == "Missing Sample")


# Identify groups with only one unique date
single_date_groups <- hx_detections %>%
  group_by(wwtp_name, flu_pcr_target) %>%
  filter(n_distinct(sample_collect_date) == 1)

# Apply padding only to groups with varying dates
hx_detections_filled_IDB <- hx_detections %>%
  group_by(wwtp_name, flu_pcr_target) %>%
  filter(n_distinct(sample_collect_date) > 1) %>%
  pad(by = "sample_collect_date", interval = "day") %>%
  bind_rows(single_date_groups) %>%
  arrange(wwtp_name, sample_collect_date)

hx_detections_filled_IDB <- hx_detections_filled_IDB %>% 
  mutate(h1_detected = tidyr::replace_na(h1_detected, "No Sample"),
         h3_detected = tidyr::replace_na(h3_detected, "No Sample"),
         h5_detected = tidyr::replace_na(h5_detected, "No Sample"),
         flu_pcr_target = case_when(flu_pcr_target == "FLUAV" ~ "Inf_A-MP", TRUE ~ flu_pcr_target)) %>%
  mutate(across(everything(), ~ gsub("Missing Sample", "No Sample", .))) %>%
  mutate(across(everything(), ~ gsub("Insufficient FluA Concentration", "Not Detected", .))) %>%
  filter(sample_collect_date >= "2024-09-30")

#fill 6/23/24 to 9/30/24 with No Sample
flu_a_multiplex_LP2 <- hx_OutputForSaving %>% 
  filter(pcr_target == "FLUAV" & lab_phase == "Lab Phase 2" & sample_collect_date >= "2024-06-24") %>% 
  filter(wwtp_name %in% sentinel_sites_IDB) %>%
  select(c(wwtp_name, sample_collect_date, pcr_target_avg_conc)) %>%
  rename(flu_pcr_target_avg_conc = pcr_target_avg_conc) %>% 
  group_by(wwtp_name) %>%
  complete(sample_collect_date = seq.Date(from = as.Date("2024-06-24"), to = as.Date("2024-09-29"), by = "day"),
           fill = list(flu_pcr_target_avg_conc = NA)) %>%
  mutate(h5_detected = "No Sample",
         h3_detected = "No Sample",
         h1_detected = "No Sample") %>%
  mutate(flu_pcr_target = "Inf_A-MP") %>%
  select(-pcr_target) %>%
  mutate(h5_pcr_target_avg_conc = as.character(NA),
         h3_pcr_target_avg_conc = as.character(NA),
         h1_pcr_target_avg_conc = as.character(NA)) %>%
  mutate(sample_collect_date = as.character(sample_collect_date)) %>%
  mutate(flu_pcr_target_avg_conc = as.character(flu_pcr_target_avg_conc)) %>%
  select(c("wwtp_name", "sample_id", "sample_collect_date", "flu_pcr_target", "flu_pcr_target_avg_conc",
           "h5_pcr_target_avg_conc", "h5_detected", "h3_pcr_target_avg_conc" , "h3_detected",
           "h1_pcr_target_avg_conc", "h1_detected"))

hx_detections_filled_IDB <- rbind(flu_a_multiplex_LP2, hx_detections_filled_IDB) %>%
  arrange(wwtp_name, sample_collect_date)

hx_detections_filled_IDB <- hx_detections_filled_IDB %>%
  mutate(h5_detected = case_when(is.na(flu_pcr_target_avg_conc) & !is.na(h5_pcr_target_avg_conc) ~ "Sample Processing", TRUE ~ h5_detected),
         h3_detected = case_when(is.na(flu_pcr_target_avg_conc) & !is.na(h3_pcr_target_avg_conc) ~ "Sample Processing", TRUE ~ h3_detected),
         h1_detected = case_when(is.na(flu_pcr_target_avg_conc) & !is.na(h1_pcr_target_avg_conc) ~ "Sample Processing", TRUE ~ h1_detected))

hx_detections_filled_IDB <- hx_detections_filled_IDB %>% 
  mutate(
    h5_detected = case_when(h5_detected == "Sample Processing" ~ "No Sample",TRUE ~ h5_detected),
    h3_detected = case_when(h3_detected == "Sample Processing" ~ "No Sample", TRUE ~ h3_detected),
    h1_detected = case_when(h1_detected == "Sample Processing" ~ "No Sample", TRUE ~ h1_detected))

missing_flua_samples_withhxresult <- hx_detections_filled_IDB %>%
  filter(is.na(flu_pcr_target_avg_conc) & !is.na(h5_pcr_target_avg_conc)) %>%
  filter(is.na(flu_pcr_target_avg_conc) & !is.na(h3_pcr_target_avg_conc)) %>%
  filter(is.na(flu_pcr_target_avg_conc) & !is.na(h1_pcr_target_avg_conc)) %>%
  select(sample_id)

hx_detections_filled_IDB <- hx_detections_filled_IDB %>%
  filter(!sample_id %in% missing_flua_samples_withhxresult)

hx_detections_filled_IDB <- hx_detections_filled_IDB %>%
  filter(wwtp_name %in% sentinel_sites_IDB_Hx)

##Adjust for off season
hx_detections_filled_IDB <- hx_detections_filled_IDB %>%
  mutate(h1_detected = case_when(sample_collect_date >= "2025-05-17" ~ "Not Tested",
                                 TRUE ~ h1_detected)) %>%
  mutate(h3_detected = case_when(sample_collect_date >= "2025-05-17" ~ "Not Tested",
                                 TRUE ~ h3_detected)) 

#########################################
##      PRINT TO IDB DASHBOARDS       ###
#########################################

#save to IDB drive
write.csv(hx_detections_filled_IDB, "dashboards/InternalDashboard/data/flua_hx_data.csv")



##################################
###      PFD HX Data           ###
##################################

##Read in values from WW Data Compiler##
flu_a_multiplex <- hx_OutputForSaving %>% 
  filter(pcr_target == "FLUAV" & lab_phase == "Lab Phase 3") %>% 
  filter(wwtp_name %in% sentinel_sites)
h5_detections <- hx_OutputForSaving %>% 
  filter(pcr_target == "H5") %>% 
  filter(wwtp_name %in% sentinel_sites)
h3_detections <- hx_OutputForSaving %>% 
  filter(pcr_target == "H3") %>% 
  filter(wwtp_name %in% sentinel_sites)
h1_detections <- hx_OutputForSaving %>% 
  filter(pcr_target == "H1") %>% 
  filter(wwtp_name %in% sentinel_sites)


##Detection Calculation for the H variants##
h5_detections_merged <- flu_a_multiplex %>% 
  left_join(h5_detections, by = 'sample_id') %>% 
  select(sample_id, wwtp_name.x, pcr_target.x, pcr_target.y, sample_collect_date.x, 
         pcr_target_avg_conc.x,pcr_target_below_lod.x, pcr_target_avg_conc.y, pcr_target_below_lod.y) %>% 
  rename(wwtp_name = wwtp_name.x,
         flu_pcr_target = pcr_target.x, 
         subclassification = pcr_target.y,
         sample_collect_date = sample_collect_date.x,
         flu_pcr_target_avg_conc = pcr_target_avg_conc.x,
         flu_below_lod = pcr_target_below_lod.x,
         h5_pcr_target_avg_conc = pcr_target_avg_conc.y,
         h5_below_lod = pcr_target_below_lod.y)

h5_detections_merged <- h5_detections_merged %>%
  mutate(h5_pcr_target_avg_conc = ifelse(h5_below_lod == 'yes', 600, h5_pcr_target_avg_conc),
         h5_detected = ifelse(h5_pcr_target_avg_conc > 1200, 'Detected', 'Not Detected'),
         h5_detected = ifelse(sample_collect_date >= "2024-07-11" & is.na(h5_pcr_target_avg_conc), "Insufficient FluA Concentration", h5_detected),
         h5_detected = ifelse(h5_detected == "Insufficient FluA Concentration" & flu_pcr_target_avg_conc > 600, "Missing Sample", h5_detected)) 

h5_detections_merged <- h5_detections_merged %>%
  mutate(h5_detected = case_when(flu_below_lod == 'yes'~ 'Not Detected', TRUE ~ h5_detected)) %>%
  filter(!is.na(h5_detected)) %>%
  select(wwtp_name, sample_id, sample_collect_date, flu_pcr_target, flu_pcr_target_avg_conc, h5_pcr_target_avg_conc, h5_detected) 



h3_detections_merged <- flu_a_multiplex %>% 
  left_join(h3_detections, by = 'sample_id') %>% 
  select(sample_id, wwtp_name.x, pcr_target.x, pcr_target.y, sample_collect_date.x, 
         pcr_target_avg_conc.x, pcr_target_below_lod.x, pcr_target_avg_conc.y, pcr_target_below_lod.y) %>% 
  rename(wwtp_name = wwtp_name.x,
         flu_pcr_target = pcr_target.x, 
         subclassification = pcr_target.y,
         sample_collect_date = sample_collect_date.x,
         flu_below_lod = pcr_target_below_lod.x,
         flu_pcr_target_avg_conc = pcr_target_avg_conc.x,
         h3_pcr_target_avg_conc = pcr_target_avg_conc.y,
         h3_below_lod = pcr_target_below_lod.y)

h3_detections_merged <- h3_detections_merged %>%
  mutate(h3_pcr_target_avg_conc = ifelse(h3_below_lod == 'yes', 600, h3_pcr_target_avg_conc),
         h3_detected = ifelse(h3_pcr_target_avg_conc > 1200, 'Detected', 'Not Detected'),
         h3_detected = ifelse(sample_collect_date >= "2024-07-11" & is.na(h3_pcr_target_avg_conc), "Insufficient FluA Concentration", h3_detected),
         h3_detected = ifelse(h3_detected == "Insufficient FluA Concentration" & flu_pcr_target_avg_conc > 600, "Missing Sample", h3_detected))

h3_detections_merged <- h3_detections_merged %>%
  mutate(h3_detected = case_when(flu_below_lod == 'yes'~ 'Not Detected', TRUE ~ h3_detected))  %>%
  filter(!is.na(h3_detected)) %>%
  select(wwtp_name, sample_id, sample_collect_date, flu_pcr_target, flu_pcr_target_avg_conc, h3_pcr_target_avg_conc, h3_detected) 


h1_detections_merged <- flu_a_multiplex %>% 
  left_join(h1_detections, by = 'sample_id') %>% 
  select(sample_id, wwtp_name.x, pcr_target.x, pcr_target.y, sample_collect_date.x, 
         pcr_target_avg_conc.x, pcr_target_below_lod.x, pcr_target_avg_conc.y, pcr_target_below_lod.y) %>% 
  rename(wwtp_name = wwtp_name.x,
         flu_pcr_target = pcr_target.x, 
         subclassification = pcr_target.y,
         sample_collect_date = sample_collect_date.x,
         flu_pcr_target_avg_conc = pcr_target_avg_conc.x,
         flu_below_lod = pcr_target_below_lod.x,
         h1_pcr_target_avg_conc = pcr_target_avg_conc.y,
         h1_below_lod = pcr_target_below_lod.y)

h1_detections_merged <- h1_detections_merged %>%
  mutate(h1_pcr_target_avg_conc = ifelse(h1_below_lod == 'yes', 600, h1_pcr_target_avg_conc),
         h1_detected = ifelse(h1_pcr_target_avg_conc > 1200, 'Detected', 'Not Detected'),
         h1_detected = ifelse(sample_collect_date >= "2024-07-11" & is.na(h1_pcr_target_avg_conc), "Insufficient FluA Concentration", h1_detected),
         h1_detected = ifelse(h1_detected == "Insufficient FluA Concentration" & flu_pcr_target_avg_conc > 600, "Missing Sample", h1_detected))

h1_detections_merged <- h1_detections_merged %>%
  mutate(h1_detected = case_when(flu_below_lod == 'yes'~ 'Not Detected', TRUE ~ h1_detected)) %>%
  filter(!is.na(h1_detected)) %>%
  select(wwtp_name, sample_id, sample_collect_date, flu_pcr_target, flu_pcr_target_avg_conc, h1_pcr_target_avg_conc, h1_detected) 


###Merging the detection statuses together##
hx_detections <- h5_detections_merged %>% 
  full_join(h3_detections_merged, by = c("wwtp_name", "sample_id", "sample_collect_date", "flu_pcr_target", "flu_pcr_target_avg_conc")) %>%
  full_join(h1_detections_merged, by = c("wwtp_name", "sample_id", "sample_collect_date", "flu_pcr_target", "flu_pcr_target_avg_conc"))

hx_missing_samples <- hx_detections %>% 
  filter(h1_detected == "Missing Sample" | h3_detected == "Missing Sample" | h5_detected == "Missing Sample")


# Identify groups with only one unique date
single_date_groups <- hx_detections %>%
  group_by(wwtp_name, flu_pcr_target) %>%
  filter(n_distinct(sample_collect_date) == 1)

# Apply padding only to groups with varying dates
hx_detections_filled <- hx_detections %>%
  group_by(wwtp_name, flu_pcr_target) %>%
  filter(n_distinct(sample_collect_date) > 1) %>%
  pad(by = "sample_collect_date", interval = "day") %>%
  bind_rows(single_date_groups) %>%
  arrange(wwtp_name, sample_collect_date)

hx_detections_filled <- hx_detections_filled %>% 
  mutate(h1_detected = tidyr::replace_na(h1_detected, "No Sample"),
         h3_detected = tidyr::replace_na(h3_detected, "No Sample"),
         h5_detected = tidyr::replace_na(h5_detected, "No Sample"),
         flu_pcr_target = case_when(flu_pcr_target == "FLUAV" ~ "Inf_A-MP", TRUE ~ flu_pcr_target)) %>%
  mutate(across(everything(), ~ gsub("Missing Sample", "No Sample", .))) %>%
  mutate(across(everything(), ~ gsub("Insufficient FluA Concentration", "Not Detected", .))) %>%
  filter(sample_collect_date >= "2024-09-30")

#fill 6/23/24 to 9/30/24 with No Sample
flu_a_multiplex_LP2 <- hx_OutputForSaving %>% 
  filter(pcr_target == "FLUAV" & lab_phase == "Lab Phase 2" & sample_collect_date >= "2024-06-24") %>% 
  filter(wwtp_name %in% sentinel_sites) %>%
  select(c(wwtp_name, sample_collect_date, pcr_target_avg_conc)) %>%
  rename(flu_pcr_target_avg_conc = pcr_target_avg_conc) %>% 
  group_by(wwtp_name) %>%
  complete(sample_collect_date = seq.Date(from = as.Date("2024-06-24"), to = as.Date("2024-09-29"), by = "day"),
           fill = list(flu_pcr_target_avg_conc = NA)) %>%
  mutate(h5_detected = "No Sample",
         h3_detected = "No Sample",
         h1_detected = "No Sample") %>%
  mutate(flu_pcr_target = "Inf_A-MP") %>%
  select(-pcr_target) %>%
  mutate(h5_pcr_target_avg_conc = as.character(NA),
         h3_pcr_target_avg_conc = as.character(NA),
         h1_pcr_target_avg_conc = as.character(NA)) %>%
  mutate(sample_collect_date = as.character(sample_collect_date)) %>%
  mutate(flu_pcr_target_avg_conc = as.character(flu_pcr_target_avg_conc)) %>%
  select(c("wwtp_name", "sample_id", "sample_collect_date", "flu_pcr_target", "flu_pcr_target_avg_conc",
           "h5_pcr_target_avg_conc", "h5_detected", "h3_pcr_target_avg_conc" , "h3_detected",
           "h1_pcr_target_avg_conc", "h1_detected"))

hx_detections_filled <- rbind(flu_a_multiplex_LP2, hx_detections_filled) %>%
  arrange(wwtp_name, sample_collect_date)

hx_detections_filled <- hx_detections_filled %>%
  mutate(h5_detected = case_when(is.na(flu_pcr_target_avg_conc) & !is.na(h5_pcr_target_avg_conc) ~ "Sample Processing", TRUE ~ h5_detected),
         h3_detected = case_when(is.na(flu_pcr_target_avg_conc) & !is.na(h3_pcr_target_avg_conc) ~ "Sample Processing", TRUE ~ h3_detected),
         h1_detected = case_when(is.na(flu_pcr_target_avg_conc) & !is.na(h1_pcr_target_avg_conc) ~ "Sample Processing", TRUE ~ h1_detected))

hx_detections_filled <- hx_detections_filled %>% 
  mutate(
    h5_detected = case_when(h5_detected == "Sample Processing" ~ "No Sample",TRUE ~ h5_detected),
    h3_detected = case_when(h3_detected == "Sample Processing" ~ "No Sample", TRUE ~ h3_detected),
    h1_detected = case_when(h1_detected == "Sample Processing" ~ "No Sample", TRUE ~ h1_detected))

missing_flua_samples_withhxresult <- hx_detections_filled %>%
  filter(is.na(flu_pcr_target_avg_conc) & !is.na(h5_pcr_target_avg_conc)) %>%
  filter(is.na(flu_pcr_target_avg_conc) & !is.na(h3_pcr_target_avg_conc)) %>%
  filter(is.na(flu_pcr_target_avg_conc) & !is.na(h1_pcr_target_avg_conc)) %>%
  select(sample_id)

hx_detections_filled <- hx_detections_filled %>%
  filter(!sample_id %in% missing_flua_samples_withhxresult)

hx_detections_filled_IDB <- hx_detections_filled_IDB %>%
  filter(wwtp_name %in% sentinel_sites_IDB_Hx)

########################################
##      PRINT TO PFD DASHBOARD       ###
########################################
## format date for PDB
hx_detections_filled <- hx_detections_filled %>%
  mutate(sample_collect_date =  format(as.Date(sample_collect_date, format = '%Y-%m-%d'), '%m/%d/%Y')) %>% #format as m/d/y
  mutate(sample_collect_date = as.Date(sample_collect_date, format = "%m/%d/%Y")) %>% #format as date format
  rename(utility = wwtp_name) %>%
  rename(pcr_target = flu_pcr_target) %>%
  rename(measure_date = sample_collect_date) %>%
  select(utility, pcr_target, measure_date, h1_detected, h3_detected, h5_detected) %>%
  mutate(pcr_target = case_when(pcr_target == "Inf_A-MP" ~ "FLUAV",
                                TRUE ~ pcr_target))

##Adjust for off season
hx_detections_filled <- hx_detections_filled %>%
  mutate(h1_detected = case_when(measure_date >= "2025-05-17" ~ "Not Tested",
                                 TRUE ~ h1_detected)) %>%
  mutate(h3_detected = case_when(measure_date >= "2025-05-17" ~ "Not Tested",
                                 TRUE ~ h3_detected)) 


# #write to PDB output
file_path <- "dashboards/PublicDashboard/PDB_hx_data.xlsx"
write.xlsx(hx_detections_filled, file = file_path, sheetName = "PDB_hx_data")



