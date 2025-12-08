
##read in PDB data
PDB_data <-
  list.files(path = "dashboards/PublicDashboard") %>%
  as.data.frame() %>%
  `colnames<-`("filename") %>%
  filter(grepl("Public_Dashboard_Data_", filename, fixed = TRUE)) %>%
  dplyr::mutate(date = str_remove(filename, "Public_Dashboard_Data_") %>%
                  str_remove(".csv") %>%
                  ymd()) %>%
  filter(date == max(date)) %>%
  select(filename) %>%
  unlist() %>%
  paste0("dashboards/PublicDashboard/", .) %>%
  read.csv() %>%
  mutate(measure_date = as.Date(measure_date))


##read in sars weekly trend data
weeklytrend_sars <- 
  list.files(path = "output/trends/bsts_output") %>%
  as.data.frame() %>%
  `colnames<-`("filename") %>%
  mutate(extension = file_ext(filename)) %>%
  filter(extension %in% "csv") %>%
  filter(grepl("wwweeklytrend_", filename, fixed = TRUE)) %>%
  dplyr::mutate(date = substr(filename, 15, 24) %>%
                  ymd()) %>%
  filter(date == max(date)) %>%
  select(filename) %>%
  unlist() %>%
  paste0("output/trends/bsts_output/", .) %>%
  read.csv() 


###########################################
###       Viral Concentration Data     ###
###########################################
SystemData_region <- SystemData %>%
  select(wwtp_name, Region, primary_county) %>%
  rename(utility = wwtp_name) %>%
  rename(County = primary_county)

PDB_output <- PDB_data %>% 
  filter(!is.na(sample_id)) %>%
  filter(!pcr_target %in% c("H5", "H3", "H1")) %>%
  mutate(percentile_lvl = round(percentile_lvl, 3))  %>%
  ungroup() %>%
  left_join(SystemData_region, by=c("utility")) %>%
  select(pcr_target, utility, measure_date, viral_conc_raw, pcr_target_below_lod, lab_phase, population_served, County, Region)


file_path <- "dashboards/PublicDashboard/PDB_conc.xlsx"
write.xlsx(PDB_output, file = file_path, sheetName = "PDB_conc")



############################################
###        Trends / Detections           ###
############################################

#sars trends
sars_trends <- PDB_data %>% 
  filter(!is.na(sample_id))%>% 
  select(-trend) %>%
  filter(pcr_target == "sars-cov-2") %>%
  group_by(utility, pcr_target) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  left_join(weeklytrend_sars, by = "utility") %>%
  rename(trend = recenttrend) %>%
  group_by(utility, pcr_target) %>%
  arrange(utility, pcr_target, desc(measure_date), trend) %>% 
  slice_head(n = 1) %>%
  select(utility, pcr_target, measure_date, trend) 


#detection status for remaining pathogens
pathogen_detection_status <- PDB_data %>% 
  filter(!pcr_target %in% c("sars-cov-2", "H5", "H3", "H1")) %>%
  ungroup() %>%
  filter(!is.na(detection_status)) %>%
  mutate(detection_status = case_when(detection_status == "No recent submission" ~ "No recent data",
                                      TRUE ~ detection_status)) %>%
  select(utility, pcr_target, measure_date, detection_status) %>%
  rename(trend = detection_status) %>%
  # mutate(trend = factor(trend, levels = c("Persistent detection", "Detection", "No recent detection", "No recent data"))) %>% 
  group_by(utility, pcr_target) %>%
  arrange(utility, pcr_target, desc(measure_date), trend) %>% 
  slice_head(n = 1)

all_trends <- pathogen_detection_status %>%
  rbind(sars_trends) %>%
  arrange(utility, pcr_target, desc(measure_date), trend) %>%
  select(utility, measure_date, trend, pcr_target)


file_path <- "dashboards/PublicDashboard/PDB_trends.xlsx"
write.xlsx(all_trends, file = file_path, sheetName = "PDB_trends")


#############################################
##    DLH TABLE UPLOAD FOR OPEN PORTAL     ##
#############################################
##read in OpenPortal data file

DLH_ww_table <- PDB_data %>%
  left_join(OpenPortal_masterlist, by = c("utility" = "wwtp_name", "pcr_target")) %>%
  filter(measure_date >= activation_start) %>%
  filter(measure_date <= activation_end | is.na(activation_end)) %>%
  filter(!pcr_target %in% c("H1", "H3", "H5")) %>% #remove flu subtyping values for public display
  filter(!is.na(sample_id)) %>%
  ungroup() %>%
  select(utility, pcr_target, measure_date, viral_conc_raw, lab_phase)


DLH_ww_table_RSV <- DLH_ww_table %>%
  filter(pcr_target %in% c("RSV_A", "RSV_B")) %>%
  mutate(pcr_target = case_when(pcr_target %in% c("RSV_A", "RSV_B") ~ "RSV", TRUE ~ pcr_target)) %>%
  group_by(utility, pcr_target, measure_date, lab_phase) %>%
  summarize(viral_conc_raw = sum(viral_conc_raw, na.rm = TRUE)) %>%
  mutate(viral_conc_raw = case_when(viral_conc_raw == 4000 ~ 2000, TRUE ~ viral_conc_raw))

DLH_ww_table_RSV_all <- DLH_ww_table %>%
  filter(pcr_target %in% c("PanRSV")) %>%
  mutate(pcr_target = case_when(pcr_target %in% c("PanRSV") ~ "RSV", TRUE ~ pcr_target)) %>%
  group_by(utility, pcr_target, measure_date, lab_phase) %>%
  mutate(viral_conc_raw = case_when(viral_conc_raw == 1600 ~ 600, TRUE ~ viral_conc_raw)) %>%
  rbind(DLH_ww_table_RSV)


DLH_ww_table  <- DLH_ww_table %>%
  filter(!pcr_target %in% c("RSV_A", "RSV_B", "PanRSV")) %>% #remove because joining merged dataset in next line
  rbind(DLH_ww_table_RSV_all) %>%
  mutate(viral_conc_raw_LP1 = case_when(lab_phase == "Lab Phase 1" ~ viral_conc_raw, TRUE ~ NA),
         viral_conc_raw_LP2 = case_when(lab_phase == "Lab Phase 2" ~ viral_conc_raw, TRUE ~ NA),
         viral_conc_raw_LP3 = case_when(lab_phase == "Lab Phase 3" ~ viral_conc_raw, TRUE ~ NA)) %>%
  select(-viral_conc_raw) %>%
  relocate(lab_phase, .after = viral_conc_raw_LP3) 


#write to k drive
OpenPortalOutputName = paste0("dashboards/PublicDashboard/OpenPortal_Data_", Sys.Date(), ".csv")
write_csv(DLH_ww_table, OpenPortalOutputName)



#Open Portal Duplicates
duplicatedata_ODP <- DLH_ww_table %>%
  filter(!viral_conc_raw_LP1 == 0) %>%
  filter(!viral_conc_raw_LP2 == 0) %>%
  filter(!viral_conc_raw_LP3 == 0) %>%
  group_by(utility, pcr_target, measure_date) %>%
  filter(n() > 1) %>%
  ungroup() 


if (nrow(duplicatedata_ODP) == 0) {
  duplicatedata_ODP <- data.frame(
    utility = "no data")
}

write.csv(duplicatedata_ODP, paste0("output/validation_reports/duplicatedata_ODP_", Sys.Date(), ".csv"))



