
##################################################################################################################
####Finish adjusting for internal dashboard output
SystemData_combinescript <- SystemData %>%
  select(wwtp_name,
         sitename_id,
         lab_id,
         county_names,
         population_served,
         lpha1,
         lpha2,
         lpha3, 
         lpha1_contactname,
         lpha1_contactemail) %>%
  rename(utility = wwtp_name)

#####################################################
####            READ IN DATA                      ###
#####################################################

# Sample metadata
Metadata <-
  read_csv("output/working data files/WW_Dashboard_Data.csv") %>%
  dplyr::rename(utility = Utility,
                measure_date = Date,
                viral_conc_raw = `Viral Copies/L (uncorrected)`,
                log_viral_conc_raw = `Log_Viral Copies/L (uncorrected)`) %>%
  mutate(measure_date = lubridate::as_date(measure_date, format = "%m/%d/%y"))

# Trend data - sample level trends and weekly trend category
TrendData <-
  list.files(path = "output/trends/bsts_output") %>%
  data.frame() %>%
  `colnames<-`("filename") %>%
  mutate(extension = xfun::file_ext(filename)) %>%
  filter(extension %in% "csv") %>%
  filter(grepl("utility_bsts_lm_", filename, fixed = TRUE)) %>%
  filter(grepl("_21d", filename, fixed = TRUE)) %>%
  dplyr::mutate(date = str_remove(filename, "utility_bsts_lm_") %>%
                  str_remove("_21d.csv") %>%
                  ymd()) %>%
  filter(date == max(date)) %>%
  select(filename) %>%
  unlist() %>%
  paste0("output/trends/bsts_output/",
         .) %>%
  read_csv(col_types = cols(lpha3 = col_character())) %>% #fixing an issue with lpha3 turning into boolean and giving a warning  mutate(pcr_target = "sars-cov-2") %>%
  select(utility, pcr_target, measure_date, classification, slope, p_val) %>%
  rename(trend = classification) 

#output only most recent trend result
TrendData <- TrendData %>%
  filter(!is.na(trend)) %>%
  group_by(utility, pcr_target) %>%
  slice_tail(n = 1)

#Detection Status data based on CDC definitions
DetectionStatus <- list.files(path = "output/trends/detection_status") %>%
  data.frame() %>%
  `colnames<-`("filename") %>%
  mutate(extension = xfun::file_ext(filename)) %>%
  filter(extension %in% "csv") %>%
  filter(grepl("IDB_DetectionStatus_", filename, fixed = TRUE)) %>%
  dplyr::mutate(date = str_remove(filename, "IDB_DetectionStatus_") %>%
                  ymd()) %>%
  filter(date == max(date)) %>%
  select(filename) %>%
  unlist() %>%
  paste0("output/trends/detection_status/",
         .) %>%
  read_csv() %>%
  mutate(measure_date = as.Date(measure_date)) %>%
  select(-...1)

variants <- sars_sequencing_data %>%
  ungroup() %>%
  distinct(variant)

#read in county data for each utility
WWTPMajorCounties <- WWTP_Counties_reference %>%
  separate_rows(overlapping_counties, sep = ", ") %>%
  mutate(county = paste(primary_county, overlapping_counties, sep = "_")) %>%
  separate_rows(county, sep = "_") %>%
  select(utility, county) %>%
  filter(!county == "") %>%
  filter(!is.na(county)) %>%
  filter(!county == "NA") %>%
  distinct()


# Merge sample-date level data
wwdatafull_allcolumns <- Metadata %>%
  left_join(SystemData_combinescript, by = "utility") %>%
  left_join(sars_sequencing_data, by = c("utility" = "wwtp_name", "pcr_target", "sample_id"), relationship = "many-to-many") %>%
  left_join(TrendData, by = c("utility", "pcr_target", "measure_date"), relationship = "many-to-many") %>%
  left_join(DetectionStatus, by = c("utility", "pcr_target", "measure_date"), relationship = "many-to-many") 

#arrange coluumns 
wwdatafull_allcolumns <- wwdatafull_allcolumns %>%
  relocate(detection_status, .after = trend) %>%
  relocate(week, .after = detection_status) %>%
  relocate(ND_flag, .after = week)


#remove duplicates
wwdatafull_allcolumns <- wwdatafull_allcolumns %>%
  distinct()

###########################################
###       IDB Dashboard Output        ##
###########################################

wwdatafull <- wwdatafull_allcolumns %>%
  select(utility,
         measure_date, 
         viral_conc_raw, log_viral_conc_raw,
         pcr_target_cl_95_lo,
         pcr_target_cl_95_up,
         sitename_id, lab_id, county_names, 
         population_served, 
         lpha1, lpha2, lpha3,
         lpha1_contactname, lpha1_contactemail,
         sample_type, sample_id, quality_flag, flow_rate,        
         pcr_target, pcr_target_below_lod,
         trend, detection_status, slope, p_val, lab_phase, variant, abundance) 


#calculate normalized viral concentration based on flowrate and population served
wwdatafull <- wwdatafull %>%
  mutate(sample_id = case_when(sample_id == "" ~ NA, 
                               TRUE ~ sample_id)) %>%
  mutate(viral_conc_fpnorm = (10^((log10(viral_conc_raw)) + (log10(flow_rate*3.7854*1000000)) - (log10(population_served))))/1000000) %>% #NWSS calculation
  mutate(log_viral_conc_fpnorm = ((log10(viral_conc_raw)) + (log10(flow_rate*3.7854*1000000)) - (log10(population_served)))/1000000) %>% #NWSS calculation 
  relocate(viral_conc_fpnorm, .after = log_viral_conc_raw) %>%
  relocate(log_viral_conc_fpnorm, .after = viral_conc_fpnorm)

firstDate <-   wwdatafull %>%
  filter(!is.na(viral_conc_raw)) %>%
  arrange(utility, pcr_target, measure_date) %>%
  group_by(utility, pcr_target) %>%
  slice_head(n=1)  %>%
  mutate(first_date = measure_date) %>%
  select(utility, first_date) %>%
  distinct() %>% #denver county jail has two start dates that are identical (need to simplify results in IDB script)
  ungroup

##ENTER TRENDS SCRIPT HERE?

wwdatafull <- wwdatafull %>%
  full_join(firstDate, by = c("utility", "pcr_target")) %>%
  left_join(IDB_masterlist, by = c("pcr_target", "utility" = "wwtp_name")) %>%
  filter(measure_date >= activation_start) %>%
  filter(measure_date <= activation_end | is.na(activation_end)) %>%
  select(-c(participation_status, public_repository, activation_start, activation_end, Surv_System)) %>%
  mutate(pcr_target = case_when(pcr_target == "PanRSV" ~ "RSV A + B Combined", TRUE ~ pcr_target))


wwdatafull_LP3_overlap <- wwdatafull %>%
  filter(lab_phase == "Lab Phase 3" & measure_date < "2024-09-30") #posting LP3 results after 9/30/2024, use side by side LP2 results until then

wwdatafull_LP2_overlap <- wwdatafull %>%
  filter(lab_phase == "Lab Phase 2" & measure_date >= "2024-09-30") #posting LP3 results after 9/30/2024, use side by side LP2 results until then

wwdatafull_remove_LP_overlap <- anti_join(wwdatafull, wwdatafull_LP3_overlap)
IDB_output <- anti_join(wwdatafull_remove_LP_overlap, wwdatafull_LP2_overlap)


#Write it to the application directory
OutputName = ("dashboards/InternalDashboard/data/wwdatafull.csv")
write_csv(IDB_output, OutputName,na = "")



###########################################
###       Public Dashboard Output        ##
###########################################

#select columns needed
PDB_output <- wwdatafull_allcolumns %>%
  left_join(PFD_masterlist, by = c("pcr_target", "utility" = "wwtp_name")) %>%
  filter(measure_date >= activation_start) %>%
  filter(measure_date <= activation_end | is.na(activation_end))


wwdatafull_LP3_overlap_PDB <- PDB_output %>%
  filter(lab_phase == "Lab Phase 3" & measure_date < "2024-09-30")

wwdatafull_LP2_overlap_PDB <- PDB_output %>%
  filter(lab_phase == "Lab Phase 2" & measure_date >= "2024-09-30")

wwdatafull_remove_LP_overlap_PDB <- anti_join(PDB_output, wwdatafull_LP3_overlap_PDB)
wwdatafull_remove_LP_overlap_PDB <- anti_join(wwdatafull_remove_LP_overlap_PDB, wwdatafull_LP2_overlap_PDB)

#get percentiles plot output
percentile_plot_dat <- read.csv("dashboards/InternalDashboard/data/trend_percentile_data.csv") %>%
  left_join(PFD_masterlist %>% filter(pcr_target == "sars-cov-2"), by = c("utility" = "wwtp_name")) %>%
  filter(participation_status == "Active" & public_repository == "Publish") %>%
  filter(measure_date >= activation_start) %>%
  filter(measure_date <= activation_end | is.na(activation_end)) %>%
  mutate(measure_date = as.Date(measure_date, format = "%Y-%m-%d")) %>%
  mutate(sig = case_when(sig == "Not Significant" ~ "Trend Slope Not Significant", TRUE ~ "Trend Slope Significant")) %>%
  select(utility, measure_date, pcr_target, level_tile, sig) %>%
  group_by(utility) 

# #add percentiles plot output to pdb_output
PDB_output <- wwdatafull_remove_LP_overlap_PDB %>%
  left_join(percentile_plot_dat, by = c("pcr_target", "utility", "measure_date")) %>%
  relocate(level_tile, .before = p_val) %>%
  relocate(sig, .after = p_val) %>%
  rename(percentile_lvl = level_tile)


#write to google drive - google drive doesn't accept -inf; add rolling averages
PDB_output <- PDB_output %>%
  group_by(utility, pcr_target) %>%
  arrange(utility, pcr_target, measure_date) %>%
  mutate(viral_conc_raw = case_when(is.na(viral_conc_raw) ~ 0, TRUE ~ viral_conc_raw)) %>%
  select(utility,
         measure_date, 
         viral_conc_raw,
         sample_id, sitename_id, lab_id, county_names, 
         population_served,
         sample_type, sample_id, quality_flag, pcr_target_below_lod, ND_flag, flow_rate,        
         pcr_target, trend, detection_status, slope, percentile_lvl, sig, lab_phase)          

PDB_output <- PDB_output %>%
  filter(
    (pcr_target == "sars-cov-2" & measure_date >= "2020-08-01") |
      (pcr_target == "FLUAV" & measure_date >= "2023-09-01") |
      (pcr_target == "FLUBV" & measure_date >= "2023-09-01") |
      (pcr_target == "RSV_A" & measure_date >= "2023-09-01") |
      (pcr_target == "RSV_B" & measure_date >= "2023-09-01") |
      (pcr_target == "PanRSV" & measure_date >= "2024-09-30") |   
      (pcr_target == "H1" & measure_date >= "2024-09-30") |
      (pcr_target == "H3" & measure_date >= "2024-09-30") |
      (pcr_target == "H5" & measure_date >= "2024-09-30") 
  )




#write to K drive for historic output
print("Writing PDB Output to K Drive")
PDBOutputName = paste0("dashboards/PublicDashboard/Public_Dashboard_Data_", Sys.Date(), ".csv")
write_csv(PDB_output, PDBOutputName)




######################################################
###      DUPLICATES FOR VALIDATION REPORT          ###
######################################################

#IDB
duplicatedata_IDB <- IDB_output %>%
  group_by(utility, pcr_target, measure_date) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  filter(!viral_conc_raw == 0)


if (nrow(duplicatedata_IDB) == 0) {
  duplicatedata_IDB <- data.frame(
    utility = "no data")
}

write.csv(duplicatedata_IDB, paste0("output/validation_reports/duplicatedata_IDB_", Sys.Date(), ".csv"))


#PFD
duplicatedata_PDB <- PDB_output %>%
  group_by(utility, pcr_target, measure_date) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  filter(!viral_conc_raw == 0)

if (nrow(duplicatedata_PDB) == 0) {
  duplicatedata_PDB <- data.frame(
    utility = "no data")
}

write.csv(duplicatedata_PDB, paste0("output/validation_reports/duplicatedata_PDB_", Sys.Date(), ".csv"))



