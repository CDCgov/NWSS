

library(progress)

#####################################################
###        Read in Historic Trends                ###
#####################################################

#2024 data and prior (already printed to help with code efficiency)
IDB_detectionstatus_historic <- read.csv("reference_sheets/IDB_DetectionStatus_2024prior.csv") %>%
  mutate(measure_date = as.Date(measure_date)) %>%
  # mutate(detection_status = case_when(detection_status == "No Recent Detection"~ "No recent detection",
  #                                     detection_status == "Persistent Detection" ~ "Persistent detection",
  #                                     detection_status == "NO Recent Data" ~ "No recent data")) %>%
  select(-X)


#####################################################
###                 IDB Trends                    ###
#####################################################

# Sample metadata
Metadata <-read_csv("output/working data files/WW_Dashboard_Data.csv") %>%
  dplyr::rename(utility = Utility,
                measure_date = Date,
                viral_conc_raw = `Viral Copies/L (uncorrected)`,
                log_viral_conc_raw = `Log_Viral Copies/L (uncorrected)`) %>%
  mutate(measure_date = lubridate::as_date(measure_date, format = "%m/%d/%y")) 

##read in data
IDB_detectionstatusoutput <- Metadata %>%
  filter(!is.na(sample_id)) %>%  
  filter(lab_phase == "Lab Phase 3") %>%
  filter(!(pcr_target %in% c("RSV_A", "RSV_B"))) %>% #dont need to run on RSV A B separately
  mutate(ND_flag = case_when(((is.na(viral_conc_raw) | viral_conc_raw == 0) & is.na(pcr_target_cl_95_lo) | pcr_target_below_lod == "yes") ~ "yes", TRUE ~ NA)) %>%
  mutate(ND_flag = case_when(ND_flag == "yes" ~ "No Detection", TRUE ~ "Detection")) %>%
  select(utility, pcr_target, measure_date, ND_flag) %>%
  mutate(year_week = paste0(isoyear(measure_date), "-", isoweek(measure_date)), #create number of weeks from start of year with Monday as week start
         week_start = floor_date(measure_date, unit = "week", week_start = 1)  # date of Monday start
  ) %>%
  arrange(utility, pcr_target, year_week) %>%
  group_by(utility, pcr_target, year_week) %>%
  filter(!measure_date < "2024-12-01") #remove historic data to help with code efficiency, keep dec so jan classification write correctly


week_lookup <- IDB_detectionstatusoutput %>%
  ungroup() %>%
  select(year_week, week_start) %>%
  distinct(year_week, week_start) %>%
  arrange(desc(week_start)) %>%
  mutate(week = row_number() - 1)

IDB_detectionstatusoutput <- IDB_detectionstatusoutput %>%
  left_join(week_lookup, by = c("year_week", "week_start"))


#------------------------------------------------------------
#determine how many samples have been DETECTED within 2 weeks
#------------------------------------------------------------

# Initialize a new column for the flag
IDB_detectionstatusoutput$CDC_TwoWeek_Detection_Flag <- "no"

# Get unique combinations of utility, pcr_target, and week
unique_groups <- IDB_detectionstatusoutput %>%
  distinct(utility, pcr_target, week)

#Define list of utilities 
unique_groups_pb <- unique_groups %>%
  ungroup() %>%
  count(utility, pcr_target, week) %>%
  arrange(desc(n))

print("Step 1 of 3: Determining how many samples have been Detected within 2 weeks")
#Define progress bar
pb <- progress_bar$new(total = nrow(unique_groups_pb))

# Loop through each unique group
for (i in seq_len(nrow(unique_groups))) {
  group <- unique_groups[i, ]
  
  pb$tick() #for progress bar
  
  # Get the current week's rows and the previous 2 weeks
  rows_to_check <- IDB_detectionstatusoutput %>%
    filter(
      utility == group$utility,
      pcr_target == group$pcr_target,
      week <= group$week + 1, # Include previous week
      week >= group$week
    )
  
  # Check if any row in this group has ND_flag == "Detection"
  if (any(rows_to_check$ND_flag == "Detection", na.rm = TRUE)) {
    # Mark all rows for the current week as "yes"
    IDB_detectionstatusoutput <- IDB_detectionstatusoutput %>%
      mutate(
        CDC_TwoWeek_Detection_Flag = ifelse(
          utility == group$utility &
            pcr_target == group$pcr_target &
            week == group$week,
          "yes",
          CDC_TwoWeek_Detection_Flag
        )
      )
  }
}



#------------------------------------------------------------
#determine how many samples have been DETECTED within 4 weeks
#------------------------------------------------------------

# Initialize a new column for the flag
IDB_detectionstatusoutput$CDC_FourWeek_Detections <- 0

# Get unique combinations of utility, pcr_target, and week
unique_groups <- IDB_detectionstatusoutput %>%
  distinct(utility, pcr_target, week)


print("Step 2 of 3: Determining how many samples have been Detected within 4 weeks")

#Define progress bar
pb <- progress_bar$new(total = nrow(unique_groups_pb))

# Loop through each unique group
for (i in seq_len(nrow(unique_groups))) {
  group <- unique_groups[i, ]
  
  pb$tick() #for progress bar
  
  # Get the current week's rows and the previous 4 weeks
  rows_to_check <- IDB_detectionstatusoutput %>%
    filter(
      utility == group$utility,
      pcr_target == group$pcr_target,
      week <= group$week + 3, # Include current week + 3 more weeks = 4
      week >= group$week
    )
  
  # Summarize the count of detections in this group
  detection_count <- rows_to_check %>%
    filter(ND_flag == "Detection") %>%
    nrow()
  
  # Update the original dataframe for the current week
  IDB_detectionstatusoutput <- IDB_detectionstatusoutput %>%
    mutate(
      CDC_FourWeek_Detections = ifelse(
        utility == group$utility &
          pcr_target == group$pcr_target &
          week == group$week,
        detection_count,
        CDC_FourWeek_Detections
      )
    )
}



#------------------------------------------------------------
#determine how many samples have been SUBMITTED within 4 weeks
#------------------------------------------------------------

# Initialize a new column for the flag
IDB_detectionstatusoutput$CDC_FourWeek_Submission <- 0

# Get unique combinations of utility, pcr_target, and week
unique_groups <- IDB_detectionstatusoutput %>%
  distinct(utility, pcr_target, week)

print("Step 3 of 3: Determining how many samples have been Submitted within 4 weeks")
#Define progress bar
pb <- progress_bar$new(total = nrow(unique_groups_pb))

# Loop through each unique group
for (i in seq_len(nrow(unique_groups))) {
  group <- unique_groups[i, ]
  
  
  pb$tick() #for progress bar
  
  # Get the current week's rows and the previous 4 weeks
  rows_to_check <- IDB_detectionstatusoutput %>%
    filter(
      utility == group$utility,
      pcr_target == group$pcr_target,
      week <= group$week + 3, # IInclude current week + 3 more weeks = 4
      week >= group$week
    )
  
  # Summarize the count of samples submitted in this group
  samplessubmitted <- rows_to_check %>%
    filter(!is.na(ND_flag)) %>%
    nrow()
  
  # Update the original dataframe for the current week
  IDB_detectionstatusoutput <- IDB_detectionstatusoutput %>%
    mutate(
      CDC_FourWeek_Submission = ifelse(
        utility == group$utility &
          pcr_target == group$pcr_target &
          week == group$week,
        samplessubmitted,
        CDC_FourWeek_Submission
      )
    )
}


#---------------------------------------------------------------
#determine how many PERCENT of samples submitted within 4 weeks
#---------------------------------------------------------------

#combine together and determine PERCENT OF DETECTIONS within 4 weeks
IDB_detectionstatusoutput <- IDB_detectionstatusoutput %>%
  mutate(FourWeek_Detection_Percent = (CDC_FourWeek_Detections/CDC_FourWeek_Submission)*100) %>%
  mutate(FourWeek_Detection_Percent = case_when(is.na(FourWeek_Detection_Percent) ~ 0, 
                                                TRUE ~ FourWeek_Detection_Percent)) %>%
  mutate(CDC_PersistentDetection_Flag = case_when(FourWeek_Detection_Percent >= 80 ~ "yes", 
                                                  TRUE ~ "no")) %>%
  mutate(CDC_Detection_Flag = case_when((FourWeek_Detection_Percent <= 80 & FourWeek_Detection_Percent >= 1) ~ "yes", 
                                        TRUE ~ "no"))


#---------------------------------------------------------------
#Classify CDC Definitions
#---------------------------------------------------------------
##create trend classification based on CDC definitions
# Persistent detection: The virus that causes mpox was detected in more than 80% of samples in the past 4 weeks AND the most recent detection was within the past 2 weeks.
# Detection: The virus that causes mpox was detected in 1% to 80% of samples in the past 4 weeks AND the most recent detection was within the past 2 weeks.
# No recent data: Fewer than 3 samples were submitted in the past 4 weeks.
# No recent detection: The virus that causes mpox was not detected in any samples in the past 2 weeks.



#merge with historic data and label utilities we haven't received samples from in over a month as "no recent data"
IDB_detectionstatus <- IDB_detectionstatusoutput %>%
  filter(measure_date > "2024-12-31" | is.na(measure_date)) %>%  #remove dec so we don't get duplicates from historic dataset, keep NAs (originally needed to help calculate jan)
  rbind(IDB_detectionstatus_historic)

IDB_detectionstatus <- IDB_detectionstatus %>%
  ungroup() %>%
  mutate(detection_status = case_when((CDC_PersistentDetection_Flag == "yes" & CDC_TwoWeek_Detection_Flag == "yes") ~ "Persistent detection",
                                      (CDC_PersistentDetection_Flag == "no" & CDC_TwoWeek_Detection_Flag == "yes") ~ "Detection",
                                      CDC_FourWeek_Submission < 3 ~ "No recent data", 
                                      CDC_TwoWeek_Detection_Flag == "no" ~ "No recent detection",                   
                                      TRUE ~ "No recent data")) %>%
  select(utility, pcr_target, measure_date, week, ND_flag, detection_status) %>%
  arrange(desc(measure_date), utility, pcr_target) 





#write to k drive output
write.csv(IDB_detectionstatus, paste0("output/trends/detection_status/IDB_DetectionStatus_", Sys.Date(), ".csv"))










#####################################################
###           Detection Status Summary Sheet      ###
#####################################################

pcr_targets <- unique(IDB_masterlist$pcr_target)

detectioncat <- data.frame(
  pcr_target = rep(unique(IDB_masterlist$pcr_target), each = 4),  # Repeat each pcr_target 4 times
  detection_status = rep(c("Persistent detection", 
                           "Detection", 
                           "No recent detection", 
                           "No recent data"), 
                         times = length(unique(IDB_masterlist$pcr_target))),  # Repeat statuses for each pcr_target
  utility = NA,
  measure_date = NA,
  last_date = NA
)

last_available_date <- IDB_detectionstatus %>%
  group_by(utility, pcr_target) %>%
  arrange(utility, pcr_target, desc(measure_date)) %>%
  slice_head(n = 1) %>% #slice for latest submission date per utility/pcr_target
  filter(!week == 0) %>% #remove utilities we've received a sample from this week
  select(utility, pcr_target, measure_date) %>%
  rename(last_date = measure_date)

detectionstatus_summary <- IDB_detectionstatus %>%
  ungroup() %>%
  mutate(measure_date = as.Date(measure_date)) %>%
  left_join(last_available_date, by = c("utility", "pcr_target"), relationship = "many-to-many") %>%
  group_by(utility, pcr_target) %>%
  arrange(utility, pcr_target, week, desc(measure_date)) %>%
  slice_head(n = 1) %>% #slice for latest status per utility/pcr_target
  ungroup() %>%
  full_join(IDB_masterlist, by = c("utility" = "wwtp_name", "pcr_target")) %>%
  filter(Surv_System %in% c("SSS", "ESS", "FSS")) %>%
  filter(participation_status == "Active") %>%
  filter(!pcr_target %in% c("RSV_A", "RSV_B")) %>%
  select(utility, pcr_target, measure_date, last_date, detection_status) %>%
  rbind(detectioncat)

#sars-cov-2
detectionstatus_summary_sars <- detectionstatus_summary %>%
  filter(pcr_target == "sars-cov-2") %>%
  mutate(utility = case_when(!is.na(measure_date) ~ paste0(utility, " (last submitted: ", measure_date, ")"),
                             is.na(measure_date) ~ paste0(utility, " (last submitted: ", last_date, ")"),
                             TRUE ~ utility)) %>%
  reshape2::dcast(utility ~ detection_status, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  select(-utility) %>%
  select(c(`Persistent detection`, `Detection`, `No recent detection`, `No recent data`)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate_all(~ na_if(., "NA (last submitted: NA)")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Function to remove NA and shift column up
shift_column_up <- function(column) {
  non_na_values <- column[!is.na(column)]
  length_diff <- length(column) - length(non_na_values)
  c(non_na_values, rep(NA, length_diff))
}

# Loop through each column and apply the function
for (col in names(detectionstatus_summary_sars)) {
  detectionstatus_summary_sars[[col]] <- shift_column_up(detectionstatus_summary_sars[[col]])
}





# panRSV
detectionstatus_summary_rsv <- detectionstatus_summary %>%
  filter(pcr_target == "PanRSV") %>%
  mutate(utility = case_when(!is.na(measure_date) ~ paste0(utility, " (last submitted: ", measure_date, ")"),
                             is.na(measure_date) ~ paste0(utility, " (last submitted: ", last_date, ")"),
                             TRUE ~ utility)) %>%
  reshape2::dcast(utility ~ detection_status, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  select(-utility) %>%
  select(c(`Persistent detection`, `Detection`, `No recent detection`, `No recent data`)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate_all(~ na_if(., "NA (last submitted: NA)")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Loop through each column and apply the function
for (col in names(detectionstatus_summary_rsv)) {
  detectionstatus_summary_rsv[[col]] <- shift_column_up(detectionstatus_summary_rsv[[col]])
}


# FluA
detectionstatus_summary_flua <- detectionstatus_summary %>%
  filter(pcr_target == "FLUAV") %>%
  mutate(utility = case_when(!is.na(measure_date) ~ paste0(utility, " (last submitted: ", measure_date, ")"),
                             is.na(measure_date) ~ paste0(utility, " (last submitted: ", last_date, ")"),
                             TRUE ~ utility)) %>%
  reshape2::dcast(utility ~ detection_status, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  select(-utility) %>%
  select(c(`Persistent detection`, `Detection`, `No recent detection`, `No recent data`)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate_all(~ na_if(., "NA (last submitted: NA)")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Loop through each column and apply the function
for (col in names(detectionstatus_summary_flua)) {
  detectionstatus_summary_flua[[col]] <- shift_column_up(detectionstatus_summary_flua[[col]])
}

# FluB
detectionstatus_summary_flub <- detectionstatus_summary %>%
  filter(pcr_target == "FLUBV") %>%
  mutate(utility = case_when(!is.na(measure_date) ~ paste0(utility, " (last submitted: ", measure_date, ")"),
                             is.na(measure_date) ~ paste0(utility, " (last submitted: ", last_date, ")"),
                             TRUE ~ utility)) %>%
  reshape2::dcast(utility ~ detection_status, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  select(-utility) %>%
  select(c(`Persistent detection`, `Detection`, `No recent detection`, `No recent data`)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate_all(~ na_if(., "NA (last submitted: NA)")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Loop through each column and apply the function
for (col in names(detectionstatus_summary_flub)) {
  detectionstatus_summary_flub[[col]] <- shift_column_up(detectionstatus_summary_flub[[col]])
}

# H5
detectionstatus_summary_h5 <- detectionstatus_summary %>%
  filter(pcr_target == "H5") %>%
  mutate(utility = case_when(!is.na(measure_date) ~ paste0(utility, " (last submitted: ", measure_date, ")"),
                             is.na(measure_date) ~ paste0(utility, " (last submitted: ", last_date, ")"),
                             TRUE ~ utility)) %>%
  reshape2::dcast(utility ~ detection_status, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  select(-utility) %>%
  select(c(`Persistent detection`, `Detection`, `No recent detection`, `No recent data`)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate_all(~ na_if(., "NA (last submitted: NA)")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Loop through each column and apply the function
for (col in names(detectionstatus_summary_h5)) {
  detectionstatus_summary_h5[[col]] <- shift_column_up(detectionstatus_summary_h5[[col]])
}

# H3
detectionstatus_summary_h3 <- detectionstatus_summary %>%
  filter(pcr_target == "H3") %>%
  mutate(utility = case_when(!is.na(measure_date) ~ paste0(utility, " (last submitted: ", measure_date, ")"),
                             is.na(measure_date) ~ paste0(utility, " (last submitted: ", last_date, ")"),
                             TRUE ~ utility)) %>%
  reshape2::dcast(utility ~ detection_status, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  select(-utility) %>%
  select(c(`Persistent detection`, `Detection`, `No recent detection`, `No recent data`)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate_all(~ na_if(., "NA (last submitted: NA)")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Loop through each column and apply the function
for (col in names(detectionstatus_summary_h3)) {
  detectionstatus_summary_h3[[col]] <- shift_column_up(detectionstatus_summary_h3[[col]])
}

# H1
detectionstatus_summary_h1 <- detectionstatus_summary %>%
  filter(pcr_target == "H1") %>%
  mutate(utility = case_when(!is.na(measure_date) ~ paste0(utility, " (last submitted: ", measure_date, ")"),
                             is.na(measure_date) ~ paste0(utility, " (last submitted: ", last_date, ")"),
                             TRUE ~ utility)) %>%
  reshape2::dcast(utility ~ detection_status, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  select(-utility) %>%
  select(c(`Persistent detection`, `Detection`, `No recent detection`, `No recent data`)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate_all(~ na_if(., "NA (last submitted: NA)")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Loop through each column and apply the function
for (col in names(detectionstatus_summary_h1)) {
  detectionstatus_summary_h1[[col]] <- shift_column_up(detectionstatus_summary_h1[[col]])
}

#print to excel sheet
#write trend summary for monthly submitters meeting
Style_PersistentDetection <- createStyle(fgFill="firebrick", fontColour = "white")
Style_Detection <- createStyle(fgFill="indianred", fontColour = "white")
Style_NoRecentDetection <- createStyle(fgFill="darkblue", fontColour = "white")
Style_NoRecentData <- createStyle(fgFill="black", fontColour = "white")
Style_noborder <- createStyle(borderColour = "white", border = "TopBottomLeftRight", borderStyle = "thick")



DetectionStatus_Workbook <- createWorkbook()
addWorksheet(DetectionStatus_Workbook, sheetName = "sars-cov-2") 
addStyle(DetectionStatus_Workbook, "sars-cov-2", Style_PersistentDetection, rows = 2, cols = 2)
addStyle(DetectionStatus_Workbook, "sars-cov-2", Style_Detection, rows = 2, cols = 3)
addStyle(DetectionStatus_Workbook, "sars-cov-2", Style_NoRecentDetection, rows = 2, cols = 4)
addStyle(DetectionStatus_Workbook, "sars-cov-2", Style_NoRecentData, rows = 2, cols = 5)
addStyle(DetectionStatus_Workbook, "sars-cov-2", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(DetectionStatus_Workbook, "sars-cov-2", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(DetectionStatus_Workbook, "sars-cov-2", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(DetectionStatus_Workbook, "sars-cov-2", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(DetectionStatus_Workbook, "sars-cov-2", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(DetectionStatus_Workbook, "sars-cov-2", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(DetectionStatus_Workbook, "sars-cov-2", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
writeData(DetectionStatus_Workbook, "sars-cov-2", detectionstatus_summary_sars, startCol = 2,startRow = 2) 

addWorksheet(DetectionStatus_Workbook, sheetName = "PanRSV") 
addStyle(DetectionStatus_Workbook, "PanRSV", Style_PersistentDetection, rows = 2, cols = 2)
addStyle(DetectionStatus_Workbook, "PanRSV", Style_Detection, rows = 2, cols = 3)
addStyle(DetectionStatus_Workbook, "PanRSV", Style_NoRecentDetection, rows = 2, cols = 4)
addStyle(DetectionStatus_Workbook, "PanRSV", Style_NoRecentData, rows = 2, cols = 5)
addStyle(DetectionStatus_Workbook, "PanRSV", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(DetectionStatus_Workbook, "PanRSV", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(DetectionStatus_Workbook, "PanRSV", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(DetectionStatus_Workbook, "PanRSV", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(DetectionStatus_Workbook, "PanRSV", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(DetectionStatus_Workbook, "PanRSV", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(DetectionStatus_Workbook, "PanRSV", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
writeData(DetectionStatus_Workbook, "PanRSV", detectionstatus_summary_rsv, startCol = 2,startRow = 2) 

addWorksheet(DetectionStatus_Workbook, sheetName = "Flu_A") 
addStyle(DetectionStatus_Workbook, "Flu_A", Style_PersistentDetection, rows = 2, cols = 2)
addStyle(DetectionStatus_Workbook, "Flu_A", Style_Detection, rows = 2, cols = 3)
addStyle(DetectionStatus_Workbook, "Flu_A", Style_NoRecentDetection, rows = 2, cols = 4)
addStyle(DetectionStatus_Workbook, "Flu_A", Style_NoRecentData, rows = 2, cols = 5)
addStyle(DetectionStatus_Workbook, "Flu_A", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(DetectionStatus_Workbook, "Flu_A", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(DetectionStatus_Workbook, "Flu_A", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(DetectionStatus_Workbook, "Flu_A", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(DetectionStatus_Workbook, "Flu_A", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(DetectionStatus_Workbook, "Flu_A", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(DetectionStatus_Workbook, "Flu_A", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
writeData(DetectionStatus_Workbook, "Flu_A", detectionstatus_summary_flua, startCol = 2,startRow = 2) 

addWorksheet(DetectionStatus_Workbook, sheetName = "Flu_B") 
addStyle(DetectionStatus_Workbook, "Flu_B", Style_PersistentDetection, rows = 2, cols = 2)
addStyle(DetectionStatus_Workbook, "Flu_B", Style_Detection, rows = 2, cols = 3)
addStyle(DetectionStatus_Workbook, "Flu_B", Style_NoRecentDetection, rows = 2, cols = 4)
addStyle(DetectionStatus_Workbook, "Flu_B", Style_NoRecentData, rows = 2, cols = 5)
addStyle(DetectionStatus_Workbook, "Flu_B", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(DetectionStatus_Workbook, "Flu_B", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(DetectionStatus_Workbook, "Flu_B", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(DetectionStatus_Workbook, "Flu_B", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(DetectionStatus_Workbook, "Flu_B", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(DetectionStatus_Workbook, "Flu_B", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(DetectionStatus_Workbook, "Flu_B", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
writeData(DetectionStatus_Workbook, "Flu_B", detectionstatus_summary_flub, startCol = 2,startRow = 2) 

addWorksheet(DetectionStatus_Workbook, sheetName = "H5") 
addStyle(DetectionStatus_Workbook, "H5", Style_PersistentDetection, rows = 2, cols = 2)
addStyle(DetectionStatus_Workbook, "H5", Style_Detection, rows = 2, cols = 3)
addStyle(DetectionStatus_Workbook, "H5", Style_NoRecentDetection, rows = 2, cols = 4)
addStyle(DetectionStatus_Workbook, "H5", Style_NoRecentData, rows = 2, cols = 5)
addStyle(DetectionStatus_Workbook, "H5", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(DetectionStatus_Workbook, "H5", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(DetectionStatus_Workbook, "H5", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(DetectionStatus_Workbook, "H5", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(DetectionStatus_Workbook, "H5", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(DetectionStatus_Workbook, "H5", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(DetectionStatus_Workbook, "H5", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
writeData(DetectionStatus_Workbook, "H5", detectionstatus_summary_h5, startCol = 2,startRow = 2) 

addWorksheet(DetectionStatus_Workbook, sheetName = "H3") 
addStyle(DetectionStatus_Workbook, "H3", Style_PersistentDetection, rows = 2, cols = 2)
addStyle(DetectionStatus_Workbook, "H3", Style_Detection, rows = 2, cols = 3)
addStyle(DetectionStatus_Workbook, "H3", Style_NoRecentDetection, rows = 2, cols = 4)
addStyle(DetectionStatus_Workbook, "H3", Style_NoRecentData, rows = 2, cols = 5)
addStyle(DetectionStatus_Workbook, "H3", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(DetectionStatus_Workbook, "H3", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(DetectionStatus_Workbook, "H3", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(DetectionStatus_Workbook, "H3", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(DetectionStatus_Workbook, "H3", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(DetectionStatus_Workbook, "H3", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(DetectionStatus_Workbook, "H3", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
writeData(DetectionStatus_Workbook, "H3", detectionstatus_summary_h3, startCol = 2,startRow = 2) 

addWorksheet(DetectionStatus_Workbook, sheetName = "H1") 
addStyle(DetectionStatus_Workbook, "H1", Style_PersistentDetection, rows = 2, cols = 2)
addStyle(DetectionStatus_Workbook, "H1", Style_Detection, rows = 2, cols = 3)
addStyle(DetectionStatus_Workbook, "H1", Style_NoRecentDetection, rows = 2, cols = 4)
addStyle(DetectionStatus_Workbook, "H1", Style_NoRecentData, rows = 2, cols = 5)
addStyle(DetectionStatus_Workbook, "H1", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(DetectionStatus_Workbook, "H1", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(DetectionStatus_Workbook, "H1", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(DetectionStatus_Workbook, "H1", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(DetectionStatus_Workbook, "H1", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(DetectionStatus_Workbook, "H1", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(DetectionStatus_Workbook, "H1", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
writeData(DetectionStatus_Workbook, "H1", detectionstatus_summary_h1, startCol = 2,startRow = 2) 


#write workbook
saveWorkbook(DetectionStatus_Workbook, paste0("output/trends/detection_status/detectionstatus_summary_", today(), ".xlsx"), overwrite=TRUE) 




##########################################################
####    Data Summary Sheet for Submitters Meeting      ###
##########################################################

pcr_targets <- unique(IDB_masterlist$pcr_target)

detectioncat <- data.frame(
  pcr_target = rep(unique(IDB_masterlist$pcr_target), each = 4),  # Repeat each pcr_target 4 times
  detection_status = rep(c("Persistent detection", 
                           "Detection", 
                           "No recent detection", 
                           "No recent data"), 
                         times = length(unique(IDB_masterlist$pcr_target))),  # Repeat statuses for each pcr_target
  utility = NA,
  measure_date = NA,
  last_date = NA
)

PFD_detectionstatus <- IDB_detectionstatus %>%
  filter(utility %in% PFD_masterlist$wwtp_name)

last_available_date <- PFD_detectionstatus %>%
  group_by(utility, pcr_target) %>%
  arrange(utility, pcr_target, desc(measure_date)) %>%
  slice_head(n = 1) %>% #slice for latest submission date per utility/pcr_target
  filter(!week == 0) %>% #remove utilities we've received a sample from this week
  select(utility, pcr_target, measure_date) %>%
  rename(last_date = measure_date)

Public_detectionstatus_summary <- PFD_detectionstatus %>%
  ungroup() %>%
  mutate(measure_date = as.Date(measure_date)) %>%
  left_join(last_available_date, by = c("utility", "pcr_target"), relationship = "many-to-many") %>%
  group_by(utility, pcr_target) %>%
  arrange(utility, pcr_target, week, desc(measure_date)) %>%
  slice_head(n = 1) %>% #slice for latest status per utility/pcr_target
  ungroup() %>%
  full_join(IDB_masterlist, by = c("utility" = "wwtp_name", "pcr_target")) %>%
  filter(Surv_System %in% c("SSS", "ESS", "FSS")) %>%
  filter(participation_status == "Active") %>%
  filter(!pcr_target %in% c("RSV_A", "RSV_B")) %>%
  select(utility, pcr_target, measure_date, last_date, detection_status) %>%
  rbind(detectioncat)


#sars-cov-2
Public_detectionstatus_summary_sars <- Public_detectionstatus_summary %>%
  filter(pcr_target == "sars-cov-2") %>%
  mutate(utility = case_when(!is.na(measure_date) ~ paste0(utility, " (last submitted: ", measure_date, ")"),
                             is.na(measure_date) ~ paste0(utility, " (last submitted: ", last_date, ")"),
                             TRUE ~ utility)) %>%
  reshape2::dcast(utility ~ detection_status, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  select(-utility) %>%
  select(c(`Persistent detection`, `Detection`, `No recent detection`, `No recent data`)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate_all(~ na_if(., "NA (last submitted: NA)")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Function to remove NA and shift column up
shift_column_up <- function(column) {
  non_na_values <- column[!is.na(column)]
  length_diff <- length(column) - length(non_na_values)
  c(non_na_values, rep(NA, length_diff))
}

# Loop through each column and apply the function
for (col in names(Public_detectionstatus_summary_sars)) {
  Public_detectionstatus_summary_sars[[col]] <- shift_column_up(Public_detectionstatus_summary_sars[[col]])
}





# panRSV
Public_detectionstatus_summary_rsv <- Public_detectionstatus_summary %>%
  filter(pcr_target == "PanRSV") %>%
  mutate(utility = case_when(!is.na(measure_date) ~ paste0(utility, " (last submitted: ", measure_date, ")"),
                             is.na(measure_date) ~ paste0(utility, " (last submitted: ", last_date, ")"),
                             TRUE ~ utility)) %>%
  reshape2::dcast(utility ~ detection_status, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  select(-utility) %>%
  select(c(`Persistent detection`, `Detection`, `No recent detection`, `No recent data`)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate_all(~ na_if(., "NA (last submitted: NA)")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Loop through each column and apply the function
for (col in names(Public_detectionstatus_summary_rsv)) {
  Public_detectionstatus_summary_rsv[[col]] <- shift_column_up(Public_detectionstatus_summary_rsv[[col]])
}



# FluA
Public_detectionstatus_summary_flua <- Public_detectionstatus_summary %>%
  filter(pcr_target == "FLUAV") %>%
  mutate(utility = case_when(!is.na(measure_date) ~ paste0(utility, " (last submitted: ", measure_date, ")"),
                             is.na(measure_date) ~ paste0(utility, " (last submitted: ", last_date, ")"),
                             TRUE ~ utility)) %>%
  reshape2::dcast(utility ~ detection_status, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  select(-utility) %>%
  select(c(`Persistent detection`, `Detection`, `No recent detection`, `No recent data`)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate_all(~ na_if(., "NA (last submitted: NA)")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Loop through each column and apply the function
for (col in names(Public_detectionstatus_summary_flua)) {
  Public_detectionstatus_summary_flua[[col]] <- shift_column_up(Public_detectionstatus_summary_flua[[col]])
}

# FluB
Public_detectionstatus_summary_flub <- Public_detectionstatus_summary %>%
  filter(pcr_target == "FLUBV") %>%
  mutate(utility = case_when(!is.na(measure_date) ~ paste0(utility, " (last submitted: ", measure_date, ")"),
                             is.na(measure_date) ~ paste0(utility, " (last submitted: ", last_date, ")"),
                             TRUE ~ utility)) %>%
  reshape2::dcast(utility ~ detection_status, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  select(-utility) %>%
  select(c(`Persistent detection`, `Detection`, `No recent detection`, `No recent data`)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate_all(~ na_if(., "NA (last submitted: NA)")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Loop through each column and apply the function
for (col in names(Public_detectionstatus_summary_flub)) {
  Public_detectionstatus_summary_flub[[col]] <- shift_column_up(Public_detectionstatus_summary_flub[[col]])
}

# H5
Public_detectionstatus_summary_h5 <- Public_detectionstatus_summary %>%
  filter(pcr_target == "H5") %>%
  mutate(utility = case_when(!is.na(measure_date) ~ paste0(utility, " (last submitted: ", measure_date, ")"),
                             is.na(measure_date) ~ paste0(utility, " (last submitted: ", last_date, ")"),
                             TRUE ~ utility)) %>%
  reshape2::dcast(utility ~ detection_status, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  select(-utility) %>%
  select(c(`Persistent detection`, `Detection`, `No recent detection`, `No recent data`)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate_all(~ na_if(., "NA (last submitted: NA)")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Loop through each column and apply the function
for (col in names(Public_detectionstatus_summary_h5)) {
  Public_detectionstatus_summary_h5[[col]] <- shift_column_up(Public_detectionstatus_summary_h5[[col]])
}

# H3
Public_detectionstatus_summary_h3 <- Public_detectionstatus_summary %>%
  filter(pcr_target == "H3") %>%
  mutate(utility = case_when(!is.na(measure_date) ~ paste0(utility, " (last submitted: ", measure_date, ")"),
                             is.na(measure_date) ~ paste0(utility, " (last submitted: ", last_date, ")"),
                             TRUE ~ utility)) %>%
  reshape2::dcast(utility ~ detection_status, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  select(-utility) %>%
  select(c(`Persistent detection`, `Detection`, `No recent detection`, `No recent data`)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate_all(~ na_if(., "NA (last submitted: NA)")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Loop through each column and apply the function
for (col in names(Public_detectionstatus_summary_h3)) {
  Public_detectionstatus_summary_h3[[col]] <- shift_column_up(Public_detectionstatus_summary_h3[[col]])
}

# H1
Public_detectionstatus_summary_h1 <- Public_detectionstatus_summary %>%
  filter(pcr_target == "H1") %>%
  mutate(utility = case_when(!is.na(measure_date) ~ paste0(utility, " (last submitted: ", measure_date, ")"),
                             is.na(measure_date) ~ paste0(utility, " (last submitted: ", last_date, ")"),
                             TRUE ~ utility)) %>%
  reshape2::dcast(utility ~ detection_status, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  select(-utility) %>%
  select(c(`Persistent detection`, `Detection`, `No recent detection`, `No recent data`)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate_all(~ na_if(., "NA (last submitted: NA)")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Loop through each column and apply the function
for (col in names(Public_detectionstatus_summary_h1)) {
  Public_detectionstatus_summary_h1[[col]] <- shift_column_up(Public_detectionstatus_summary_h1[[col]])
}


#print to excel sheet
#write trend summary for monthly submitters meeting
Style_PersistentDetection <- createStyle(fgFill="firebrick", fontColour = "white")
Style_Detection <- createStyle(fgFill="indianred", fontColour = "white")
Style_NoRecentDetection <- createStyle(fgFill="darkblue", fontColour = "white")
Style_NoRecentData <- createStyle(fgFill="black", fontColour = "white")
Style_noborder <- createStyle(borderColour = "white", border = "TopBottomLeftRight", borderStyle = "thick")



Public_DetectionStatus_Workbook <- createWorkbook()
addWorksheet(Public_DetectionStatus_Workbook, sheetName = "sars-cov-2") 
addStyle(Public_DetectionStatus_Workbook, "sars-cov-2", Style_PersistentDetection, rows = 2, cols = 2)
addStyle(Public_DetectionStatus_Workbook, "sars-cov-2", Style_Detection, rows = 2, cols = 3)
addStyle(Public_DetectionStatus_Workbook, "sars-cov-2", Style_NoRecentDetection, rows = 2, cols = 4)
addStyle(Public_DetectionStatus_Workbook, "sars-cov-2", Style_NoRecentData, rows = 2, cols = 5)
addStyle(Public_DetectionStatus_Workbook, "sars-cov-2", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(Public_DetectionStatus_Workbook, "sars-cov-2", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(Public_DetectionStatus_Workbook, "sars-cov-2", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(Public_DetectionStatus_Workbook, "sars-cov-2", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(Public_DetectionStatus_Workbook, "sars-cov-2", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(Public_DetectionStatus_Workbook, "sars-cov-2", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(Public_DetectionStatus_Workbook, "sars-cov-2", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
writeData(Public_DetectionStatus_Workbook, "sars-cov-2", Public_detectionstatus_summary_sars, startCol = 2,startRow = 2) 

addWorksheet(Public_DetectionStatus_Workbook, sheetName = "PanRSV") 
addStyle(Public_DetectionStatus_Workbook, "PanRSV", Style_PersistentDetection, rows = 2, cols = 2)
addStyle(Public_DetectionStatus_Workbook, "PanRSV", Style_Detection, rows = 2, cols = 3)
addStyle(Public_DetectionStatus_Workbook, "PanRSV", Style_NoRecentDetection, rows = 2, cols = 4)
addStyle(Public_DetectionStatus_Workbook, "PanRSV", Style_NoRecentData, rows = 2, cols = 5)
addStyle(Public_DetectionStatus_Workbook, "PanRSV", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(Public_DetectionStatus_Workbook, "PanRSV", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(Public_DetectionStatus_Workbook, "PanRSV", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(Public_DetectionStatus_Workbook, "PanRSV", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(Public_DetectionStatus_Workbook, "PanRSV", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(Public_DetectionStatus_Workbook, "PanRSV", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(Public_DetectionStatus_Workbook, "PanRSV", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
writeData(Public_DetectionStatus_Workbook, "PanRSV", Public_detectionstatus_summary_rsv, startCol = 2,startRow = 2) 

addWorksheet(Public_DetectionStatus_Workbook, sheetName = "Flu_A") 
addStyle(Public_DetectionStatus_Workbook, "Flu_A", Style_PersistentDetection, rows = 2, cols = 2)
addStyle(Public_DetectionStatus_Workbook, "Flu_A", Style_Detection, rows = 2, cols = 3)
addStyle(Public_DetectionStatus_Workbook, "Flu_A", Style_NoRecentDetection, rows = 2, cols = 4)
addStyle(Public_DetectionStatus_Workbook, "Flu_A", Style_NoRecentData, rows = 2, cols = 5)
addStyle(Public_DetectionStatus_Workbook, "Flu_A", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(Public_DetectionStatus_Workbook, "Flu_A", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(Public_DetectionStatus_Workbook, "Flu_A", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(Public_DetectionStatus_Workbook, "Flu_A", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(Public_DetectionStatus_Workbook, "Flu_A", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(Public_DetectionStatus_Workbook, "Flu_A", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(Public_DetectionStatus_Workbook, "Flu_A", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
writeData(Public_DetectionStatus_Workbook, "Flu_A", Public_detectionstatus_summary_flua, startCol = 2,startRow = 2) 

addWorksheet(Public_DetectionStatus_Workbook, sheetName = "Flu_B") 
addStyle(Public_DetectionStatus_Workbook, "Flu_B", Style_PersistentDetection, rows = 2, cols = 2)
addStyle(Public_DetectionStatus_Workbook, "Flu_B", Style_Detection, rows = 2, cols = 3)
addStyle(Public_DetectionStatus_Workbook, "Flu_B", Style_NoRecentDetection, rows = 2, cols = 4)
addStyle(Public_DetectionStatus_Workbook, "Flu_B", Style_NoRecentData, rows = 2, cols = 5)
addStyle(Public_DetectionStatus_Workbook, "Flu_B", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(Public_DetectionStatus_Workbook, "Flu_B", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(Public_DetectionStatus_Workbook, "Flu_B", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(Public_DetectionStatus_Workbook, "Flu_B", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(Public_DetectionStatus_Workbook, "Flu_B", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(Public_DetectionStatus_Workbook, "Flu_B", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(Public_DetectionStatus_Workbook, "Flu_B", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
writeData(Public_DetectionStatus_Workbook, "Flu_B", Public_detectionstatus_summary_flub, startCol = 2,startRow = 2) 

addWorksheet(Public_DetectionStatus_Workbook, sheetName = "H5") 
addStyle(Public_DetectionStatus_Workbook, "H5", Style_PersistentDetection, rows = 2, cols = 2)
addStyle(Public_DetectionStatus_Workbook, "H5", Style_Detection, rows = 2, cols = 3)
addStyle(Public_DetectionStatus_Workbook, "H5", Style_NoRecentDetection, rows = 2, cols = 4)
addStyle(Public_DetectionStatus_Workbook, "H5", Style_NoRecentData, rows = 2, cols = 5)
addStyle(Public_DetectionStatus_Workbook, "H5", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(Public_DetectionStatus_Workbook, "H5", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(Public_DetectionStatus_Workbook, "H5", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(Public_DetectionStatus_Workbook, "H5", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(Public_DetectionStatus_Workbook, "H5", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(Public_DetectionStatus_Workbook, "H5", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(Public_DetectionStatus_Workbook, "H5", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
writeData(Public_DetectionStatus_Workbook, "H5", Public_detectionstatus_summary_h5, startCol = 2,startRow = 2) 

addWorksheet(Public_DetectionStatus_Workbook, sheetName = "H3") 
addStyle(Public_DetectionStatus_Workbook, "H3", Style_PersistentDetection, rows = 2, cols = 2)
addStyle(Public_DetectionStatus_Workbook, "H3", Style_Detection, rows = 2, cols = 3)
addStyle(Public_DetectionStatus_Workbook, "H3", Style_NoRecentDetection, rows = 2, cols = 4)
addStyle(Public_DetectionStatus_Workbook, "H3", Style_NoRecentData, rows = 2, cols = 5)
addStyle(Public_DetectionStatus_Workbook, "H3", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(Public_DetectionStatus_Workbook, "H3", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(Public_DetectionStatus_Workbook, "H3", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(Public_DetectionStatus_Workbook, "H3", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(Public_DetectionStatus_Workbook, "H3", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(Public_DetectionStatus_Workbook, "H3", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(Public_DetectionStatus_Workbook, "H3", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
writeData(Public_DetectionStatus_Workbook, "H3", Public_detectionstatus_summary_h3, startCol = 2,startRow = 2) 

addWorksheet(Public_DetectionStatus_Workbook, sheetName = "H1") 
addStyle(Public_DetectionStatus_Workbook, "H1", Style_PersistentDetection, rows = 2, cols = 2)
addStyle(Public_DetectionStatus_Workbook, "H1", Style_Detection, rows = 2, cols = 3)
addStyle(Public_DetectionStatus_Workbook, "H1", Style_NoRecentDetection, rows = 2, cols = 4)
addStyle(Public_DetectionStatus_Workbook, "H1", Style_NoRecentData, rows = 2, cols = 5)
addStyle(Public_DetectionStatus_Workbook, "H1", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(Public_DetectionStatus_Workbook, "H1", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(Public_DetectionStatus_Workbook, "H1", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(Public_DetectionStatus_Workbook, "H1", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(Public_DetectionStatus_Workbook, "H1", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(Public_DetectionStatus_Workbook, "H1", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(Public_DetectionStatus_Workbook, "H1", Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
writeData(Public_DetectionStatus_Workbook, "H1", Public_detectionstatus_summary_h1, startCol = 2,startRow = 2) 

#write workbook
saveWorkbook(Public_DetectionStatus_Workbook, paste0("output/trends/detection_status/detectionstatus_summary_Public_", today(), ".xlsx"), overwrite=TRUE) 

