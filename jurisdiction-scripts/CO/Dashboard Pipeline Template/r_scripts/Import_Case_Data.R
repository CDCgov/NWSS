print("### Running Import Case Data ###")


###################################################################
##                    Historic Cases for COVID                   ##
###################################################################
print("Importing historic case data for Sars-CoV-2")

#read in old case data before DLH began
historic_casedata <- read.csv("import/clinical data/CaseLineListData_Historic.csv") %>%
  mutate(collectiondate = as.Date(collectiondate, format = "%m/%d/%Y")) %>%
  filter(collectiondate < "2024-03-01") #filtering to be sure to keep DLH cases after 3/1/2024


###################################################################
##                    DLH Cases for COVID                        ##
###################################################################

#read in DLH case data
print("Importing DLH case data for Sars-CoV-2")

## If you don't have access to Google, there is another copy of this data saved import/DLHCaseData_Sars-CoV-2.csv
# DLHcasedata <- read.csv("import/clinical data/DLHCaseData_Sars-CoV-2.csv") %>% select(-X)
DLHcasedata <- read_sheet("https://docs.google.com/spreadsheets/d/1WE33232ur1ICjtCw5LAlifa8feTw1KzjC_eSgejJTME/edit?gid=0#gid=0")
  
# #pull timestamp from google drive sheet to verify DLH automation didn't run into issues
file_info <- drive_ls(as_id((drive_get("NWSS Template Mock Data")$id))) %>%
  filter(name == "Mock DLH Clinical Data") %>%
  mutate(
    modified = map_chr(drive_resource, "modifiedTime"),
    modified_mt = with_tz(ymd_hms(modified), tzone = "America/Denver")) %>%
  mutate(date = substr(modified_mt, 1, 10)) %>% #determine date last modified
  mutate(dayofweek = weekdays(as.Date(date))) %>% #determine day of week
  mutate(time = strptime(substr(modified_mt, 12, 16), format = "%H:%M")) %>%
  mutate(time = substr(time, 12, 16))

# Print last modification time
paste("DLH Case Data Last Updated:         ", file_info$dayofweek, file_info$date, " @ ", file_info$time)

#combine DLH data with the historic data
DLHcasedata_sars <- historic_casedata %>%
  rename(reported_date = collectiondate,
         case_status = casestatus, 
         county = countyassigned,
         caused_hospital = hospitalizedyesno) %>%
  mutate(admission_date = reported_date) %>%
  select(reported_date, admission_date, case_status, 
         county, caused_hospital) %>%
  rbind(DLHcasedata)

#output to working data folder to use throughout pipeline
write.csv(DLHcasedata_sars, "output/working data files/CaseLineListData_Sars-CoV-2.csv")




###################################################################
##          FLU AND RSV CASE DATA (Sentinel/Syndromic)           ##
###################################################################

print("Connecting to Google Drive and downloading Flu & RSV Sentinel / Syndromic case data")

## If you don't have access to Google, there is another copy of this data saved import/CaseLineListData_FluRSV_SentinelSyndromic_Cases.csv
flursv_cases <- read_sheet("https://docs.google.com/spreadsheets/d/1-ko4b3fuA0PiRLI2wWrfPtXjVtN2nhc2IJ70PA51SWg/edit?gid=0#gid=0")

# #pull timestamp from google drive sheet to verify DLH automation didn't run into issues
file_info <- drive_ls(as_id((drive_get("NWSS Template Mock Data")$id))) %>%
  filter(name == "Mock Flu RSV Sentinel Syndromic Cases") %>%
  mutate(
    modified = map_chr(drive_resource, "modifiedTime"),
    modified_mt = with_tz(ymd_hms(modified), tzone = "America/Denver")) %>%
  mutate(date = substr(modified_mt, 1, 10)) %>% #determine date last modified
  mutate(dayofweek = weekdays(as.Date(date))) %>% #determine day of week
  mutate(time = strptime(substr(modified_mt, 12, 16), format = "%H:%M")) %>%
  mutate(time = substr(time, 12, 16))

# Print last modification time
paste("Flu / RSV Sentinel & Syndromic Data Last Updated:         ", file_info$dayofweek, file_info$date, " @ ", file_info$time)

#write csv to working data files
write.csv(flursv_cases, "output/working data files/CaseLineListData_FluRSV_SentinelSyndromic_Cases.csv")



###################################################################
##          FLU AND RSV CASE DATA (Hospitalized)           ##
###################################################################

print("Connecting to Google and downloading Flu & RSV Hospitalization Data")

## If you don't have access to Google, there is another copy of this data saved import/CaseLineListData_FluRSV_Hospitalizations.csv
DLH_flursv_hosp <- read_sheet("https://docs.google.com/spreadsheets/d/1ZbEZWmzk1EQIMDBp5T6zKNtv4_DM0G17tdoWmI3AMdg/edit?gid=0#gid=0")

# #pull timestamp from google drive sheet to verify DLH automation didn't run into issues
file_info <- drive_ls(as_id((drive_get("NWSS Template Mock Data")$id))) %>%
  filter(name == "Mock Flu RSV Hospitalizations") %>%
  mutate(
    modified = map_chr(drive_resource, "modifiedTime"),
    modified_mt = with_tz(ymd_hms(modified), tzone = "America/Denver")) %>%
  mutate(date = substr(modified_mt, 1, 10)) %>% #determine date last modified
  mutate(dayofweek = weekdays(as.Date(date))) %>% #determine day of week
  mutate(time = strptime(substr(modified_mt, 12, 16), format = "%H:%M")) %>%
  mutate(time = substr(time, 12, 16))

# Print last modification time
paste("Flu/RSV Hospitalization Data Last Updated:         ", file_info$dayofweek, file_info$date, " @ ", file_info$time)


DLH_flursv_hosp <- DLH_flursv_hosp %>%
  rename(weekly_case_count = COUNT) %>%
  mutate(county = str_to_title(county))



#save to K drive
write.csv(DLH_flursv_hosp, "output/working data files/CaseLineListData_FluRSV_Hospitalizations.csv")



