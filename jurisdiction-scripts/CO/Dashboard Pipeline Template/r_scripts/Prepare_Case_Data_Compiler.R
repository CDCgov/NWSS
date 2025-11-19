print("### Running Prepare Case Data Compiler ###")

##################################################################
####                       READ IN DATA                       ####    
##################################################################

#Step 1: Read in WW Data----
##List wastewater files
VirusData <- list.files(path = "output/compiled_data") %>%
  as.data.frame() %>%
  `colnames<-`("filename") %>%
  filter(grepl("3_CDPHE_Wastewater_Data_",filename,fixed = TRUE)) %>%
  mutate(date = str_remove(filename, "3_CDPHE_Wastewater_Data_") %>%
           str_remove(".csv") %>%
           ymd()) %>%
  filter(date == max(date)) %>%
  dplyr::select(filename) %>%
  unlist() %>%
  paste0("output/compiled_data/",.) %>%
  read_csv()

VirusData <- VirusData %>%
  filter(is.na(sample_collect_date)==FALSE) %>%
  transmute(Utility =  wwtp_name,
            Date = sample_collect_date,
            pcr_target, 
            "Viral Copies/L (uncorrected)" = pcr_target_avg_conc,
            pcr_target_cl_95_lo,
            pcr_target_cl_95_up,
            extraction_method = extraction_method,
            sample_id,
            quality_flag,
            flow_rate,
            sample_type,
            test_result_date = test_result_date,
            county = as.character(county_names),
            lab_phase,
            pcr_target_below_lod) %>%
  mutate("Log_Viral Copies/L (uncorrected)" = log10(`Viral Copies/L (uncorrected)`)) %>%
  filter(is.na(Date)==FALSE)

#Step 2: Read in reference  Data----
Crosswalk <- read_xlsx("reference_sheets/Crosswalk.xlsx") %>%
  mutate(sewershed_name_in_case_linelist = toupper(str_squish(sewershed_name_in_case_linelist)))

Crosswalk <- Crosswalk %>% 
  dplyr::select(-site_id)

#grab sewershed cases for internal dashboard output
Output <- VirusData %>%
  group_by(Utility,county, pcr_target) %>%
  complete(Date = seq.Date(min(VirusData$Date)-14, to = Sys.Date(), by="day")) %>%
  fill(Utility,county, pcr_target) %>%
  ungroup() %>%
  left_join(Crosswalk,by=c("Utility" = "wwtp_name_nwss_dcipher"), relationship = "many-to-many") %>%
  dplyr::select(-c(epaid, epa_registry_id))






#########################################
###          COVID CASE DATA          ###
#########################################

#Step 3: Get County Level data------
##COVID
CountyLvl_CaseData_COVID <- read.csv("output/working data files/CaseLineListData_SARS-CoV-2.csv") %>%
  rename(County = county,
         Date = reported_date,
         Hospitalized = caused_hospital,
         Hosp_admin_date = admission_date,
         casestatus = case_status) %>%
  mutate(pcr_target = "sars-cov-2")

CountyLvl_CaseData_COVID$Date <- as.Date(CountyLvl_CaseData_COVID$Date, format = "%Y-%m-%d")
CountyLvl_CaseData_COVID$Hosp_admin_date <- as.Date(CountyLvl_CaseData_COVID$Hosp_admin_date, format = "%Y-%m-%d")


#create county cases dataframe
County_Cases_COVID <- CountyLvl_CaseData_COVID %>%
  filter(casestatus == "Confirmed") %>%
  group_by(pcr_target, County, Date) %>%
  summarize(`Raw County Cases` = n()) %>%
  filter(!is.na(County))


#create count hospitalizations dataframe
County_Hospitalizations_COVID <- CountyLvl_CaseData_COVID %>%
  filter(casestatus == "Confirmed", 
         Hospitalized == "Yes") %>%
  group_by(pcr_target, County, Hosp_admin_date) %>%
  summarize(`Raw Hospitalized Cases` = n()) %>%
  rename(Date = Hosp_admin_date) 



#county cases + hospitalizations
Combined_CountyCases_COVID <- County_Cases_COVID %>%
  group_by(County, pcr_target) %>%
  complete(Date = seq.Date(min(VirusData$Date)-14, to = Sys.Date(), by="day")) %>%
  fill(County) %>%
  full_join(County_Hospitalizations_COVID, by = c("pcr_target", "County", "Date"), relationship = "many-to-many") %>%
  mutate(`Raw County Cases` = case_when(is.na(`Raw County Cases`) ~ 0, 
                                        TRUE ~ `Raw County Cases`)) %>%
  mutate(`Raw Hospitalized Cases` = case_when(is.na(`Raw Hospitalized Cases`) ~ 0, 
                                              TRUE ~ `Raw Hospitalized Cases`)) %>%
  arrange(pcr_target, County, Date) %>%
  mutate(cases_3dayavg = zoo::rollmean(`Raw County Cases`,
                                       k = 3,
                                       fill = NA,
                                       align = "center")) %>%
  relocate(pcr_target, .before = County) %>%
  mutate(hosp_3dayavg = zoo::rollmean(`Raw Hospitalized Cases`,
                                      k = 3,
                                      fill = NA,
                                      align = "center")) %>%
  relocate(`Raw Hospitalized Cases`, .before = hosp_3dayavg) %>%
  mutate(Year = lubridate::year(Date)) %>%
  left_join(countypopdata, by=c("County", "Year")) %>%
  select(-c(Year)) %>%
  filter(!is.na(County))

##create county cases per 100k and filter for separate output 
Combined_CountyCases_output_COVID <- Combined_CountyCases_COVID %>%
  mutate(County_raw_cases_r100Kutil = (`Raw County Cases`/countypop)*100000,
         County_cases_3dayavg_r100Kutil = (cases_3dayavg/countypop)*100000,
         County_hosp_raw_r100Kutil = (`Raw Hospitalized Cases`/countypop)*100000,
         County_hosp_3dayavg_r100Kutil = (hosp_3dayavg/countypop)*100000) %>%
  arrange(County, Date) %>%
  select(pcr_target, County, Date, `Raw County Cases`, cases_3dayavg, County_cases_3dayavg_r100Kutil, `Raw Hospitalized Cases`, hosp_3dayavg, County_hosp_3dayavg_r100Kutil)

write_csv(Combined_CountyCases_output_COVID, "dashboards/InternalDashboard/data/CountyCases_Sars-CoV-2.csv")






#########################################
###            FLU/RSV DATA           ###
#########################################

#Syndromic RSV/Flu Case Data
#calculate RSV/Flu per 100,000 population (no need to calculate 3-day average as data is reported weekly)
CountyLvl_CaseData_FluRSV <- read.csv("output/working data files/CaseLineListData_FluRSV_SentinelSyndromic_Cases.csv")  %>%
  mutate(Date = ymd(week_begin_date)) %>%
  filter(!is.na(Date)) %>%
  select(-c(week_begin_date, Week, X)) %>%
  relocate(Date, .after = County) %>%
  rename(Diagnosed_Flu = Diagnosed.Flu) %>%
  rename(Total_ED_Visits = Total.ED.Visits)

Combined_CountyCases_output_FluRSV <- CountyLvl_CaseData_FluRSV %>%
  left_join(countypopdata, by=c("County", "Year")) %>%
  select(-c(Year)) %>%
  filter(!is.na(County)) %>%
  rename(Diagnosed_RSV = Diagnosed.RSV,
         ILI_neg_COVID = ILI.neg.COVID) %>%
  mutate(Diagnosed_Flu_r100Kutil = (Diagnosed_Flu/countypop)*100000,
         Diagnosed_RSV_r100Kutil = (Diagnosed_RSV/countypop)*100000,
         ILI_neg_COVID_r100Kutil = (ILI_neg_COVID/countypop)*100000,
         Percent_Diag_Flu = (Diagnosed_Flu/Total_ED_Visits)*100,
         Percent_Diag_RSV =  (Diagnosed_RSV / Total_ED_Visits)*100,
         Percent_Diag_ILINegCOVID =  (ILI_neg_COVID / Total_ED_Visits)*100,
         Percent_Diag_Flu_r100kutil = (Percent_Diag_Flu / countypop)*100000,
         Percent_Diag_RSV_r100kutil = (Percent_Diag_RSV / countypop)*100000,
         Percent_Diag_ILINegCOVID_r100kutil = (Percent_Diag_ILINegCOVID / countypop)*100000) %>%
  arrange(County, Date) %>%
  select(County, Date, Diagnosed_Flu, Diagnosed_Flu_r100Kutil, Percent_Diag_Flu, Percent_Diag_Flu_r100kutil, Diagnosed_RSV, Diagnosed_RSV_r100Kutil, Percent_Diag_RSV, Percent_Diag_RSV_r100kutil, ILI_neg_COVID, ILI_neg_COVID_r100Kutil, Percent_Diag_ILINegCOVID, Percent_Diag_ILINegCOVID_r100kutil, Total_ED_Visits, countypop)

#write RSV/Flu Case Data
write_csv(Combined_CountyCases_output_FluRSV, "dashboards/InternalDashboard/data/CountyCases_FluRSV.csv")



##RSV/Flu Hospitalizations
CountyLvl_CaseData_Flu_RSV_Hosp <- read.csv("output/working data files/CaseLineListData_FluRSV_Hospitalizations.csv") %>%
  mutate(week_start = as.Date(week_start)) %>%
  mutate(Date = ymd(week_start)) %>%
  rename(County = county,
         Hospitalized_CaseCount = weekly_case_count) %>%
  left_join(countypopdata, by=c("County", "Year")) %>%
  select(-c(Year, week_start)) %>%
  filter(!is.na(County)) %>%
  mutate(Hospitalized_CaseCount_r100Kutil = (Hospitalized_CaseCount/countypop)*100000) %>%
  arrange(pathogen, County, Date) %>%
  select(pathogen, County, countypop, Date, Hospitalized_CaseCount, Hospitalized_CaseCount_r100Kutil)

write_csv(CountyLvl_CaseData_Flu_RSV_Hosp, "dashboards/InternalDashboard/data/CountyCases_FluRSV_Hospitalization.csv")






#########################################
###            EXPORT DATA            ###
#########################################
#Step 5: export the data-------

#remove early sample date rows for all pathogens other than sars
Output<-Output[!(Output$pcr_target !="sars-cov-2" & Output$Date < "2023-05-01"),]

############################
#write files for trends script
SystemDataPRFlag_PDB <- IDB_masterlist %>%
  select(wwtp_name, public_repository, pcr_target)

##remove extraction method for trends script
OutputforTrendsScript <- Output %>%
  left_join(SystemDataPRFlag_PDB, by=c("Utility" = "wwtp_name", "pcr_target"), relationship = "many-to-many") %>%
  filter(pcr_target == "sars-cov-2") %>%
  group_by(Utility) %>%
  arrange(Utility, Date) %>%
  select(-c(extraction_method, test_result_date, `Log_Viral Copies/L (uncorrected)`, public_repository)) %>%
  rename("SARS-2 Copies/L (uncorrected)" = `Viral Copies/L (uncorrected)`)

# #Write it to Data for Devon folder on K drive
OutputName = paste0("output/working data files/BSTS_Prep_Sars-CoV-2.csv")
write_csv(OutputforTrendsScript,OutputName)



############################
#write files for internal dashboard application

#internal dashboard V2 output
Output_WW_Dashboard_Data <- Output %>%
  group_by(Utility) %>%
  arrange(Utility, Date) %>%
  select(-c(extraction_method, test_result_date))


#Write it to the application directory
OutputName = paste0("output/working data files/WW_Dashboard_Data.csv")
write_csv(Output_WW_Dashboard_Data,OutputName,na = "")
