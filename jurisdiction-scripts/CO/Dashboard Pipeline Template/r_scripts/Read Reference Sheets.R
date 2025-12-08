#####################################
###     County FIPS Conversions   ###
#####################################
## read in CountyFIPS codes from repo
CountyFIPS <- read_excel("reference_sheets/County Fips Conversions.xlsx") %>%
  filter(`State Code (FIPS)` %in% "08") %>%
  dplyr::mutate(COUNTY = `Area Name (including legal/statistical area description)` %>%
                  toupper()) %>%
  dplyr::mutate(FIPS = paste0(`State Code (FIPS)`,`County Code (FIPS)`)) %>%
  separate(COUNTY,into = "COUNTY",sep = " COUNTY",remove = TRUE,extra = "drop") %>%
  dplyr::select(COUNTY, FIPS)


#########################
###     System Data   ###
#########################

## read in System Data sheet from google drive 
SystemData <- read_sheet("https://docs.google.com/spreadsheets/d/1ERZ2EJK1Qu5_0GROswshgjXeiMeMb3Gclv5P4SftPSI/edit?gid=0#gid=0")
#you can read in from repo instead of google drive if you wish. Annotate out the above line and run the following code:
# SystemData <- read.csv("reference_sheets/System Data.csv")

## write copy to reference folder so this is updated with any recent changes from google
write.csv(SystemData, "reference_sheets/System Data.csv")

## write copy to dashboard folders
write.csv(SystemData, "dashboards/InternalDashboard/data/System Data.csv")
write.csv(SystemData, "dashboards/PublicDashboard/System Data.csv")

print("System Data sheet has been updated throughout system")

## prep county_names, add CountyFIPS
SystemData <- SystemData %>%
  separate(county_names,into = c("county1","county2"), sep = ",",remove=FALSE) %>%
  dplyr::mutate(county1 = toupper(county1),
                county2=toupper(county2)) %>%
  left_join(CountyFIPS,by=c("county1"= "COUNTY")) %>%
  dplyr::rename(FIPS1 = FIPS) %>%
  left_join(CountyFIPS,by=c("county2"= "COUNTY")) %>%
  dplyr::rename(FIPS2 = FIPS) %>%
  unite(., col = "FIPSc",  FIPS1, FIPS2, na.rm=TRUE, sep = ",") %>%
  dplyr::select(-county1,-county2)  %>%
  arrange(wwtp_name) 



#########################################
###     WW System Activation Status   ###
#########################################

## read in activation sheet from google drive (on google drive for easy team updates)
currentsystems_reference <- read_sheet("https://docs.google.com/spreadsheets/d/11JFz5R2zqUhVcNn6bdHwqUqH9vTzwEBgMx6CmOOFKQ0/edit?gid=0#gid=0")
#you can read in from repo instead of google drive if you wish. Annotate out the above line and run the following code:
# currentsystems_reference <- read.csv("reference_sheets/WW System Activation Status.csv")

##read in activation sheet
currentsystems_reference <- currentsystems_reference %>%
  select(wwtp_name, pcr_target, activation_start, activation_end, public_repository) %>%
  mutate(Surv_System = "SSS") %>%
  mutate(activation_end =  as.Date(activation_end, format = '%m/%d/%Y')) %>%
  mutate(activation_start =  as.Date(activation_start, format = '%m/%d/%Y')) %>%
  mutate(participation_status = case_when(Sys.Date() >= activation_start & (Sys.Date() <= activation_end | is.na(activation_end)) ~ "Active", 
                                          TRUE ~ "Inactive")) %>%
  arrange(wwtp_name, activation_start)


#######################################################
###     Create Utility Master Lists per Dashboard   ###
#######################################################
## Internal Dashboard
IDB_masterlist <- currentsystems_reference %>% 
  arrange(wwtp_name)

write.csv(IDB_masterlist, "dashboards/InternalDashboard/data/IDB_masterlist.csv")

print("IDB Masterlist has been updated for Internal Dashboard")


## NWSS Submission
NWSS_masterlist_sitenameid <- SystemData %>%
  select(wwtp_name, sitename_id)

NWSS_masterlist <- currentsystems_reference %>% 
  filter(public_repository == "Publish") %>% #publish only but will include active and inactive
  filter(!pcr_target %in% c("RSV_A", "RSV_B")) %>% #we are only sending combined PanRSV
  arrange(wwtp_name) %>%
  left_join(NWSS_masterlist_sitenameid, by = c("wwtp_name"))

rm(NWSS_masterlist_sitenameid)


## Public Dashboard
PFD_masterlist <- currentsystems_reference %>% 
  filter(Surv_System == "SSS") %>%
  filter(public_repository == "Publish")

write.csv(PFD_masterlist, "dashboards/PublicDashboard/PDB_masterlist.csv")

print("PDB Masterlist has been updated for Public Dashboard")

OpenPortal_masterlist <- currentsystems_reference %>% 
  filter(public_repository == "Publish") %>% #publish only but will include active and inactive
  arrange(wwtp_name)

## Flu Subtyping
PFD_masterlist_FluA <- PFD_masterlist %>% 
  filter(pcr_target == "FLUAV") 

IDB_masterlist_FluA <- IDB_masterlist %>% 
  filter(pcr_target == "FLUAV") 

IDB_masterlist_FluA_Hx <- IDB_masterlist %>% 
  filter(pcr_target %in% c("H5", "H3", "H1"))

#sars sequencing
PFD_masterlist_sars <- PFD_masterlist %>% 
  filter(pcr_target == "sars-cov-2") 

################################################
###            WWTP Counties Sheet           ###
################################################

## update county reference sheet
WWTP_Counties_reference <- SystemData %>%
  select(c(wwtp_name, primary_county, overlapping_counties)) %>%
  rename(utility = wwtp_name)

write.csv(WWTP_Counties_reference, "reference_sheets/WWTP_Counties_Reference.csv")
write.csv(WWTP_Counties_reference, "dashboards/InternalDashboard/data/WWTP_Counties_Reference.csv")
print("WWTP Counties Sheet has been updated throughout System")



###################################################################
##                County Level Population Data                   ##
###################################################################

# pull in county population data 
print("Importing county population information")

countypopdata <- read.csv("reference_sheets/County_Lvl_Population.csv") %>%
  rename(County = County.Name)


#add current year population data in
countypopdata <- countypopdata %>%
  arrange(County, Year) %>%
  rename(countypop = Population) %>%
  mutate(Year = as.numeric(Year))


