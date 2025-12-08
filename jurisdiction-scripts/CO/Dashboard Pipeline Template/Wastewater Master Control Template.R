#######################################
###     LOCAL ENVIRONMENT SETUP     ###
#######################################
   
#Clear environment by removing all variables and functions
remove(list=ls())

#set this to your computer name
user_folder <- "[ENTER LOCAL DRIVE NAME HERE]" #can easily change here for other team members

#assign working directories
setwd(paste0("C:/Users/", user_folder, "/Documents/GitHub/WW-Data-Analytics-Repo/NWSS Dashboard Template"))
getwd()

#assign path to local output folder
localpath_output <- paste0("C:/Users/", user_folder, "/Documents/GitHub/WW-Data-Analytics-Repo/NWSS Dashboard Template/r_scripts")

#Load libraries and additional functions
source("r_scripts/libraries and functions.r")

#authorize google drive account for LIMS and DLH
DLH_Email = "[YOUR EMAIL HERE]"

drive_auth(email = DLH_Email, cache = TRUE)
bq_auth(email = DLH_Email)
gs4_auth(cache = ".secrets", email = DLH_Email)

#######################################
###        REFERENCE SHEETS         ###
#######################################
 
##read in reference sheets from google and save updated copies to K drive
source("r_scripts/Read Reference Sheets.R")


#######################################
###    INTERNAL DASHBOARD UPDATE    ###
#######################################

#Compiler that pulls add data together from CSU, CDPHE, and DU folders on K drive
source("r_scripts/WW Data Compiler.R")

#flu a subtyping
source("r_scripts/Hx Detection Status.r")

#Get relevant CDPHE case data from server
source("r_scripts/Import_Case_Data.R")

#Import and write compiled sars sequencing data
source("r_scripts/sequencing_sars-cov-2.R")

#Mash all the data together and send it to the K drive
source("r_scripts/Prepare_Case_Data_Compiler.r")

        
#######################################
###           TRENDS                ###
#######################################

source("r_scripts/bsts_IDtrends.R")
source("r_scripts/trend_percentiles_plot.R")
source("r_scripts/CDC_detection_status.R")

#######################################
###         FINISH IDB UPDATE       ###
#######################################

#combine ww data with trends, create output for IDB
source("r_scripts/Combine_DataTrends_IDBOutput.r")

#prep files for PFD
source("r_scripts/Prep_PFD_Files.R")

#Deploy internal dashboard - This updates permissions for the app by interfacing with REDCap
## YOU WILL NEED TO ENTER YOUR API TOKEN BEFORE RUNNING
## for testing, you can use the saved user_base.RDS file in /dashboards/InternalDashboard folder
# source("r_scripts/Permissions_NWSSTemplate.R")


#######################################
###         QC Reports              ###
#######################################

#run markdown validation report before deploying
rmarkdown::render("output/validation_reports/WastewaterValidationReport.Rmd")



###########################################
###         DASHBOARD DEPLOYMENT        ###
###########################################
#----------------------------------------------
#check reports and if all looks good, deploy app!
#----------------------------------------------

#Deploy app
## You will need to adjust Deploy App script with your rshiny token info before deploying
source('r_scripts/Deploy App_Template.R')








