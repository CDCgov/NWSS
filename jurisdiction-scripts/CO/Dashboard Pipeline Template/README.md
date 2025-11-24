**Wastewater Surveillance Internal Dashboard**  
**NWSS Template SOP**

Designed for wastewater surveillance program jurisdictions, this template provides a flexible framework for building a data pipeline that cleans, transforms, and outputs data to a password-protected RShiny dashboard, as well as a formatted data file for submission to the National Wastewater Surveillance System (NWSS). 
All included data is mock data, supplied to help users test and validate the pipeline before integrating their own datasets. The document is intended to support teams in adapting the template to meet the needs of their specific wastewater surveillance programs.

This pipeline template was developed using R version 4.3.1 and RStudio version 2023.09.1 (Build 494). To minimize potential package compatibility issues, we recommend installing these or similar versions. Older versions may cause issues, particularly with the bsts library. Additionally, the formatted file intended to support submission to NWSS was designed alongside the NWSS Data Dictionary_v6.2.0_2025-09-18, which can be found in the reference_sheets folder within the Dashboard Pipeline Template folder on the repo.

This template, including the mock datasets, was created and is maintained by the Colorado Wastewater Surveillance team. For questions or support, please contact: **cdphe_wastewater_surv@state.co.us**.

**Internal Dashboard Login Information**  
**Username:** NWSSTemplate@NWSSdemo.com   
**Password:** NWSSTemplate  
*\*\*both of these are case sensitive\*\**

**Table of Contents:**

* [Notes](#bookmark=id.lg3mf2dhs2oi)  
* [Download Files from NWSS Github](#bookmark=id.4gn10swa6lx)  
* [Brief Walkthrough of Included Files](#bookmark=id.6vvelmumtmfd)  
* [Set Up on Local System](#bookmark=id.w9jgc9k4nk14)  
* [Set up Google Drive (optional)](#bookmark=id.phs3bvpgbcsp)  
* [Run Master Script](#bookmark=id.weyxo2r5ticz)  
* [Customize App Script](#bookmark=id.96wnwciejlfr)  
* [IDB Master Control Schematic](#bookmark=id.z2tg1itxk2iz)  
* [Troubleshooting](#bookmark=id.h82b41j32gv8)  
* [BSTS Github Link](#bookmark=id.aqx5dljlgqhv)  
* [Additional Resources](#bookmark=id.2vzeaafmiq3)

**Notes**

* Recommend version R 4.3.X as some of the older versions have issues with bsts library


**Download Files from NWSS Github**

1. On your local drive, create a folder called “GitHub”.  
   1. Example path:  
      C:\\Users\\\[username\]\\Documents\\GitHub\\CO-WW-IDB-Template  
2. We recommend using GitHub Desktop to clone the repo to your local drive  
   1. If you haven’t already, create an account at [github.com](http://github.com)  
      1. Go to settings → Passwords and authentication → set up 2FA with whatever mode you prefer  
   2. Download and install [GitHub Desktop](https://desktop.github.com/download/)   
   3. Go to the [CO-WW-IDB-Template](https://github.com/CDCgov/NWSS/tree/master) repo on the NWSS Github page   
   4. Click on the green “code” button and copy the https url  
   5. In your GitHub Desktop software, clone the repo to your “GitHub” folder by clicking “add” → “clone repository” → url tab → paste the url for the repo.  
   6. Follow this brief [GitHub Desktop Training](https://docs.google.com/document/d/1awJvnA0ll1WLwNocgWsLyUq8Ab8dnk2YToNcFZObPEw/edit?tab=t.0) tutorial for best git practices and to incorporate version control into your workflow/team.

**Brief Walkthrough of Included Files**

1. When you open your saved repo folder, files will be organized into the following folders:  
   1. ***dashboards***: this folder contains all files required to launch the internal dashboard using RShiny, all permissions files required to run RShiny, and all files ready to be used on a public dashboard – many of these public dashboard files will be made each time you run the “***Wastewater Master Control Template”***   
   2. ***import***: this folder includes mock data created for this template, including viral concentration and sequencing data files, clinical data files, and shapefiles of sewershed boundaries  
   3. ***output***: this is where most files will output to after running the master control script, ***Wastewater Master Control Template.*** (some files will output to the dashboards folder)  
   4. ***r\_scripts***: all r scripts used in the master control script can be found here  
   5. ***reference\_sheets***: contains metadata required to clean and analyze lab results and clinical data throughout the pipeline  
   6. ***r\_packages***: this folder contains older versions of some required packages if you are having trouble with specific packages  
2. The R file entitled “***Wastewater Master Control Template***” is the master file you will use to run the dashboard pipeline.

**Set Up on Local System**

1. Install all of the packages listed in the “libraries and functions” r script found under the r\_scripts folder  
2. Install all of the packages listed at the top of the “app” r script found under the dashboards/InternalDashboard folder  
3. If some scripts won’t install, try the steps in the [troubleshooting section](#bookmark=id.h82b41j32gv8) of this guide

**Set Up Google Drive (optional)**

1. If you would like to keep reference sheets and/or lab results on google drive for easy sharing between teams (for example lab and data analytics teams), you have that option.   
2. To do this, you will need to share those files with whatever email you type into line 22 in the Wastewater Master Control Template script.  
3. For the purposes of this template, we’ve created a [NWSS Template Mock Data](https://drive.google.com/drive/u/0/folders/1hHBroouF9TwmATWDLkVUhkxyK_-hGo-K) google folder with mock data:  
- [System Data](https://docs.google.com/spreadsheets/d/1ERZ2EJK1Qu5_0GROswshgjXeiMeMb3Gclv5P4SftPSI/edit?gid=0#gid=0)  
- [System Activation Status](https://docs.google.com/spreadsheets/d/11JFz5R2zqUhVcNn6bdHwqUqH9vTzwEBgMx6CmOOFKQ0/edit?gid=0#gid=0)  
- [Mock DataLakehouse Clinical Data](https://docs.google.com/spreadsheets/d/1WE33232ur1ICjtCw5LAlifa8feTw1KzjC_eSgejJTME/edit?gid=0#gid=0)  
- [Mock Flu/RSV Sentinel Surveillance Cases](https://docs.google.com/spreadsheets/d/1-ko4b3fuA0PiRLI2wWrfPtXjVtN2nhc2IJ70PA51SWg/edit?gid=0#gid=0)  
- [Mock Flu/RSV Hospitalizations](https://docs.google.com/spreadsheets/d/1ZbEZWmzk1EQIMDBp5T6zKNtv4_DM0G17tdoWmI3AMdg/edit?gid=0#gid=0)  
- [Mock Sars-CoV-2 Sequencing Results](https://drive.google.com/drive/u/0/folders/1Hv_3j0EgtLlqBIPWQvsEjZqNFs8YEHLD)  
4. Alternatively, you may also keep your files on your local drive or a shared drive. We’ve included copies of the above files in the repo. See annotations in the “Read References Sheets.R” and “Import Case Data.R” scripts for the location of these files within the repo folder.

**Run Master Script**

1. The ***Wastewater Master Control Template*** script is designed to be the master script that runs each individual script without needing to open each every time you run the pipeline.  
2. This script is divided into the following sections:

	  
**LOCAL ENVIRONMENT SETUP**

1. On line 9, replace \[ENTER LOCAL DRIVE NAME HERE\] with the name of your local user folder.   
   1. For example, use whatever name is in your pathfile here for \[username\]: C:\\Users\\\[username\]\\Documents\\GitHub\\CO-WW-IDB-Template  
2. On line 22, replace \[YOUR EMAIL HERE\] with the email you would like to use to access any files on google drive.  
3. Run the section “Local Environment Setup” to set up your R environment.  
   2. For package issues, see the [Troubleshooting](#bookmark=id.h82b41j32gv8) section below.  
      

   **REFERENCE SHEETS**  
1. This section reads in reference sheets from either google drive or within the repo folder, depending on which option you choose.   
2. This template’s default is to use google drive; however you may remove annotations from the line below to read in locally  
   1. Open Read Reference Sheets.R script in the r\_scripts folder.  
   2. Ex. if you would like to read the SystemData file in from the repo drive:  
      1. Annotate out line 19 and remove the annotation from line 21\.  
      2. Annotate out line 52 and remove the annotation from line 54\. 

   **INTERNAL DASHBOARD UPDATE**

1. This section reads in all lab and clinical results and aggregates it together.  
2. ***WW Data Compiler:*** this script reads in all lab results and compiles them together for NWSS submission and preps files for further analysis in the pipeline.  
   1. Reads in historic lab results that have already been compiled and cleaned.  
   2. Reads in individual lab results files (any results in the import/viral\_conc\_results folder)  
   3. Combines historic and new lab results together, cleans various columns  
   4. Outputs QC reports that will get pulled into the validation report at the end of the pipeline  
   5. Identifies lab phases based on lab methodology.  
   6. Prints files:  
      1. 1\_CDPHE\_Wastewater\_Data\_\[date\].csv for the lab  
      2. 2\_CDPHE\_Wastewater\_Data\_\[date\].csv for submission to NWSS  
      3. 3\_CDPHE\_Wastewater\_Data\_\[date\].csv for further analysis later in the pipeline.  
3. ***HX Detection Status:*** this script compiles all influenza subtyping data and preps it for the Internal Dashboard and Public Dashboards  
   1. Creates lists for sentinel sites  
   2. Reads in subtyping data created in WW Compiler Script from working data files folder  
   3. Cleans data and classifies detection status  
   4. Outputs flu subtyping file to dashboards folder for both internal and public facing dashboards  
4. ***Import\_Case\_Data:*** reads in all clinical data  
   1. As with the reference sheets script, you have the option of storing clinical data on google drive (for easy access with other teams) or within the repo or shared drive. Both options are available in the code base.  
      1. To read in clinical data within the repo:  
         1. Comment out line 23, added line 24  
         2. Comment out line 63, added line 64  
   2. You will not be able to use lines 26-34, 66-74, 94-102This script begins by reading in historic cases for Sars-CoV-2 from before we switched to pulling from google drive / Data Lakehouse.  
   3. For recent Sars-CoV-2 clinical data, we now store that information in Data Lakehouse. Data Lakehouse automatically prints all Sars-CoV-2 cases every day to google drive so this script reads in the most recent data from google drive (again you have the option also to store these files in the import folder on the repo; see import\_case\_data script for file locations)  
   4. Reads in Flu and RSV sentinel/syndromic data  
   5. Reads in Flu/RSV hospitalized data  
   6. All clinical data is written to the working data files folder for further analysis later in the pipeline.  
5. ***Sequencing\_Sars-CoV-2:*** this script reads in all sequencing data for Sars-CoV-2  
   1. Once again you have the option of reading from google drive or from the repo folder. For the purposes of this template, the default is to use google drive; see script for directions to switch to repo.   
6. ***Prepare\_Case\_Data\_Compiler:*** this script reads in all case data from the working data files folder and calculates various columns based on population county level population. It then exports files for the BSTS trend analysis and another called WW\_Dashboard\_Data, which will be further analyzed down the pipeline.

**TRENDS**

1. ***bsts\_IDrends:*** this script classifies trends using a bayesian structured time series model for Sars-CoV-2  
2. ***Trend\_percentiles\_plot:*** this script preps the file needed for the trend percentile plot on the Internal Dashboard  
3. ***CDC\_definitions\_status:*** this script classifies all pathogens based on CDC definitions (see below). It outputs QC files for the validation report and outputs a file to use further in the pipeline.  
   1. *Persistent detection*: The virus that causes \[pathogen\] was detected in more than 80% of samples in the past 4 weeks AND the most recent detection was within the past 2 weeks.  
   2. *Detection*: The virus that causes \[pathogen\]was detected in 1% to 80% of samples in the past 4 weeks AND the most recent detection was within the past 2 weeks.  
   3. *No recent data*: Fewer than 3 samples were submitted in the past 4 weeks.  
   4. *No recent detection*: The virus that causes \[pathogen\] was not detected in any samples in the past 2 weeks.  
      

**FINISH IDB UPDATE**

1. ***Combine\_DataTrends\_IDBOutput:*** this script is the final script for the internal dashboard pipeline that combines all data files together, saving the output to the dashboards/InternalDashboard/data folder.   
   1. It also prints QC reports for both the internal and public facing dashboards that checks for duplicate data being posted.  
2. ***Prep\_PFD\_Files:*** this script preps all final data files for the public dashboards, filtering out any utilities that are to be kept internal according to the MOU.  
3. ***Permissions\_NWSSTemplate:*** this script has been commented out as it needs to be adjusted to connect to your state’s RedCap account before it can be used. Once you adjust this script, the template will no longer work with the provided username/password combination. For purposes of this template, keep it annotated out.  
   1. When you are ready to adjust to connect to your RedCap, replace \[ENTER TOKEN HERE\] on line 7 with your RedCap API token.

**QC REPORTS**

1. This section produces an updated ***validation report*** each time the pipeline runs. This R markdown file and the script are both saved in the validation\_reports folder on the repo.   
   1. Whatever folder the script is in, that is the folder the validation report prints out to, which is why the WastewaterValidationReport.Rmd script isn’t saved under the r\_scripts folder.  
2. You will need to set your working directory in this file before running the first time. Open the WastewaterValidationReport.RMD file found under the output/validation folder and adjust \[ENTER LOCAL DRIVE NAME HERE\] on line 50\. You may need to set your Knit directory according to current working directory. To do this, go the “Knit” button on the Rstudio toolbar \> “Knit Directory” \> “Current Working Directory”   
3. Update line 997 with file writing location. 

**DASHBOARD DEPLOYMENT**

1. If this is your first time deploying, see Customize App Script below.  
2. Otherwise, run this script to deploy the updates to your own RShiny dashboard\!  
3. Login information is located at the top of this document.  
4. Once loaded, you may need to adjust the date range to view the mock clinical data results. The latest clinical result date is in July 2025 for the mock data.

   

**Customize App Script**

1. Open the app script saved under dashboards/InternalDashboard to edit the verbiage and plots to fit your jurisdictions requirements.  
2. This script is divided into three main sections:  
   1. ***Set Up Environment & Read in Data***: this section loads all files saved to the dashboard/InternalDashboard folder. All files must be saved to this folder in order to deploy the app correctly.  
   2. ***UI***: this section creates the user interface layout of your dashboard. This is where you control how large the plots are, where wording occurs, etc.  
   3. ***Server***: this section provides the data that will be shown in the ui layout. This is where you plot your data that will then show in the section of the dashboard specified in the UI section  
3. You can test your dashboard before deploying by clicking on the “Run App” button in your R text editor. Be sure to set your working directory (line 19\) before running.  
4. When running the app script, we recommend hitting the red “Stop” button in your console instead of exiting out of the pop-up and exiting quickly can cause R to crash.  
5. Open the Deploy App\_Template.R file in r\_scripts and enter your shinyapps.io account info. Update line 1 with your account name, line 2 with your token, line 3 with your secret, and line 7 with your account name.   
   1. If you are running R Shiny for the very first time, you will need to sign up for a (free) account at [https://www.shinyapps.io/](https://www.shinyapps.io/). Go directly to the sign up page [here](https://login.shinyapps.io/register?redirect=%2Foauth%2Fauthorize%3Fclient_id%3Drstudio-shinyapps%26redirect_uri%3Dhttps%253A%252F%252Fwww.shinyapps.io%252Fauth%252Foauth%252Ftoken%26response_type%3Dcode%26scopes%3D%252A%26show_auth%3D0&product=shinyapps). This will give you the three things you need to run a Shiny app: account name, token, and secret. This information allows you to deploy the app, but use the user information at the top of this document to login to the dashboard, once you deploy it. 

**NWSS Template Master Control Schematic**  
For a visual breakdown of the imports and outputs of each scripts within the Master Control script, please see [NWSS Template Master Control Schematic](https://docs.google.com/presentation/d/1_vT4DrK3V-_o1u85mkR1jVVtmMlsG19TmCKKQhCuosU/edit?slide=id.p#slide=id.p)

**Troubleshooting**

1. If **library(spdplyr)** will not install, in R go to tools \-\> install packages \-\> select “package archive file” and import in the spdplyr\_0.4.0.tar.gz file found on the repo under “r\_packages” folder  
2. If **library(rgdal)** will not install, in R go to tools \-\> install packages \-\> select “package archive file” and import in the rgdal\_1.6-6.tar.gz file found on the repo under “r\_packages” folder.  
   1. Note about rgdal package. In order to be able to download and run the package: Need R 4.3.2 or 4.3.1 and Rtools 43\.   
      1. Need to have the following packages downloaded before rgdal 1.6-6 will work (the specific versions matters\!): spbabel 0.6-0, spdplyr 0.4-0, sp 2.1-4, terra 1.7-78, raster 3.6-26, rsconnect 0.8-29

**BSTS Github Link**  
Please see [NWSS Github co\_ww\_analysis](https://github.com/CSPH-COVID/co_ww_analysis) page for more information on the bayesian structured time series model.

**Additional Resources**  
If you are new to RShiny, there is a great resource to build your own simple dashboard that helps understand how the UI and Server sections are set up. We highly recommend looking at [https://gesurvey.shinyapps.io/Graduate-Employment-Survey/](https://gesurvey.shinyapps.io/Graduate-Employment-Survey/)  
