# WW Internal Dashboard 
# Authors:
#     Rachel Shaw      (rachel.shaw@state.co.us)
#     Kirsten Weisbeck (kirsten.weisbeck@state.co.us)
#     
# 
# 
# -------------- TRY DYGRAPHS (app_denver_2 plotly replacement)
# -------------- ADD NARRATIVE - CREATE GOOGLE DOC (KW to do)
# -------------- FORMATTING
# -------------- FIX HOSPS CALC (hosp_dygraph, casehospdataest (skip geocoding section takes 7 hours), use cedrsdata_missing_dta csv )
# -------------- LAST: PERMISSIONS
# 

##########################################################################
####                    Set Up Environment & Read in Data             ####
##########################################################################

# setwd("C:/Users/[username]/Documents/GitHub/WW-Data-Analytics-Repo/NWSS Dashboard Template/dashboards/InternalDashboard")

 
{
  # Load packages
  library(pacman)
  p_load(
    tidyverse,
    stringr,
    readxl,
    readr,
    ggthemes,
    plotly,
    lubridate,
    sf,
    skimr,
    leaflet,
    shiny,
    shinyjs,
    ggplot2,
    shinydashboard,
    dplyr,
    conflicted,
    htmltools,
    magrittr,
    scales, 
    dygraphs,
    kableExtra
  )
  library(shinyauthr)
  library(viridis)
  
  
  conflict_prefer("box", "shinydashboard")
  conflict_prefer("filter", "dplyr")
  conflict_prefer("layout", "plotly")

  #Define Favorite Utility Function
  "%!in%" <- function(x, y)
    ! ('%in%'(x, y))
  
  ####################################
  ##          READ IN DATA          ##
  ####################################

  #Read in credentials
  user_base <- readRDS("user_base.rds")
  

  # System data
  System_Data <-
    read.csv(
      normalizePath(
        "data/System Data.csv"
      )
    )  %>%
    mutate(utility = wwtp_name) %>%
    select(-wwtp_name)
  
  #metadata classifications
  metadata_survey <- System_Data %>%
    select(utility, surveyed_classification)

  #data dictionary
  data_dictionary <- read.csv("data/IDB Data Dictionary.csv")
  
  IDB_masterlist <- read.csv("data/IDB_masterlist.csv") %>%
    mutate(utility = wwtp_name)
  
  SSS_masterlist <- IDB_masterlist %>%
    filter(Surv_System == "SSS")
  
  
  #viral concentration / metadata
  #import metadata (need AddtoCompiler script to output this file)
  wwdatafull <- read.csv("data/wwdatafull.csv") %>%
    mutate(measure_date = lubridate::as_date(measure_date, format = "%Y-%m-%d")) %>%
    group_by(utility, pcr_target) 
  
  
  #import flua subtyping
  flua_subtyping_data <- read.csv("data/flua_hx_data.csv") %>%
    rename(utility = wwtp_name) %>%
    rename(measure_date = sample_collect_date) %>%
    mutate(measure_date = as.Date(measure_date, format = "%Y-%m-%d")) %>%
    mutate(flu_pcr_target = case_when(flu_pcr_target == "Inf_A-MP" ~ "FLUAV",
                                      TRUE ~ flu_pcr_target))
  
  
  ##import trend percentile plot data
  plot_dat <- read.csv("data/trend_percentile_data.csv") %>%
    mutate(measure_date = as.Date(measure_date, format = "%Y-%m-%d")) %>%
    mutate(sig = case_when(sig == "Not Significant" ~ "Trend Slope Not Significant", TRUE ~ "Trend Slope Significant"))
  
  
  #import county level case data
  countycase_data <- read.csv("data/CountyCases_Sars-CoV-2.csv") %>%
    mutate(measure_date = as.Date(Date, format = "%Y-%m-%d")) %>%
    select(-Date) %>%
    mutate_all(~ ifelse(is.na(.), -1, .)) %>%
    mutate(measure_date = as.Date(measure_date, format = "%Y-%m-%d"))
  
  countycase_data_flursv <- read.csv("data/CountyCases_FluRSV.csv") %>%
    mutate(measure_date = as.Date(Date, format = "%Y-%m-%d")) %>%
    select(-Date) %>%
    mutate_all(~ ifelse(is.na(.), -1, .)) %>%
    mutate(measure_date = as.Date(measure_date, format = "%Y-%m-%d"))
  
  countycase_data_flursv_hosp <- read.csv("data/CountyCases_FluRSV_Hospitalization.csv") %>%
    mutate(measure_date = as.Date(Date, format = "%Y-%m-%d")) %>%
    select(-Date) %>%
    mutate_all(~ ifelse(is.na(.), -1, .)) %>%
    mutate(measure_date = as.Date(measure_date, format = "%Y-%m-%d"))
  
  #SARS COUNTY CASE DATA
  #sars county case
  sars_casedata <- countycase_data %>%
    filter(pcr_target == "sars-cov-2") %>%
    select(County, measure_date, County_cases_3dayavg_r100Kutil)
  
  #sars county case
  sars_hospdata <- countycase_data %>%
    filter(pcr_target == "sars-cov-2") %>%
    select(County, measure_date, County_hosp_3dayavg_r100Kutil)

  
  
  ##RSV County Cases
  rsv_casedata <- countycase_data_flursv %>%
    select(County, measure_date, Percent_Diag_RSV) %>%
    pivot_longer(!c(County, measure_date), names_to = "metric", values_to = "CountyCases") %>%
    mutate(metric = case_when(metric == "Percent_Diag_RSV" ~ "% RSV ED Visits", TRUE ~ metric))
  
  rsv_hosp <- countycase_data_flursv_hosp %>%
    filter(pathogen == "RSV")
  
  ##Flu County Cases
  flu_casedata <- countycase_data_flursv %>%
    select(County, measure_date, Percent_Diag_Flu) %>%
    pivot_longer(!c(County, measure_date), names_to = "metric", values_to = "CountyCases") %>%
    mutate(metric = case_when(metric == "Percent_Diag_Flu" ~ "% Influenza ED Visits", TRUE ~ metric))
  
  flu_hosp <- countycase_data_flursv_hosp %>%
    filter(pathogen == "Influenza")
  

  #grab regions from system data
  region_names_sars <- System_Data %>%
    select(c(utility, Region))
  
  #import utility county reference sheet
  countyperutility <- read.csv("data/WWTP_Counties_Reference.csv")
  
  #shapes for map
  wwshapes <- sf::st_read("data/shapefiles/SewershedBoundaries_Demo.shp",
                          stringsAsFactors = F) %>%
    st_transform() %>%
    st_zm() %>%
    as("Spatial")
  
  filtered_sites <- c("Utility1","Utility2","Utility3","Utility4","Utility5")
  
  
  countyshapes <-
    sf::st_read("data/shapefiles/Colorado_County_Boundaries.shp",
                stringsAsFactors = F) %>%
    st_transform() %>%
    st_zm() %>%
    as("Spatial")
  

  recentData <- wwdatafull %>%
    filter(!is.na(viral_conc_raw)) %>%
    arrange(utility, measure_date) %>%
    group_by(utility) %>%
    slice_tail(n=1) %>%
    ungroup() 
  
  #participation status
  part_status <- IDB_masterlist %>% 
    filter(public_repository == "Publish") %>%
    select(utility, participation_status) %>%
    arrange(utility, participation_status) %>%
    group_by(utility) %>%
    arrange(utility) %>%
    slice_head(n=1)
  
  wwshapes@data <- wwshapes@data %>%
    rename(utility = wwtp) %>%
    select(utility) %>%
    left_join(recentData, by = "utility") %>%
    left_join(part_status, by="utility") %>%
    separate(lpha1_contactname, into = c("lpha1_contactname1", "lpha1_contactname2", "lpha1_contactname3", "lpha1_contactname4", "lpha1_contactname5"), sep = ", ") %>%
    separate(lpha1_contactemail, into = c("lpha1_contactemail1", "lpha1_contactemail2", "lpha1_contactemail3", "lpha1_contactemail4", "lpha1_contactemail5"), sep = ", ") %>%
    mutate(lpha1_contactname1 = case_when(is.na(lpha1_contactname1) ~ "Not available", TRUE ~ lpha1_contactname1)) %>%
    mutate(lpha1_contactemail1 = case_when(is.na(lpha1_contactemail1) ~ "Not available", TRUE ~ lpha1_contactemail1)) %>%
    mutate(fill_color = case_when(utility %in% filtered_sites ~ "#DDCC77",
                                  TRUE ~ "grey"),
           legend_label = case_when(utility %in% filtered_sites ~ "Sentinel Sites",
                                    TRUE ~ "Emergency Surveillance/Historic Sites"))
  
  # view(wwshapes@data)
  
  wwshapes <- sf::st_as_sf(wwshapes)
  wwshapes <- st_transform(wwshapes, crs = 4326)
  

  
  firstDate <-   wwdatafull %>%
    filter(!is.na(viral_conc_raw)) %>%
    group_by(utility) %>%
    slice_min(order_by = measure_date, n = 1) %>%
    mutate(first_date = measure_date) %>%
    select(utility, first_date)
  
  # define other global variables
  utilnames <- wwdatafull %>%
    group_by(utility) %>%
    summarize()
  
  System_Data_activeonly <- System_Data %>%
    select(utility, population_served, county_names) %>%
    left_join(IDB_masterlist, by= c("utility")) %>%
    group_by(utility) %>%
    slice_head(n=1) %>%
    filter(Surv_System == "SSS") %>%
    filter(public_repository == "Publish") %>%
    filter(participation_status == "Active")
  
  totalUtils <- System_Data_activeonly %>%
    group_by(utility) %>%
    summarise() %>%
    nrow()
  
  totalPop <-
    round(sum(System_Data_activeonly$population_served, na.rm = TRUE)) %>%
    format(
      big.mark = ",",
      big.interval = 3L,
      digits = 1,
      scientific = FALSE
    )
  
  totalCounties <-
    data.frame(do.call("rbind", strsplit(
      as.character(System_Data_activeonly$county_names), ",", fixed = TRUE
    ))) %>%
    n_distinct()
  
  totalCounties <- countyperutility %>%
    filter(utility %in% System_Data_activeonly$utility) %>%
    separate_rows(overlapping_counties, sep = ", ") %>%
    mutate(combined_column = paste(primary_county, overlapping_counties, sep = "_")) %>%
    separate_rows(combined_column, sep = "_") %>%
    select(combined_column) %>%
    filter(!combined_column == "") %>%
    n_distinct()
  
  bins <- c(0, 1, 2.5, 5, 7.5, 10, Inf)
  
  pal <- colorBin("YlOrRd", domain = wwshapes$county_pos, bins = bins)
  
  lastupdateDate <- format(file.info("data/wwdatafull.csv")$mtime, "%Y-%m-%d")
  
  
  
  ##Utilities testing new pathogens
  Utilities_RSV <- IDB_masterlist %>% filter(pcr_target == "RSV_A" | pcr_target == "RSV_B" | pcr_target == "RSV A + B Combined" | pcr_target == "PanRSV") %>% select(utility) %>% distinct(utility) %>% unlist()
  Utilities_Flu <- IDB_masterlist %>% filter(pcr_target == "FLUAV" | pcr_target == "FLUAB") %>% select(utility) %>% distinct(utility) %>% unlist()
  
}

##################################################
##########            UI              ############
##################################################

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(title = " [ADD JURISDICTION NAME HERE] Wastewater Dashboard"),
  
  dashboardSidebar(
    
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Dashboard",
        tabName = "Dashboard",
        icon = icon("chart-line"),
        selected = TRUE
      ),
      menuItem(
        "Order Supplies",
        icon = icon("box", lib = "font-awesome"),
        href = "https://docs.google.com/forms/d/e/1FAIpQLSdaHHrrKBtO9iv6Mq-4dyHfaPypq5WKRdkZl3ymRFIjFrzoGg/viewform"
      ),
      menuItem(
        "CDPHE Wastewater Monitoring",
        icon = icon("house-laptop", lib = "font-awesome"),
        href = "https://cdphe.colorado.gov/covid-19/wastewater"
      ),
      menuItem(
        "Colorado Center of Excellence",
        icon = icon("star", lib = "font-awesome"),
        href = "https://www.du.edu/nwsscoe"
      ),
      menuItem(
        "Public Wastewater Dashboard",
        icon = icon("book-atlas", lib = "font-awesome"),
        href = "https://cdphe.maps.arcgis.com/apps/dashboards/d79cf93c3938470ca4bcc4823328946b"
      ),
      menuItem(
        "SARS-CoV-2 Detection Heatmap",
        icon = icon("chart-bar", lib = "font-awesome"),
        href = "https://docs.google.com/spreadsheets/d/1wV9LQm7Ev2g1zCGRdE7yJKiG9tdR9URlSxHDvhYtJbk/edit#gid=1754563600"
      ),
      menuItem(
        "NWSS COVID Data Tracker",
        icon = icon("tachograph-digital", lib = "font-awesome"),
        href = "https://covid.cdc.gov/covid-data-tracker/#wastewater-surveillance"
      ),
      menuItem(
        "Data Interpretation Guide",
        icon = icon("magnifying-glass-chart", lib = "font-awesome"),
        href = "https://docs.google.com/document/d/1WdxTRbTs_ZBsx3eXpdsBRz-FVNbBnDgr0Q79c5fciEs/edit?usp=sharing"#,
      ),
      menuItem(
        "Contact Us",
        icon = icon("envelope", lib = "font-awesome"),
        href = "https://forms.gle/RXXf1abfcmRBe2EX6"
      ),
      br(),
      column(12,alight="center",offset = 0,
             downloadButton("downloadData_all", label = "Download Full Dataset", class = "data-download-button"),
             tags$style(type="text/css", "#downloadData_all {background-color: gray20 ; color: black; border: black}")
      ),
      br(),
      br(),
      column(12,alight="center",offset = 0,
             downloadButton("downloadData_selected", label = "Download Selected Data", class = "data-download-button"),
             tags$style(type="text/css", "#downloadData_selected {background-color: gray20 ; color: black; border: black}")
      ),
      br(),
      br(),
      column(12,alight="center",offset = 0,
             downloadButton("downloadData_dictionary", label = "Download Data Dictionary", class = "data-download-button"),
             tags$style(type="text/css", "#downloadData_dictionary {background-color: gray20 ; color: black; border: black}")
      ),
      br(),
      br()
    )
    
  ),
  
  dashboardBody(
    
    #add password protection
    div(class="pull-right",shinyauthr::logoutUI(id="logout")),   #add logout button UI 
    shinyauthr::loginUI(id="login"),                             #add login panel UI function 
    
    
    tags$style(
      HTML(".box.box-solid.box-primary>.box-header {}
            .box.box-solid.box-primary{background:#222d32}
            .tabbable > .nav > li[class=active] > a {background-color: #3d9970;  color:white}"
      )),
    
    tabItems(
      tabItem(
        tabName = "Dashboard",
        h2(" [ADD JURISDICTION NAME HERE] Wastewater Surveillance Program | Internal Dashboard"),
        fluidRow(
          box(
            width = 12,
            color = "light-blue",
            uiOutput("welcome_text")
          )
        ),
        
        
        fluidRow(
          box(
            title = span(icon("faucet-drip"), "Active Wastewater Utility Partners"),
            background = "olive",
            width = 4,
            style = "font-size:24px",
            uiOutput("totalUtils_text")
          ),
          box(
            title = span(icon("users"), "Population Represented"),
            background = "olive",
            width = 4,
            style = "font-size:24px",
            uiOutput("totalPop_text")
          ),
          box(
            title = span(icon("location-dot"), "Participating Counties"),
            background = "olive",
            width = 4,
            style = "font-size:24px",
            uiOutput("totalCounties_text")
          )
        ),
        
        
        fluidRow(
          box(
            title = " [ADD JURISDICTION NAME HERE] Wastewater Utilities",
            width = 8,
            collapsible = TRUE,
            leafletOutput("wwmap", width = "100%", height = 500)
          ),
          
          box(title = "About the  [ADD JURISDICTION NAME HERE] Wastewater Surveillance Program", 
              width = 4,
              uiOutput("aboutwwprogram_text")
          )),
        
        
        fluidRow(
          box(
            box(
              br(),
              width = 4,
              solidHeader = FALSE,
              uiOutput('region_dropdown_sars'),
              uiOutput('classification_dropdown_sars'),
              br(),
              br(),
              br(),
            ), 
            box(
              width = 8,
              solidHeader = FALSE,
              tableOutput('region_quadtable_sars'),
            ), 
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            title = "All-Utilities Trend Snapshot",
            width = 8,
            collapsible = TRUE,
            status = "primary",
            plotlyOutput("trends_plot_allutilities"),
            br()
          ),
          
          box(title = "About the All-Utilities Trend Snapshot Plot", 
              width = 4,
              uiOutput("trends_text_allutilities")
          )),
        
     
        fluidRow(
          box(
            title = "Select a Utility:",
            width = 3,
            solidHeader = TRUE,
            background = "olive",
            uiOutput('reactiveUtilities')
          ),
          box(
            title = "Select Date Range:",
            width = 9,
            solidHeader = TRUE,
            background = "olive",
            sliderInput(
              "daterange1",
              "",
              min = ymd("2020-08-01"),
              max = ymd(Sys.Date()),
              value = c(ymd(Sys.Date() %m-% months(2)), ymd(Sys.Date()))
            )
          )
          
        ),
        
        tabsetPanel( #create tabs for all pathogens
          
          #####################         
          #    SARS-COV-2 UI
          #####################
          tabPanel("SARS-CoV-2", value = "SARS_Tab", #SARS-COV-2 PANEL
                   
                   ##########################
                   ##WW Data
                   fluidRow(column(
                     width = 12,
                     h3("Viral Concentration in Wastewater"),
                     h5(paste("Last update: ", lastupdateDate)),
                     br()
                   )),
                   
                   ##individual trend plot
                   fluidRow(
                     box(
                       title = "Utility Trend Summary Plot",
                       width = 8,
                       collapsible = TRUE,
                       status = "primary",
                       plotlyOutput("trends_plot_individualutility"),
                       
                     ),
                     
                     box(title = "About the Utility Trend Summary Plot", 
                         width = 4,
                         uiOutput("trends_text_individualutility")
                     )),
                   
                   #viral concentration
                   fluidRow(
                     box(id ="wwplot1_sars",
                         title = "Viral Concentration",
                         width = 8,
                         collapsible = TRUE,
                         status = "primary",
                         radioButtons("log_button_rawconc_sars", 
                                      label = "Select Metric",
                                      choices = list(
                                        "Linear Scale" = "Linear Scale", 
                                        "Log Scale" = "Log Scale"),
                                      selected = "Linear Scale", 
                                      inline = TRUE),
                         plotlyOutput("wwConcGraph_sars"),
                         
                     ),
                     box(id = "wwplot1_text_sars", 
                         title = "Viral Concentration Data Details", 
                         width = 4, 
                         collapsible = TRUE, 
                         status = "primary", 
                         uiOutput("viralconc_details_sars")
                     )
                   ),
                   
                   
                   #normalized concentration
                   fluidRow(
                     box(
                       title = "Normalized Viral Concentration",
                       width = 8,
                       collapsible = TRUE,
                       status = "primary",
                       radioButtons("log_button_normconc_sars", 
                                    label = "Select Metric",
                                    choices = list(
                                      "Linear Scale" = "Linear Scale", 
                                      "Log Scale" = "Log Scale"),
                                    selected = "Linear Scale", 
                                    inline = TRUE),
                       plotlyOutput("flowNormGraph_sars"),
                       
                     ),
                     box(id = "wwplot2_text_sars", 
                         title = "Normalized Viral Concentration Data Details", 
                         width = 4, 
                         collapsible = TRUE, 
                         status = "primary",
                         uiOutput("normviralconc_details_sars")
                     )
                   ),
                   
                   
                   #omicron detection heatmap
                   fluidRow(
                     box(
                       title = "SARS-CoV-2 Variant Detection Heatmap",
                       width = 8,
                       status = "primary",
                       collapsible = TRUE, 
                       collapsed=FALSE,
                       plotlyOutput("wwConcGraph_sars2"),
                       plotlyOutput("heatmap_sars")
                     ), 
                     box(id = "vocplot_text_sars", 
                         title = "SARS-CoV-2 Variant Heatmap Data Details", 
                         width = 4, 
                         collapsible = TRUE, 
                         status = "primary", 
                         uiOutput("omicron_text")
                     )
                   ),
                   
                   ###########################
                   #COVID Case Data
                   fluidRow(column(
                     width = 9,
                     h3("COVID-19 Disease Burden"),
                     br()
                   )),
                   
                   
                   #Cases per County
                   fluidRow(
                     box(
                       br(),
                       title = "County COVID-19 Case Rates",
                       width = 8,
                       collapsible = TRUE,
                       status = "warning",
                       plotlyOutput("county_cases_sars"),
                       br(),
                       br(),
                       br(),
                     ),
                     box(id = "countycases_text_sars",
                         title = "County Case Rate Data Details",
                         width = 4,
                         collapsible = TRUE,
                         status = "warning",
                         uiOutput("caserate_text_sars")
                     ),
                     
                     box(
                       htmlOutput("counties_in_utility_list_cases_sars"),
                       br(),
                       title = "Select a County:",
                       width = 4,
                       solidHeader = TRUE,
                       uiOutput('counties_in_utility_choices_cases_sars'),
                     ),
                     
                   ),
                   
                   
                
                   # #Hospitalization Data
                   fluidRow(
                     box(
                       br(),
                       title = "County COVID-19 Hospitalization Rate",
                       width = 8,
                       collapsible = TRUE,
                       status = "warning",
                       plotlyOutput("hospGraph_sars"),
                       br(),
                       br(),
                       br(),
                     ),
                     box(id = "countyhosp_text_sars",
                         title = "County COVID-19 Hospitalization Rate Data Details",
                         width = 4,
                         collapsible = TRUE,
                         status = "warning",
                         uiOutput("hosprate_text_sars")
                     ),
                     box(
                       htmlOutput("counties_in_utility_list_hosp_sars"),
                       br(),
                       title = "Select a County:",
                       width = 4,
                       solidHeader = TRUE,
                       uiOutput('counties_in_utility_choices_hosp_sars'),
                     ),
                   ),
                   
                   
          ), #end sars tab
          
          
       
          
          
          
          
          
          
          #####################         
          #    RSV UI
          #####################
          tabPanel("RSV", value = "RSV_Tab", ##RSV PANEL
                   
                   fluidRow(column(
                     width = 12,
                     h3("Viral Concentration in Wastewater"),
                     h5(paste("Last update: ", lastupdateDate)),
                     br()
                   )),
                   fluidRow(
                     box(id ="wwplot1_rsv", 
                         title = "Viral Concentration",
                         width = 8,
                         collapsible = TRUE,
                         status = "primary",
                         radioButtons("log_button_rawconc_rsv", 
                                      label = "Select Metric",
                                      choices = list(
                                        "Linear Scale" = "Linear Scale", 
                                        "Log Scale" = "Log Scale"),
                                      selected = "Linear Scale", 
                                      inline = TRUE),
                         plotlyOutput("wwConcGraph_rsv")
                     ),
                     box(id = "text_detectionstatus_rsv", 
                         title = "Detection Status Details", 
                         width = 4, 
                         collapsible = TRUE, 
                         collapsed=TRUE,
                         status = "primary", 
                         uiOutput('text_detectionstatus_rsv'),
                     ),
                     box(id = "wwplot1_text_rsv",
                         title = "Viral Concentration Data Details",
                         width = 4,
                         collapsible = TRUE,
                         status = "primary",
                         uiOutput('viralconc_details_rsv'),
                     )
                   ),
                   
                   
                   
                   #normalized concentration
                   fluidRow(
                     box(
                       title = "Normalized Viral Concentration",
                       width = 8,
                       collapsible = TRUE,
                       status = "primary",
                       radioButtons("log_button_normconc_rsv", 
                                    label = "Select Metric",
                                    choices = list(
                                      "Linear Scale" = "Linear Scale", 
                                      "Log Scale" = "Log Scale"),
                                    selected = "Linear Scale", 
                                    inline = TRUE),
                       plotlyOutput("flowNormGraph_rsv")
                     ),
                     box(id = "wwplot2_text_rsv", 
                         title = "Normalized Viral Concentration Data Details", 
                         width = 4, 
                         collapsible = TRUE, 
                         status = "primary", 
                         uiOutput('normviralconc_details_rsv'),
                     )
                   ),
                   
                   ###########################
                   #RSV Case Data
                   fluidRow(column(
                     width = 12,
                     h3("RSV Disease Burden"),
                     br()
                   )),
                   
                   
                   #Cases per County
                   fluidRow(
                     
                     box(
                       br(),
                       title = "County RSV Related Clinical Measures",
                       width = 8,
                       collapsible = TRUE,
                       status = "warning",
                       plotlyOutput("county_cases_rsv"),
                       br(),
                       br(),
                       br(),
                     ),
                     box(id = "countycases_text_rsv",
                         title = "County Clinical Measures Data Details",
                         width = 4,
                         collapsible = TRUE,
                         status = "warning",
                         uiOutput("caserate_text_rsv")
                     ),
                     
                     box(
                       htmlOutput("counties_in_utility_list_cases_rsv"),
                       br(),
                       title = "Select a County:",
                       width = 4,
                       solidHeader = TRUE,
                       uiOutput('counties_in_utility_choices_cases_rsv'),
                     ),
                     
                   ),
                   
        ), #End of RSV Panel
          
          
          
          
          
          
          
          
          #####################         
          #    Influenza UI
          #####################
          tabPanel("Influenza", value = "Flu_Tab", ##Flu PANEL
                   
                   fluidRow(column(
                     width = 12,
                     h3("Viral Concentration in Wastewater"),
                     h5(paste("Last update: ", lastupdateDate)),
                     br()
                   )),
                   fluidRow(
                     box(id ="wwplot1_flu",
                         title = "Viral Concentration",
                         width = 8,
                         collapsible = TRUE,
                         status = "primary",
                         radioButtons("log_button_rawconc_flu", 
                                      label = "Select Metric",
                                      choices = list(
                                        "Linear Scale" = "Linear Scale", 
                                        "Log Scale" = "Log Scale"),
                                      selected = "Linear Scale", 
                                      inline = TRUE),
                         plotlyOutput("wwConcGraph_flu")
                     ),
                     box(id = "text_detectionstatus_flu", 
                         title = "Detection Status Details", 
                         width = 4, 
                         collapsible = TRUE, 
                         collapsed=TRUE,
                         status = "primary", 
                         uiOutput('text_detectionstatus_flu'),
                     ),
                     box(id = "wwplot1_text_flu",
                         title = "Viral Concentration Data Details",
                         width = 4,
                         collapsible = TRUE,
                         status = "primary",
                         uiOutput('viralconc_details_flu'),
                     )
                   ),
                   
                   
                   
                   #normalized concentration
                   fluidRow(
                     box(
                       title = "Normalized Viral Concentration",
                       width = 8,
                       collapsible = TRUE,
                       status = "primary",
                       radioButtons("log_button_normconc_flu", 
                                    label = "Select Metric",
                                    choices = list(
                                      "Linear Scale" = "Linear Scale", 
                                      "Log Scale" = "Log Scale"),
                                    selected = "Linear Scale", 
                                    inline = TRUE),
                       plotlyOutput("flowNormGraph_flu")
                     ),
                     box(id = "wwplot2_text_flu", 
                         title = "Normalized Viral Concentration Data Details", 
                         width = 4, 
                         collapsible = TRUE, 
                         status = "primary", 
                         uiOutput('normviralconc_details_flu'),
                     )
                   ),
                   
                   
                   ##flu A subtyping
                   fluidRow(
                     box(
                       title = "Influenza A Viral Concentration & Subtyping",
                       width = 8,
                       collapsible = TRUE,
                       status = "primary",
                       plotlyOutput("flua_subtyping"),
                       br(),
                       plotlyOutput("flua_subtyping_conc")
                     ),
                     box(id = "wwplot2_text_flua_subtyping", 
                         title = "Influenza A Subtyping Data Details", 
                         width = 4, 
                         collapsible = TRUE, 
                         status = "primary", 
                         uiOutput('flua_subtyping_details'),
                     ),
                     box(id = "wwplot2_text_flua_subtyping_additional", 
                         title = "More Information about Influenza A Subtypes", 
                         width = 4, 
                         collapsible = TRUE, 
                         collapsed=TRUE,
                         status = "primary", 
                         uiOutput('flua_subtyping_additionaldetails'),
                     )
                   ),
                   
                   
                   ###########################
                   #Flu Case Data
                   fluidRow(column(
                     width = 12,
                     h3("Influenza Disease Burden"),
                     br()
                   )),
                   
                   
                   #Cases per County
                   fluidRow(
                     
                     box(
                       br(),
                       title = "County Influenza Related Clinical Measures",
                       width = 8,
                       collapsible = TRUE,
                       status = "warning",
                       plotlyOutput("county_cases_flu"),
                       br(),
                       br(),
                       br(),
                     ),
                     box(id = "countycases_text_flu",
                         title = "County Clinical Measures Data Details",
                         width = 4,
                         collapsible = TRUE,
                         status = "warning",
                         uiOutput("caserate_text_flu")
                     ),
                     
                     box(
                       htmlOutput("counties_in_utility_list_cases_flu"),
                       br(),
                       title = "Select a County:",
                       width = 4,
                       solidHeader = TRUE,
                       uiOutput('counties_in_utility_choices_cases_flu'),
                     ),
                     
                   ),
                   
          ), #End of Flu Panel
          
  
        
          
          
          
        id ="Pathogen_Tabs", #label main panel section for show/hide tab functionality
          

        ), #end of tab panels
        
        #Methodology 
        fluidRow(
          box(
            title = "Methodology",
            width = 12,
            color = "navy",
            collapsible = TRUE,
            uiOutput('methodology_text'),
          )
        ),
        
        fluidRow(
          box(
            title = "Glossary",
            width = 12,
            color = "navy",
            collapsible = TRUE,
            uiOutput('glossary_text'),
          )
        ),
        
        
      )# tabitems)# dashboardBody
    ),
    
  ) #end tab panel
  
  
)#dashboard page


##################################################
##########            SERVER          ############
##################################################

server <- function(input, output) {
  
  #call login module supplying dataframe, user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  #call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  output$reactiveUtilities = renderUI({
    req(credentials()$user_auth)
    credentials()$info
    
    choices <- credentials()$info %>%
      dplyr::select(allowed_utilities) %>%
      unlist() %>%
      as.character()
    
    
    selectInput("utility",
                "Select Utility:",
                choices = unique(choices))
    
    
  })
  
  
  
  #####################################
  #####   OVERALL REGION DROP DOWN #####
  #####################################
  
  #Region drop down for overall COVID
  output$region_dropdown_sars = renderUI({
    req(credentials()$user_auth)
    credentials()$info
    
    region_choices <- region_names_sars %>%
      separate(
        col=Region,
        into = c("Region1","Region2"),
        sep = ", ",
        remove=TRUE) %>%
      select(-utility)
    
    region1 <- data.frame(Region = unique(region_choices$Region1))
    region2 <- data.frame(Region = unique(region_choices$Region2))
    
    all_option <- data.frame(Region = "All Regions")
    
    correctorder_choices <- rbind(all_option, region1, region2) %>%
      filter(!is.na(Region)) %>%
      distinct(Region) %>%
      arrange(Region)
    
    selectInput("region_percentile_sars",
                "Select Region:",
                choices = correctorder_choices)
    
  })
  
  ##############################################
  #####   OVERALL CLASSIFICATION DROP DOWN #####
  ##############################################
  
  #Region drop down for overall COVID
  output$classification_dropdown_sars = renderUI({
    req(credentials()$user_auth)
    credentials()$info
    
    class_choices <- metadata_survey %>%
      separate(
        col=surveyed_classification,
        into = c("Classification1","Classification2", "Classification3"),
        sep = ", ",
        remove=TRUE) %>%
      select(-utility)
    
    class1 <- data.frame(Classification = unique(class_choices$Classification1))
    class2 <- data.frame(Classification = unique(class_choices$Classification2))
    class3 <- data.frame(Classification = unique(class_choices$Classification3))
    
    all_option <- data.frame(Classification = "All Classifications")
    
    correctorder_choices <- rbind(all_option, class1, class2, class3) %>%
      filter(!is.na(Classification)) %>%
      distinct(Classification) %>%
      arrange(Classification)
    
    selectInput("classification_percentile_sars",
                "Select Classification:",
                choices = correctorder_choices)
    
  })
  
  
  #####################################
  #####   COVID COUNTY DROP DOWNS #####
  #####################################
  
  #County drop down for COVID
  output$counties_in_utility_choices_cases_sars = renderUI({
    req(credentials()$user_auth)
    credentials()$info
    
    topchoices <- countyperutility %>%
      filter(utility %in% input$utility) %>%
      separate(
        col=overlapping_counties,
        into = c("overlap_county1","overlap_county2", "overlap_county3"),
        sep = ",",
        remove=TRUE) %>%
      mutate(overlap_county1 = case_when(overlap_county1 == "" ~ NA, TRUE~overlap_county1)) %>%
      select(-utility) 
    
    topchoices <- c(topchoices$primary_county, topchoices$overlap_county1, topchoices$overlap_county2, topchoices$overlap_county3, "----")
    topchoices <- data.frame(county = topchoices)
    
    choices <- countyperutility %>%
      separate_rows(overlapping_counties, sep = ", ") %>%
      mutate(combined_column = paste(primary_county, overlapping_counties, sep = "_")) %>%
      separate_rows(combined_column, sep = "_") %>%
      select(combined_column) %>%
      filter(!combined_column == "") %>%
      distinct() %>%
      arrange(combined_column) %>% 
      rename(county = combined_column)
    
    choices <- choices[!(choices$county %in% topchoices$county), ]
    choices <- data.frame(county = choices)
    
    
    correctorder_choices <- rbind(topchoices, choices) %>%
      filter(!is.na(county))   
    
    selectInput("sars_case_county",
                "Select County:",
                choices = unique(correctorder_choices))
    
  })
  
  #county drop down for covid hosp
  output$counties_in_utility_choices_hosp_sars = renderUI({
    req(credentials()$user_auth)
    credentials()$info
    
    topchoices <- countyperutility %>%
      filter(utility %in% input$utility) %>%
      separate(
        col=overlapping_counties,
        into = c("overlap_county1","overlap_county2", "overlap_county3"),
        sep = ",",
        remove=TRUE) %>%
      mutate(overlap_county1 = case_when(overlap_county1 == "" ~ NA, TRUE~overlap_county1)) %>%
      select(-utility) 
    
    topchoices <- c(topchoices$primary_county, topchoices$overlap_county1, topchoices$overlap_county2, topchoices$overlap_county3, "----")
    topchoices <- data.frame(county = topchoices)
    
    
    choices <- countyperutility %>%
      separate_rows(overlapping_counties, sep = ", ") %>%
      mutate(combined_column = paste(primary_county, overlapping_counties, sep = "_")) %>%
      separate_rows(combined_column, sep = "_") %>%
      select(combined_column) %>%
      filter(!combined_column == "") %>%
      distinct() %>%
      arrange(combined_column) %>% 
      rename(county = combined_column)
    
    choices <- choices[!(choices$county %in% topchoices$county), ]
    choices <- data.frame(county = choices)
    
    
    correctorder_choices <- rbind(topchoices, choices) %>%
      filter(!is.na(county))   
    
    selectInput("sars_hosp_county",
                "Select County:",
                choices = unique(correctorder_choices))
    
  })
  
  
  #populate county NAMES in utility drop down menu
  #for cases plot
  output$counties_in_utility_list_cases_sars = renderUI({
    req(credentials()$user_auth)
    credentials()$info
    
    filtered_counties <- countyperutility %>%
      filter(utility %in% input$utility) %>%
      separate(
        col=overlapping_counties,
        into = c("overlap_county1","overlap_county2", "overlap_county3"),
        sep = ",",
        remove=TRUE) %>%
      mutate(overlap_county1 = case_when(overlap_county1 == "" ~ NA, TRUE~overlap_county1))
    
    if(is.na(filtered_counties$overlap_county1)){
      HTML(paste(input$utility, "is located in", "<b>", filtered_counties$primary_county, "</b>", "county."))
    }
    else if(!is.na(filtered_counties$overlap_county1) && is.na(filtered_counties$overlap_county2)){
      HTML(paste(input$utility, "is located primarily in", "<b>", filtered_counties$primary_county, "</b>", 
                 "county and overlaps with", "<b>", filtered_counties$overlap_county1, "</b>", "county"))
    }
    else if(!is.na(filtered_counties$overlap_county1) && !is.na(filtered_counties$overlap_county2) && is.na(filtered_counties$overlap_county3)){
      HTML(paste(input$utility, "is located primarily in", "<b>", filtered_counties$primary_county, "</b>", 
                 "county. <br> <br> It also overlaps with", "<b>", filtered_counties$overlap_county1, "</b>", 
                 "and", "<b>", filtered_counties$overlap_county2, "</b>", "counties."))
    }
    else if(!is.na(filtered_counties$overlap_county1) && !is.na(filtered_counties$overlap_county2) && !is.na(filtered_counties$overlap_county3)){
      HTML(paste(input$utility, "is located primarily in", "<b>", filtered_counties$primary_county, "</b>", 
                 "county. <br> <br> It also overlaps with", "<b>", filtered_counties$overlap_county1, "</b>", 
                 ", ", "<b>", filtered_counties$overlap_county2, "</b>", 
                 "and", "<b>", filtered_counties$overlap_county3, "</b>", "counties."))
    }
    
  })
  
  #for hosp plot
  output$counties_in_utility_list_hosp_sars = renderUI({
    req(credentials()$user_auth)
    credentials()$info
    
    
    filtered_counties <- countyperutility %>%
      filter(utility %in% input$utility) %>%
      separate(
        col=overlapping_counties,
        into = c("overlap_county1","overlap_county2", "overlap_county3"),
        sep = ",",
        remove=TRUE) %>%
      mutate(overlap_county1 = case_when(overlap_county1 == "" ~ NA, TRUE~overlap_county1))
    
    if(is.na(filtered_counties$overlap_county1)){
      HTML(paste(input$utility, "is located in", "<b>", filtered_counties$primary_county, "</b>", "county."))
    }
    else if(!is.na(filtered_counties$overlap_county1) && is.na(filtered_counties$overlap_county2)){
      HTML(paste(input$utility, "is located primarily in", "<b>", filtered_counties$primary_county, "</b>", 
                 "county and overlaps with", "<b>", filtered_counties$overlap_county1, "</b>", "county"))
    }
    else if(!is.na(filtered_counties$overlap_county1) && !is.na(filtered_counties$overlap_county2) && is.na(filtered_counties$overlap_county3)){
      HTML(paste(input$utility, "is located primarily in", "<b>", filtered_counties$primary_county, "</b>", 
                 "county. <br> <br> It also overlaps with", "<b>", filtered_counties$overlap_county1, "</b>", 
                 "and", "<b>", filtered_counties$overlap_county2, "</b>", "counties."))
    }
    else if(!is.na(filtered_counties$overlap_county1) && !is.na(filtered_counties$overlap_county2) && !is.na(filtered_counties$overlap_county3)){
      HTML(paste(input$utility, "is located primarily in", "<b>", filtered_counties$primary_county, "</b>", 
                 "county. <br> <br> It also overlaps with", "<b>", filtered_counties$overlap_county1, "</b>", 
                 ", ", "<b>", filtered_counties$overlap_county2, "</b>", 
                 "and", "<b>", filtered_counties$overlap_county3, "</b>", "counties."))
    }
    
  })
  
  
  
  #####################################
  #####   RSV COUNTY DROP DOWNS #####
  #####################################
  
  ##County drop down for RSV
  output$counties_in_utility_choices_cases_rsv = renderUI({
    req(credentials()$user_auth)
    credentials()$info
    
    topchoices <- countyperutility %>%
      filter(utility %in% input$utility) %>%
      separate(
        col=overlapping_counties,
        into = c("overlap_county1","overlap_county2", "overlap_county3"),
        sep = ",",
        remove=TRUE) %>%
      mutate(overlap_county1 = case_when(overlap_county1 == "" ~ NA, TRUE~overlap_county1)) %>%
      select(-utility) 
    
    topchoices <- c(topchoices$primary_county, topchoices$overlap_county1, topchoices$overlap_county2, topchoices$overlap_county3, "----")
    topchoices <- data.frame(county = topchoices)
    
    
    choices <- countyperutility %>%
      separate_rows(overlapping_counties, sep = ", ") %>%
      mutate(combined_column = paste(primary_county, overlapping_counties, sep = "_")) %>%
      separate_rows(combined_column, sep = "_") %>%
      select(combined_column) %>%
      filter(!combined_column == "") %>%
      distinct() %>%
      arrange(combined_column) %>% 
      rename(county = combined_column)
    
    choices <- choices[!(choices$county %in% topchoices$county), ]
    choices <- data.frame(county = choices)
    
    
    correctorder_choices <- rbind(topchoices, choices) %>%
      filter(!is.na(county))   
    
    selectInput("rsv_case_county",
                "Select County:",
                choices = unique(correctorder_choices))
    
  })
  
  #populate county names in utility
  #for cases plot
  output$counties_in_utility_list_cases_rsv = renderUI({
    req(credentials()$user_auth)
    credentials()$info
    
    filtered_counties <- countyperutility %>%
      filter(utility %in% input$utility) %>%
      separate(
        col=overlapping_counties,
        into = c("overlap_county1","overlap_county2", "overlap_county3"),
        sep = ",",
        remove=TRUE) %>%
      mutate(overlap_county1 = case_when(overlap_county1 == "" ~ NA, TRUE~overlap_county1))
    
    if(is.na(filtered_counties$overlap_county1)){
      HTML(paste(input$utility, "is located in", "<b>", filtered_counties$primary_county, "</b>", "county."))
    }
    else if(!is.na(filtered_counties$overlap_county1) && is.na(filtered_counties$overlap_county2)){
      HTML(paste(input$utility, "is located primarily in", "<b>", filtered_counties$primary_county, "</b>", 
                 "county and overlaps with", "<b>", filtered_counties$overlap_county1, "</b>", "county"))
    }
    else if(!is.na(filtered_counties$overlap_county1) && !is.na(filtered_counties$overlap_county2) && is.na(filtered_counties$overlap_county3)){
      HTML(paste(input$utility, "is located primarily in", "<b>", filtered_counties$primary_county, "</b>", 
                 "county. <br> <br> It also overlaps with", "<b>", filtered_counties$overlap_county1, "</b>", 
                 "and", "<b>", filtered_counties$overlap_county2, "</b>", "counties."))
    }
    else if(!is.na(filtered_counties$overlap_county1) && !is.na(filtered_counties$overlap_county2) && !is.na(filtered_counties$overlap_county3)){
      HTML(paste(input$utility, "is located primarily in", "<b>", filtered_counties$primary_county, "</b>", 
                 "county. <br> <br> It also overlaps with", "<b>", filtered_counties$overlap_county1, "</b>", 
                 ", ", "<b>", filtered_counties$overlap_county2, "</b>", 
                 "and", "<b>", filtered_counties$overlap_county3, "</b>", "counties."))
    }
    
  })
 
  
  #####################################
  #####   Flu COUNTY DROP DOWNS #####
  #####################################
  
  ##County drop down for flu
  output$counties_in_utility_choices_cases_flu = renderUI({
    req(credentials()$user_auth)
    credentials()$info
    
    topchoices <- countyperutility %>%
      filter(utility %in% input$utility) %>%
      separate(
        col=overlapping_counties,
        into = c("overlap_county1","overlap_county2", "overlap_county3"),
        sep = ",",
        remove=TRUE) %>%
      mutate(overlap_county1 = case_when(overlap_county1 == "" ~ NA, TRUE~overlap_county1)) %>%
      select(-utility) 
    
    topchoices <- c(topchoices$primary_county, topchoices$overlap_county1, topchoices$overlap_county2, topchoices$overlap_county3, "----")
    topchoices <- data.frame(county = topchoices)
    
    
    choices <- countyperutility %>%
      separate_rows(overlapping_counties, sep = ", ") %>%
      mutate(combined_column = paste(primary_county, overlapping_counties, sep = "_")) %>%
      separate_rows(combined_column, sep = "_") %>%
      select(combined_column) %>%
      filter(!combined_column == "") %>%
      distinct() %>%
      arrange(combined_column) %>% 
      rename(county = combined_column)
    
    choices <- choices[!(choices$county %in% topchoices$county), ]
    choices <- data.frame(county = choices)
    
    
    correctorder_choices <- rbind(topchoices, choices) %>%
      filter(!is.na(county))   
    
    selectInput("flu_case_county",
                "Select County:",
                choices = unique(correctorder_choices))
    
  })
  
  #populate county names in utility
  #for cases plot
  output$counties_in_utility_list_cases_flu = renderUI({
    req(credentials()$user_auth)
    credentials()$info
    
    filtered_counties <- countyperutility %>%
      filter(utility %in% input$utility) %>%
      separate(
        col=overlapping_counties,
        into = c("overlap_county1","overlap_county2", "overlap_county3"),
        sep = ",",
        remove=TRUE) %>%
      mutate(overlap_county1 = case_when(overlap_county1 == "" ~ NA, TRUE~overlap_county1))
    
    if(is.na(filtered_counties$overlap_county1)){
      HTML(paste(input$utility, "is located in", "<b>", filtered_counties$primary_county, "</b>", "county."))
    }
    else if(!is.na(filtered_counties$overlap_county1) && is.na(filtered_counties$overlap_county2)){
      HTML(paste(input$utility, "is located primarily in", "<b>", filtered_counties$primary_county, "</b>", 
                 "county and overlaps with", "<b>", filtered_counties$overlap_county1, "</b>", "county"))
    }
    else if(!is.na(filtered_counties$overlap_county1) && !is.na(filtered_counties$overlap_county2) && is.na(filtered_counties$overlap_county3)){
      HTML(paste(input$utility, "is located primarily in", "<b>", filtered_counties$primary_county, "</b>", 
                 "county. <br> <br> It also overlaps with", "<b>", filtered_counties$overlap_county1, "</b>", 
                 "and", "<b>", filtered_counties$overlap_county2, "</b>", "counties."))
    }
    else if(!is.na(filtered_counties$overlap_county1) && !is.na(filtered_counties$overlap_county2) && !is.na(filtered_counties$overlap_county3)){
      HTML(paste(input$utility, "is located primarily in", "<b>", filtered_counties$primary_county, "</b>", 
                 "county. <br> <br> It also overlaps with", "<b>", filtered_counties$overlap_county1, "</b>", 
                 ", ", "<b>", filtered_counties$overlap_county2, "</b>", 
                 "and", "<b>", filtered_counties$overlap_county3, "</b>", "counties."))
    }
    
  })
  
  
  #####################################
  #####          MAP              #####
  #####################################
  
  output$wwmap <- renderLeaflet({
    req(credentials()$user_auth)
    credentials()$info
    
    # Define a function to generate the pop-up content for each row in the data frame
    generatePopupContent <- function(wwshapes) {
      result <- 
        #if 1 is NA
        ifelse(wwshapes$lpha1_contactname1 == "Not available",
               paste0(wwshapes$lpha1_contactname1),
               
               #if 2 empty, print 1
               ifelse(is.na(wwshapes$lpha1_contactname2),
                      paste0(wwshapes$lpha1_contactname1, " (", wwshapes$lpha1_contactemail1, ")"),
                      
                      #if 3 empty, print 1 + 2       
                      ifelse(is.na(wwshapes$lpha1_contactname3),
                             paste0(wwshapes$lpha1_contactname1, " (", wwshapes$lpha1_contactemail1, ")",
                                    "<br>",
                                    paste0(wwshapes$lpha1_contactname2, " (", wwshapes$lpha1_contactemail2, ")")),
                             #if 4 empty, print 1 + 2 +3      
                             ifelse(is.na(wwshapes$lpha1_contactname4),
                                    paste0(wwshapes$lpha1_contactname1, " (", wwshapes$lpha1_contactemail1, ")",
                                           "<br>",
                                           paste0(wwshapes$lpha1_contactname2, " (", wwshapes$lpha1_contactemail2, ")"),
                                           "<br>",
                                           paste0(wwshapes$lpha1_contactname3, " (", wwshapes$lpha1_contactemail3, ")")),
                                    #if 5 empty, print 1 + 2 +3 +4     
                                    ifelse(is.na(wwshapes$lpha1_contactname5),
                                           paste0(wwshapes$lpha1_contactname1, " (", wwshapes$lpha1_contactemail1, ")",
                                                  "<br>",
                                                  paste0(wwshapes$lpha1_contactname2, " (", wwshapes$lpha1_contactemail2, ")"),
                                                  "<br>",
                                                  paste0(wwshapes$lpha1_contactname3, " (", wwshapes$lpha1_contactemail3, ")"),
                                                  "<br>",
                                                  paste0(wwshapes$lpha1_contactname4, " (", wwshapes$lpha1_contactemail4, ")")),
                                           
                                           #else print 1 + 2+ 3+ 4 + 5
                                           paste0(wwshapes$lpha1_contactname1, " (", wwshapes$lpha1_contactemail1, ")",
                                                  "<br>",
                                                  paste0(wwshapes$lpha1_contactname2, " (", wwshapes$lpha1_contactemail2, ")"),
                                                  "<br>",
                                                  paste0(wwshapes$lpha1_contactname3, " (", wwshapes$lpha1_contactemail3, ")"),
                                                  "<br>",
                                                  paste0(wwshapes$lpha1_contactname4, " (", wwshapes$lpha1_contactemail4, ")"),
                                                  "<br>",
                                                  paste0(wwshapes$lpha1_contactname5, " (", wwshapes$lpha1_contactemail5, ")"))
                                    )
                             )
                      )
               )
        )
      
      popupContent <- paste0(
        "<b>Utility: </b>", wwshapes$utility, "<br><br>",
        "<b>Most Recent Sample Date: </b>", wwshapes$measure_date, "<br><br>",
        "<b>Sewershed Population Est.: </b>",
        format(round(as.numeric(wwshapes$population_served), 0), nsmall = 0, big.mark = ","), "<br><br>",
        "<b>Primary LPHA: </b>", wwshapes$lpha1, "<br><br>",
        "<b>Primary LPHA Contact: </b>", result, "<br><br>",
        "<b>Participation Status: </b>", wwshapes$participation_status, "<br><br>",
        "<b>Program: </b>", wwshapes$legend_label
      )
      
      return(popupContent)
    }
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = countyshapes,
        color = "#808080",
        weight = 2,
        opacity = .5,
        fillColor = "#d1ddba",
        fillOpacity = .1
      ) %>%
      addPolygons(
        data = wwshapes,
        #color = "black",
        color = ~ifelse(utility %in% filtered_sites, "black", "#606060"),
        weight = 2,
        opacity = 1,
        fillColor = ~fill_color,
        fillOpacity = 0.5,
        popup = generatePopupContent(wwshapes),
        popupOptions = popupOptions(
          minWidth = 50,
          maxWidth = 300,
          closeOnClick = TRUE
        )
      ) %>%
      leaflet::addLegend(
        position = "bottomleft",
        colors = c("#DDCC77", "grey"),
        labels = c("Sentinel Sites", "Emergency Surveillance/Historic Sites"),
        title = "Legend"
      ) %>%
      fitBounds(
        lng1 = -109,
        lng2 = -101.9,
        lat1 = 41.1,
        lat2 = 37
      )
    
    
    
  })
  
  #######################################
  ##    OVERALL SARS-COV-2 TRENDS     ###
  #######################################
  ##all utilities trends plot
  output$trends_plot_allutilities <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    
    ##calculate trend individual data
    alpha_set=.3
    
    #filter uot private utilities
    publish_only <- IDB_masterlist %>% 
      filter(public_repository == "Publish") %>%
      filter(participation_status == "Active")
    
    regions <- region_names_sars %>%
      separate(
        col=Region,
        into = c("Region1","Region2"),
        sep = ", ",
        remove=TRUE)
    
    regions$Region_All <- "All Regions"
    
    metadata_survey_data <- metadata_survey %>%
      separate(
        col=surveyed_classification,
        into = c("Classification1","Classification2", "Classification3"),
        sep = ", ",
        remove=TRUE)
    
    metadata_survey_data$Class_All <- "All Classifications"
    
    all_plot_dat_outbounds <- plot_dat %>%
      filter(utility %in% publish_only$utility) %>%
      filter(measure_date >= (Sys.Date()-60)) %>%
      group_by(utility) %>%
      slice_max(measure_date)
    
    all_plot_dat <- all_plot_dat_outbounds %>%
      left_join(regions, by = "utility") %>%     
      filter(Region1 %in% input$region_percentile_sars | Region2 %in% input$region_percentile_sars | Region_All %in% input$region_percentile_sars) %>%
      left_join(metadata_survey_data, by = "utility") %>%     
      filter(Classification1 %in% input$classification_percentile_sars | Classification2 %in% input$classification_percentile_sars | Classification3 %in% input$classification_percentile_sars | Class_All %in% input$classification_percentile_sars)
    
    
    all_plot_dat$sig <- factor(all_plot_dat$sig, levels = c("Trend Slope Significant", "Trend Slope Not Significant"))
    
    ggplotly(
      ggplot(data = all_plot_dat, aes(x = slope, y = level_tile)) +
        geom_rect(aes(xmin = 0, xmax = 1e10, ymin = 0.5, ymax = 1e10, fill = "Increasing Trend <br>High Percentile"), alpha = alpha_set) +
        geom_rect(aes(xmin = -1e10, xmax = 0, ymin = -1e10, ymax = 0.5, fill = "Decreasing Trend <br>Low Percentile"), alpha = alpha_set) +
        geom_rect(aes(xmin = -1e10, xmax = 0, ymin = 0.5, ymax = 1e10, fill = "Decreasing Trend <br>High Percentile"), alpha = alpha_set) +
        geom_rect(aes(xmin = 0, xmax = 1e10, ymin = -1e10, ymax = 0.5, fill = "Increasing Trend <br>Low Percentile"), alpha = alpha_set) +
        geom_point(aes(fill = factor(sig),
                       size = population_served,
                       text = paste("<b>Utility:</b>", utility,
                                    "<br><b>Sample Date:</b> ",
                                    measure_date,
                                    "<br><b>Sample Type:</b>",
                                    sample_type,
                                    "<br><b>Sewershed Population Est.: </b>",
                                    paste0(format(round(as.numeric((population_served)), 0), nsmall = 0, big.mark = ",")),
                                    "<br><b>Trend:</b>",
                                    format(round(as.numeric(all_plot_dat$slope), 2), nsmall = 0, big.mark = ","),
                                    "<br><b>Percentile: </b>",
                                    paste0(format(round(as.numeric((all_plot_dat$level_tile) * 100), 0), nsmall = 0, big.mark = ","), "%"),
                                    "<br><b>P-value: </b>",
                                    round(p_val, 4)
                                    
                       )),
                   shape = 21, alpha = 0.7, show.legend = FALSE) +
        geom_hline(yintercept = 0.5) +
        geom_vline(xintercept = 0) +
        theme_bw(base_size = 15) +
        scale_fill_manual(values = c("#67001F", "#D6604F", "#D1E5F0", "#2166AC", "gray80", "black"),  # Specify your fill colors
                          labels = c("Increasing Trend <br>High Percentile",
                                     "Increasing Trend <br>Low Percentile",
                                     "Decreasing Trend <br>High Percentile",
                                     "Decreasing Trend <br>Low Percentile",
                                     "Trend Slope Significant",
                                     "Trend Slope Not Significant"),
                          breaks = c("Increasing Trend <br>High Percentile",
                                     "Increasing Trend <br>Low Percentile",
                                     "Decreasing Trend <br>High Percentile",
                                     "Decreasing Trend <br>Low Percentile",
                                     "Trend Slope Significant",
                                     "Trend Slope Not Significant")
        ) +
        labs(x = "Trend (slope)", y = "Level (percentile)") +
        coord_cartesian(xlim = c(min(all_plot_dat_outbounds$slope) - 0.01, max(all_plot_dat_outbounds$slope) + 0.01),
                        ylim = c(min(all_plot_dat_outbounds$level_tile) - 0.1, max(all_plot_dat_outbounds$level_tile) + 0.1),
                        expand = FALSE) +
        theme(text = element_text(family = "Arial", size = 8)),
      tooltip = "text"
    )  %>%      
      layout(
        title = list(
          text =
            if(input$region_percentile_sars == "All Regions"){paste("Statewide SARS-CoV-2 Wastewater Trends & Levels")}
          else {paste(input$region_percentile_sars, "Wastewater Trends & Levels")}
        ),
        xaxis = list(title = "Slope of Recent Trend", fixedrange = FALSE),
        yaxis = list(title = "Viral Concentration Percentile", fixedrange = FALSE),
        showlegend = TRUE,  # Show legend
        legend = list(
          title = NA,
          orientation = "h",
          y = -0.2,
          font = list(size = 10.5),
          x = 0
        )
      )
    
  })
  
  
  ##all utilities trends plot
  output$region_quadtable_sars <- function()({
    req(credentials()$user_auth)
    credentials()$info
    
    #filter out private utilities
    publish_only <- IDB_masterlist %>% 
      filter(public_repository == "Publish") %>%
      filter(participation_status == "Active")
    
    regions <- region_names_sars %>%
      separate(
        col=Region,
        into = c("Region1","Region2"),
        sep = ", ",
        remove=TRUE)
    
    regions$Region_All <- "All Regions"
    
    metadata_survey_data <- metadata_survey %>%
      separate(
        col=surveyed_classification,
        into = c("Classification1","Classification2", "Classification3"),
        sep = ", ",
        remove=TRUE)
    
    metadata_survey_data$Class_All <- "All Classifications"
    
    all_plot_dat_outbounds <- plot_dat %>%
      filter(utility %in% publish_only$utility) %>%
      filter(measure_date >= (Sys.Date()-60)) %>%
      group_by(utility) %>%
      slice_max(measure_date)
    
    all_plot_dat <- all_plot_dat_outbounds %>%
      left_join(regions, by = "utility") %>%     
      filter(Region1 %in% input$region_percentile_sars | Region2 %in% input$region_percentile_sars | Region_All %in% input$region_percentile_sars) %>%
      left_join(metadata_survey_data, by = "utility") %>%     
      filter(Classification1 %in% input$classification_percentile_sars | Classification2 %in% input$classification_percentile_sars | Classification3 %in% input$classification_percentile_sars |Class_All %in% input$classification_percentile_sars)
    
    
    all_plot_dat$sig <- factor(all_plot_dat$sig, levels = c("Trend Slope Significant", "Trend Slope Not Significant"))
    
    regions_countquad_table <- all_plot_dat %>%
      mutate(quadrant = case_when(slope >= 0 & level_tile >= 0.5 ~ "Increasing trend, at or above 50th percentile", 
                                  slope >= 0 & level_tile <= 0.5 ~ "Increasing trend, below 50th percentile",
                                  slope <= 0 & level_tile >= 0.5 ~ "Decreasing trend, at or above 50th percentile",
                                  slope <= 0 & level_tile <= 0.5 ~ "Decreasing trend, below 50th percentile",
                                  TRUE ~ NA)) %>%
      group_by(quadrant)
    
    regions_countquad_table$quadrant <- factor(regions_countquad_table$quadrant, level = c("Increasing trend, at or above 50th percentile", 
                                                                                           "Increasing trend, below 50th percentile",
                                                                                           "Decreasing trend, at or above 50th percentile", 
                                                                                           "Decreasing trend, below 50th percentile"))

  
    regions_countquad_table <- regions_countquad_table %>%
      summarise(count = n()) %>%
      ungroup() %>%
      complete(quadrant) %>%
      mutate(count = case_when(is.na(count) ~ 0, TRUE ~ count)) %>%
      mutate(count = round(count, 0)) %>%
      mutate(cellColor = c("rgba(130, 0, 31, 0.3)", "rgba(214, 96, 79, 0.3)", "rgba(209, 229, 240, 0.3)", "rgba(33, 102, 172, 0.3)")) 
    
    
    regions_countquad_table %>%
      mutate(count = cell_spec(count, "html", color = "black", bold = F), 
             quadrant = cell_spec(x = quadrant, 
                                  format = "html", 
                                  color = "black", 
                                  bold = T, 
                                  extra_css = paste(paste('background-color', cellColor, sep = ': '), # combine background-color CSS rule with the observation vector value
                                                    'display: inline-block', # extremely important CSS modifier for the span tag in the table cell
                                                    'padding: 5px', # expand the field of text
                                                    'margin: -5px', # expand the field of text
                                                    sep = "; "), # CSS notation rule
             )
      ) %>%
      select(-cellColor) %>% # exclude cellColor vector
      pivot_wider(names_from = quadrant, values_from = count) %>%
      kable(format ="html", align = 'c', escape = F) %>% 
      kable_styling(bootstrap_options = c("bordered", full_width = F, font_size = 10), position = "center") %>%
      add_header_above(c("Number of Utilities in Category:" = 4))
    
    
    
  })
  
  
  #################################
  ##        SARS-COV-2 Data     ###
  #################################
  ##individual utility trends plot
  output$trends_plot_individualutility <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
 
    ##calculate trend individual data
    alpha_set=.3
    
    single_utility <- plot_dat %>%
      filter(utility %in% input$utility) %>%
      arrange(rev(measure_date)) %>%
      slice(1:3) %>%
      mutate(trail_alpha=percent_rank(measure_date)) %>%
      mutate(slope = round(slope, 2)) %>%
      mutate(level_scale = round(level_scale,2)) %>%
      arrange(measure_date)
    
    single_utility$sig <- factor(single_utility$sig, levels = c("Trend Slope Significant", "Trend Slope Not Significant"))
    
    
    
    min_x <- if (min(single_utility$slope) > -0.05) {
      result <- -0.05
    } else {
      result <- min(single_utility$slope) + -0.05
    }
    
    max_x <- if (max(single_utility$slope) == 0.10) {
      result <- 0.20
    } else if (max(single_utility$slope) == 0.20) {
      result <- 0.30
    } else if (max(single_utility$slope) == 0.30) {
      result <- 0.40
    } else if (max(single_utility$slope) == 0.50) {
      result <- 0.50
    } else if (max(single_utility$slope) == 0.00) {
      result <- 0.10
    } else {
      result <- ceiling(max(single_utility$slope) * 10) / 10 #round UP to nearest tenth
      
    }
    
    
    min_y <- -0.1
    
    max_y <- 1.1
 
    
    ggplotly(
      ggplot(data = single_utility, aes(x = slope, y = level_tile)) +
        geom_rect(aes(xmin = 0, xmax = 1e10, ymin = 0.5, ymax = 1e10, fill = "Increasing Trend <br>High Percentile"), alpha = alpha_set) +
        geom_rect(aes(xmin = -1e10, xmax = 0, ymin = -1e10, ymax = 0.5, fill = "Decreasing Trend <br>Low Percentile"), alpha = alpha_set) +
        geom_rect(aes(xmin = -1e10, xmax = 0, ymin = 0.5, ymax = 1e10, fill = "Decreasing Trend <br>High Percentile"), alpha = alpha_set) +
        geom_rect(aes(xmin = 0, xmax = 1e10, ymin = -1e10, ymax = 0.5, fill = "Increasing Trend <br>Low Percentile"), alpha = alpha_set) +
        geom_point(aes(fill = factor(sig),
                       #alpha=trail_alpha, 
                       text = paste("<br><b>Sample Date:</b> ",
                                    measure_date,
                                    "<br><b>Sample Type:</b>",
                                    sample_type,
                                    "<br><b>Trend:</b>",
                                    format(round(as.numeric(slope), 2), nsmall = 0, big.mark = ","),
                                    "<br><b>Percentile: </b>",
                                    paste0(format(round(as.numeric((level_tile)*100), 0), nsmall = 0, big.mark = ","), "%"),
                                    "<br><b>P-value: </b>",
                                    round(p_val, 4)
                       )), 
                   size = 3,
                   show.legend = F) +
        geom_path(aes(group = 1), color = "black", linetype = "dashed", show.legend = FALSE) +
        geom_text(aes(label = format(measure_date, "%Y-%m-%d")), size = 3, position = position_nudge(x = 0.001)) +
        scale_fill_manual(#name = "Trends & Percentiles",
          values = c("#67001F", "#D6604F", "#D1E5F0", "#2166AC", "gray80", "black"),  # Specify your fill colors
          labels = c("Increasing Trend <br>High Percentile",
                     "Increasing Trend <br>Low Percentile",
                     "Decreasing Trend <br>High Percentile",
                     "Decreasing Trend <br>Low Percentile",
                     "Trend Slope Significant",
                     "Trend Slope Not Significant"),
          breaks = c("Increasing Trend <br>High Percentile",
                     "Increasing Trend <br>Low Percentile",
                     "Decreasing Trend <br>High Percentile",
                     "Decreasing Trend <br>Low Percentile",
                     "Trend Slope Significant",
                     "Trend Slope Not Significant")
        ) +
        coord_cartesian(xlim = c(min_x, max_x), 
                        ylim = c(min_y, max_y), 
                        expand = FALSE) +
        geom_hline(yintercept = 0.5) +
        geom_vline(xintercept = 0) +
        theme_bw(base_size = 15) +
        labs(x="Slope of Recent Trend", y="Pecentile") +
        theme(legend.position = "none") +        
        theme(text = element_text(family = "Arial", size = 8)),
      tooltip = "text"
    ) %>%
      style(textposition = "right") %>%
      layout(
        title = list(
          text = paste("SARS-CoV-2 Trend Summary Plot for", input$utility)
        ),
        xaxis = list(title = "Slope of Recent Trends", fixedrange = FALSE),
        yaxis = list(title = "Viral Concentration Percentile", fixedrange = FALSE),
        showlegend = TRUE,  # Show legend
        legend = list(
          title = NA,
          orientation = "h",
          y = -0.2,
          font = list(size = 10.5)#,
        )
      )
  })
  
  
  
  
  
  
  #viral concentration graph
  viralconc_sars_input <- reactive({
    req(credentials()$user_auth)
    credentials()$info
    
    req(input$log_button_rawconc_sars)
    
    if(input$log_button_rawconc_sars == "Linear Scale"){
      viralconc_sars_input_dataset <- wwdatafull %>%
        filter(utility %in% input$utility) %>%
        filter(pcr_target == "sars-cov-2") %>%
        filter(!is.na(viral_conc_raw)) %>%
        rename(output_viral_conc = viral_conc_raw) 
      
      max_trenddate <- viralconc_sars_input_dataset %>%
        filter(!trend == "") %>%
        select(measure_date)
      
      max_trenddate <- max_trenddate[1,1]
      
      viralconc_sars_input_dataset <- viralconc_sars_input_dataset %>%
        mutate(trend = ifelse((trend == "" & measure_date < max_trenddate),"Historic Data", trend)) %>%
        mutate(trend = case_when(is.na(trend) ~ "Historic Data", TRUE ~ trend)) %>%
        mutate(trend = case_when(trend == "" ~ "Insufficient Data", TRUE ~ trend))
    }
    else if(input$log_button_rawconc_sars == "Log Scale"){
      viralconc_sars_input_dataset <- wwdatafull %>%
        filter(utility %in% input$utility) %>%
        filter(pcr_target == "sars-cov-2") %>%
        filter(!is.na(log_viral_conc_raw)) %>%
        rename(output_viral_conc = log_viral_conc_raw)
      
      max_trenddate <- viralconc_sars_input_dataset %>%
        filter(!trend == "") %>%
        select(measure_date) 
      
      max_trenddate <- max_trenddate[1,1]
      
      viralconc_sars_input_dataset <- viralconc_sars_input_dataset %>%
        mutate(trend = ifelse((trend == "" & measure_date < max_trenddate),"Historic Data", trend)) %>%
        mutate(trend = case_when(is.na(trend) ~ "Historic Data", TRUE ~ trend)) %>%
        mutate(trend = case_when(trend == "" ~ "Insufficient Data", TRUE ~ trend))
    }
    
    viralconc_sars_input_dataset$trend <- factor(viralconc_sars_input_dataset$trend, levels = c("Increasing", "Plateau", "Decreasing", "Insufficient Data", "Historic Data"))
    
    return(viralconc_sars_input_dataset)
  })   
  
  output$wwConcGraph_sars <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    
    #input <- data.frame(utility = "Alamosa", daterange1 = as.Date(c("2024-06-24", "2024-07-08")), rsv_outbreak_county = "Adams")
    
    #calcualte max y so y-axis auto adjusts based on data presented in selected date range
    max_y <- viralconc_sars_input() %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    max_y <- max(max_y$output_viral_conc, na.rm = TRUE)
    max_y <- abs(max_y)
    
    lod_lp2 <- viralconc_sars_input() %>%
      group_by(utility) %>%
      complete(measure_date = seq.Date(as.Date("2024-07-12"), to = as.Date("2024-09-29"), by="day")) %>%
      filter(measure_date < "2024-09-30") %>%
      mutate(lod_limit = 2000) 
    
    lod_lp3 <- viralconc_sars_input() %>%
      group_by(utility) %>%
      complete(measure_date = seq.Date(as.Date("2024-09-30"), to = Sys.Date(), by="day")) %>%
      filter(measure_date >= "2024-09-30") %>%
      mutate(lod_limit = 1200)
    
    ggplotly(
      ggplot() +
        geom_tile(
          data = lod_lp2, 
          aes(x = measure_date, y = lod_limit/2,
              height = 2000,
              text = paste(
                "<b>Date:</b>",
                measure_date,
                "<br><b>Limit of Detection:</b> ",
                lod_limit, "copies/L")
          ),
          fill = "lightblue", color = "lightblue"
        ) + 
        geom_tile(
          data = lod_lp3, 
          aes(x = measure_date, y = lod_limit/2,
              height = 1200,
              text = paste(
                "<b>Date:</b>",
                measure_date,
                "<br><b>Limit of Detection:</b> ",
                lod_limit, "copies/L")
          ),
          fill = "lightblue", color = "lightblue"
        ) + 
        geom_line(
          data = viralconc_sars_input(),
          aes(x = measure_date, y = output_viral_conc
          )
        ) +
      geom_point(
        data = viralconc_sars_input() %>% filter(trend != "Historic Data"),  # Exclude "Historic Data" from legend
        aes(
          x = measure_date,
          y = output_viral_conc,
          color = factor(trend),
          text = paste(
            "<b>Sample Date:</b> ",
            measure_date,
            "<br><b>Sample ID:</b>",
            sample_id,
            "<br><b>Lab Phase:</b> ",
            lab_phase,
            "<br><b>Viral Concentration:</b>",
            format(round(as.numeric(output_viral_conc), 0), nsmall = 0, big.mark = ","),
            "copies/L<br><b>Sample Type:</b> ",sample_type,
            "<br><b>Trend: </b>", trend,
            "<br><b>Slope: </b>", round(slope, 2)),
        ),
        size = 2.5
      ) +
        geom_point(
          data = viralconc_sars_input() %>% filter(trend == "Historic Data"),
          aes(
            x = measure_date,
            y = output_viral_conc,
            text = paste(
              "<b>Sample Date:</b> ",
              measure_date,
              "<br><b>Sample ID:</b>",
              sample_id,
              "<br><b>Lab Phase:</b> ",
              lab_phase,
              "<br><b>Viral Concentration:</b>",
              format(round(as.numeric(output_viral_conc), 0), nsmall = 0, big.mark = ","),
              "copies/L<br><b>Sample Type:</b> ",sample_type,
              "<br><b>Slope: </b>", round(slope, 2)),
          ),
          color = "gray",
          show.legend = FALSE  # <- Hides this layer from the legend
        ) +
        scale_color_manual(name = "", 
                           breaks = c(
                             'Increasing',
                             'Plateau', 
                             'Decreasing', 
                             'Insufficient Data',
                             'Historic Data'
                           ),
                           values = c(
                             'Increasing' = '#67001F', 
                             'Plateau' = '#FDDBC7', 
                             'Decreasing' = '#2166AC', 
                             'Insufficient Data' = '#000000',
                             'Historic Data' = "#BBBBBB"
                           ),
                           labels = c('Increasing',
                                      'Plateau', 
                                      'Decreasing', 
                                      'Insufficient Data',
                                      'Historic Data'
                           )
        )+
        guides(fill = "none", color = guide_none()) +  # Remove fill legend
        geom_vline(xintercept = as.numeric(as.Date("2024-09-30")), linetype = "dashed") +
        geom_text(
          data = data.frame(measure_date = as.Date("2024-09-30"), output_viral_conc = max_y * 1.5),
          aes(
            x = measure_date, 
            y = output_viral_conc, 
            label = "",
            text = "Lab Phase 3 Begins: September 30th, 2024"
          ),
          vjust = -1,
          hjust = 0.5,
          show.legend = FALSE,
          color = "black"
        ) +
        geom_vline(xintercept = as.numeric(as.Date("2023-07-13")), linetype = "dashed") +
        geom_text(
          data = data.frame(measure_date = as.Date("2023-07-13"), output_viral_conc = max_y * 1.5),
          aes(
            x = measure_date, 
            y = output_viral_conc, 
            label = "",
            text = "Lab Phase 2 Begins: July 13th, 2023"
          ),
          vjust = -1,
          hjust = 0.5,
          show.legend = FALSE,
          color = "black"
        ) +
        coord_cartesian(xlim = c(
          input$daterange1[1], input$daterange1[2]
        )) +
        scale_y_continuous(labels = comma) +
        ylim(0, max_y*1.5) +
        theme_gdocs() +
        theme(text = element_text(family = "Arial", size = 8)),
      tooltip = "text"
    ) %>%
      layout(
        title = list(
          text = paste(
            "Sars-Cov-2 Viral Concentration by Sampling Date for",
            input$utility
          )
        ),
        xaxis = list(title = "Sample Collection Date", fixedrange = TRUE),
        yaxis = list(title = "Viral Concentration (gene copies/Liter)", fixedrange = TRUE),
        guides(linetype = guide_legend(title = "Lab Phase"),
               color = guide_legend(title = "Trend"))
      ) %>%
      config(
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")
      ) 
  })
  
  
  #normalized viral concentration plot
  normviralconc_sars_input <- reactive({
    req(credentials()$user_auth)
    credentials()$info
    
    req(input$log_button_normconc_sars)
    
    if(input$log_button_normconc_sars == "Linear Scale"){
      flowNormData <- wwdatafull %>%
        filter(utility %in% input$utility) %>%
        filter(pcr_target == "sars-cov-2") %>%
        filter(!is.na(flow_rate))%>%
        filter(!is.na(viral_conc_raw))%>%
        filter(!is.na(population_served))%>%
        rename(output_viral_conc = viral_conc_fpnorm) 
      
      max_trenddate <- flowNormData %>%
        filter(!trend == "") %>%
        select(measure_date)
      
      max_trenddate <- max_trenddate[1,1]
      
      flowNormData <- flowNormData %>%
        mutate(trend = ifelse((trend == "" & measure_date < max_trenddate),"Historic Data", trend)) %>%
        mutate(trend = case_when(is.na(trend) ~ "Historic Data", TRUE ~ trend)) %>%
        mutate(trend = case_when(trend == "" ~ "Insufficient Data", TRUE ~ trend))
      
    }
    else if(input$log_button_normconc_sars == "Log Scale"){
      flowNormData <- wwdatafull %>%
        filter(utility %in% input$utility) %>%
        filter(pcr_target == "sars-cov-2") %>%
        filter(!is.na(flow_rate))%>%
        filter(!is.na(log_viral_conc_raw))%>%
        filter(!is.na(population_served))%>%
        rename(output_viral_conc = log_viral_conc_fpnorm) 
      
      max_trenddate <- flowNormData %>%
        filter(!trend == "") %>%
        select(measure_date)
      
      max_trenddate <- max_trenddate[1,1]
      
      flowNormData <- flowNormData %>%
        mutate(trend = ifelse((trend == "" & measure_date < max_trenddate),"Historic Data", trend)) %>%
        mutate(trend = case_when(is.na(trend) ~ "Historic Data", TRUE ~ trend)) %>%
        mutate(trend = case_when(trend == "" ~ "Insufficient Data", TRUE ~ trend))
      
    }
    
    flowNormData$trend <- factor(flowNormData$trend, levels = c("Increasing", "Plateau", "Decreasing", "Insufficient Data", "Historic Data"))
    
    return(flowNormData)
  })   
  
  
  output$flowNormGraph_sars <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    
    max_y <- normviralconc_sars_input() %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    max_y <- max(max_y$output_viral_conc, na.rm = TRUE)
    max_y <- abs(max_y)
    
    
    ggplotly(
      ggplot() +
        geom_line(
          data = normviralconc_sars_input(),
          aes(x = measure_date, y = output_viral_conc
          )
        ) +
      geom_point(
        data = normviralconc_sars_input() %>% filter(trend != "Historic Data"),  # Exclude "Historic Data" from legend
        aes(
          x = measure_date,
          y = output_viral_conc,
          color = factor(trend),
          text = paste(
            "<b>Sample Date:</b> ",
            measure_date,
            "<br><b>Sample ID:</b>",
            sample_id,
            "<br><b>Lab Phase:</b> ",
            lab_phase,
            "<br><b>Viral Concentration:</b>",
            format(round(as.numeric(output_viral_conc), 0), nsmall = 0, big.mark = ","),
            "copies/L<br><b>Sample Type:</b> ",sample_type,
            "<br><b>Trend: </b>", trend,
            "<br><b>Slope: </b>", round(slope, 2)),
        ),
        size = 2.5
      ) +
        geom_point(
          data = normviralconc_sars_input() %>% filter(trend == "Historic Data"),
          aes(
            x = measure_date,
            y = output_viral_conc,
            text = paste(
              "<b>Sample Date:</b> ",
              measure_date,
              "<br><b>Sample ID:</b>",
              sample_id,
              "<br><b>Lab Phase:</b> ",
              lab_phase,
              "<br><b>Viral Concentration:</b>",
              format(round(as.numeric(output_viral_conc), 0), nsmall = 0, big.mark = ","),
              "copies/L<br><b>Sample Type:</b> ",sample_type,
              "<br><b>Slope: </b>", round(slope, 2)),
          ),
          color = "gray",
          show.legend = FALSE  # <- Hides this layer from the legend
        ) +
        geom_vline(xintercept = as.numeric(as.Date("2024-09-30")), linetype = "dashed") +
        geom_text(
          data = data.frame(measure_date = as.Date("2024-09-30"), output_viral_conc = max_y * 1.5),
          aes(
            x = measure_date, 
            y = output_viral_conc, 
            label = "",
            text = "Lab Phase 3 Begins: September 30th, 2024"
          ),
          vjust = -1,
          hjust = 0.5,
          show.legend = FALSE,
          color = "black"
        ) +
        geom_vline(xintercept = as.numeric(as.Date("2023-07-13")), linetype = "dashed") +
        geom_text(
          data = data.frame(measure_date = as.Date("2023-07-13"), output_viral_conc = max_y * 1.5),
          aes(
            x = measure_date, 
            y = output_viral_conc, 
            label = "",
            text = "Lab Phase 2 Begins: July 13th, 2023"
          ),
          vjust = -1,
          hjust = 0.5,
          show.legend = FALSE,
          color = "black"
        ) +
        scale_color_manual(name = "",
                           breaks = c(
                             'Increasing',
                             'Plateau',
                             'Decreasing',
                             'Insufficient Data',
                             'Historic Data'
                           ),
                           values = c(
                             'Increasing' = '#67001F', 
                             'Plateau' = '#FDDBC7', 
                             'Decreasing' = '#2166AC', 
                             'Insufficient Data' = '#000000',
                             'Historic Data' = "#BBBBBB"
                           ),
                           labels = c('Increasing',
                                      'Plateau',
                                      'Decreasing',
                                      'Insufficient Data',
                                      'Historic Data'
                           )
        )+
        coord_cartesian(xlim = c(
          input$daterange1[1], input$daterange1[2]
        )) +
        scale_y_continuous(labels = comma) +
        ylim(0, max_y*2) +
        theme_gdocs() +
        theme(text = element_text(family = "Arial", size = 8)),
      tooltip = "text") %>%
      layout(
        title = list(
          text = paste(
            "Normalized SARS-CoV-2 Viral Concentration by Sampling Date for",
            input$utility
          )
        ),
        xaxis = list(title = "Sample Collection Date", fixedrange = TRUE),
        yaxis = list(title = "Viral Concentration (gene copies per person per day", fixedrange = TRUE)
      ) %>%
      config(
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")
      )
    
  })   
  
  
  #### Viral Concentration map above heatmap
  output$wwConcGraph_sars2 <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    
    input <- data.frame(utility = "Utility1", daterange1 = as.Date(c("2025-01-24", "2025-05-08")), rsv_outbreak_county = "Countty1")
    viralconc_sars_input <- wwdatafull %>%
      filter(utility %in% input$utility) %>%
      filter(pcr_target == "sars-cov-2") %>%
      filter(!is.na(viral_conc_raw)) %>%
      rename(output_viral_conc = viral_conc_raw) 
    
    #calcualte max y so y-axis auto adjusts based on data presented in selected date range
    max_y <- viralconc_sars_input %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    max_y <- max(max_y$output_viral_conc, na.rm = TRUE)
    max_y <- abs(max_y)
    
    lod_lp2 <- viralconc_sars_input %>%
      group_by(utility) %>%
      complete(measure_date = seq.Date(as.Date("2024-07-12"), to = as.Date("2024-09-29"), by="day")) %>%
      filter(measure_date < "2024-09-30") %>%
      mutate(lod_limit = 2000) 
    
    lod_lp3 <- viralconc_sars_input %>%
      group_by(utility) %>%
      complete(measure_date = seq.Date(as.Date("2024-09-30"), to = Sys.Date(), by="day")) %>%
      filter(measure_date >= "2024-09-30") %>%
      mutate(lod_limit = 1200)
    
    ggplotly(
      ggplot() +
        geom_tile(
          data = lod_lp2, 
          aes(x = measure_date, y = lod_limit/2,
              height = 2000,
              text = paste(
                "<b>Date:</b>",
                measure_date,
                "<br><b>Limit of Detection:</b> ",
                lod_limit, "copies/L")#,
          ),
          fill = "lightblue", color = "lightblue"
        ) + 
        geom_tile(
          data = lod_lp3, 
          aes(x = measure_date, y = lod_limit/2,
              height = 1200,
              text = paste(
                "<b>Date:</b>",
                measure_date,
                "<br><b>Limit of Detection:</b> ",
                lod_limit, "copies/L")
          ),
          fill = "lightblue", color = "lightblue"
        ) + 
        geom_line(
          data = viralconc_sars_input,
          aes(x = measure_date, y = output_viral_conc
          )
        ) +
        geom_point(
          data = viralconc_sars_input,
          aes(
            x = measure_date,
            y = output_viral_conc,
            text = paste(
              "<b>Sample Date:</b> ",
              measure_date,
              "<br><b>Sample ID:</b>",
              sample_id,
              "<br><b>Lab Phase:</b> ",
              lab_phase,
              "<br><b>Viral Concentration:</b>",
              format(round(as.numeric(output_viral_conc), 0), nsmall = 0, big.mark = ","),
              "copies/L<br><b>Sample Type:</b> ",sample_type,
              "<br><b>Trend: </b>", trend,
              "<br><b>Slope: </b>", round(slope, 2)),
          ),
          size = 2.5
        ) +
        geom_vline(xintercept = as.numeric(as.Date("2024-09-30")), linetype = "dashed") +
        geom_text(
          data = data.frame(measure_date = as.Date("2024-09-30"), output_viral_conc = max_y * 1.5),
          aes(
            x = measure_date, 
            y = output_viral_conc, 
            label = "",
            text = "Lab Phase 3 Begins: September 30th, 2024"
          ),
          vjust = -1,
          hjust = 0.5,
          show.legend = FALSE,
          color = "black"
        ) +
        geom_vline(xintercept = as.numeric(as.Date("2023-07-13")), linetype = "dashed") +
        geom_text(
          data = data.frame(measure_date = as.Date("2023-07-13"), output_viral_conc = max_y * 1.5),
          aes(
            x = measure_date, 
            y = output_viral_conc, 
            label = "",
            text = "Lab Phase 2 Begins: July 13th, 2023"
          ),
          vjust = -1,
          hjust = 0.5,
          show.legend = FALSE,
          color = "black"
        ) +
        coord_cartesian(xlim = c(
          input$daterange1[1], input$daterange1[2]
        )) +
        scale_y_continuous(labels = comma) +
        ylim(0, max_y*1.5) +
        theme_gdocs() +
        theme(text = element_text(family = "Arial", size = 8)),
      tooltip = "text"
    ) %>%
      layout(
        title = list(
          text = paste(
            "Sars-Cov-2 Viral Concentration & Sequencing Abundance for",
            input$utility
          )
        ),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(title = "Viral Concentration (gene copies/Liter)", fixedrange = TRUE)
      ) %>%
      config(
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")
      ) 
  })
  
  #Omicron Variant Data
  output$heatmap_sars <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    
    #create list of variant names
    variants <- wwdatafull %>%
      ungroup() %>%
      distinct(variant) %>%
      filter(!is.na(variant), !variant == "")
    
    VOC_Data <- wwdatafull %>% 
      ungroup() %>%
      filter(pcr_target == "sars-cov-2") %>%
      select(utility, measure_date, variant, abundance) %>%
      filter(utility %in% input$utility) %>%
      filter(!is.na(variant), !variant == "") %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2]) %>%
      mutate(measure_date = lubridate::as_date(measure_date, format = "%Y-%m-%d")) %>%
      mutate(abundance = round(abundance * 100,1)) %>%
      mutate(abundance = case_when(is.na(abundance) ~ 0, 
                                   TRUE ~ abundance)) %>%
      mutate(abundance = case_when(variant == "Insufficient Signal" & abundance == 0 ~ 100,
                                   TRUE ~ abundance))
    
    

    
    variant_names <- unique(variants$variant)
    
    color_palette <- viridis(length(variant_names), option = "D")  # Or "C", "B", "A", etc.
    color_palette[grepl("Insufficient Signal", variant_names)] <- "gray"
    
    colors_vector <- setNames(color_palette, variant_names)
    
    
    ggplotly(
      ggplot(VOC_Data, aes(x = measure_date, y = abundance, fill = variant,
                           text = paste0("<b>Date: </b>", measure_date,
                                         "<br><b>Variant: </b>",
                                         variant,
                                         "<br><b>Abundance: </b>", 
                                         format(round(as.numeric(abundance)), nsmall = 0, big.mark = ","), "%")
      )) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = colors_vector) +
        labs(
          x = "Sample Collection Date",
          y = "Abundance",
          fill = "Variant"
        ) + 
        guides(fill = guide_legend(title = NULL)) +
        coord_cartesian(xlim = c(
          input$daterange1[1], input$daterange1[2]
        )) +
        theme_gdocs() +
        theme(text = element_text(family = "Arial", size = 8)),
      tooltip = "text"
    ) %>%
      layout(
        xaxis = list(title = "Sample Collection Date", fixedrange = TRUE),
        yaxis = list(title = "Abundance (%)", fixedrange = TRUE),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2
        )
      ) %>%
      config(
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")
      )
    
  }) 
  
  
  #county level case data
  output$county_cases_sars <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    
    SARSCountyCaseGraphData <- sars_casedata %>%
      filter(County %in% input$sars_case_county) 
    
    #calcualte max y so y-axis auto adjusts based on data presented in selected date range
    max_y <- SARSCountyCaseGraphData %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    max_y <- max(max_y$County_cases_3dayavg_r100Kutil, na.rm = TRUE)
    max_y <- abs(max_y)
    
    ggplotly(
      ggplot() +
        geom_bar(
          data = SARSCountyCaseGraphData,
          aes(
            x = measure_date,
            y = County_cases_3dayavg_r100Kutil,
            text = paste0("<b>Date: </b>", measure_date,
                          "<br><b>Number of Cases: <b>", 
                          format(round(as.numeric(County_cases_3dayavg_r100Kutil),1), nsmall = 0, big.mark = ",")),
            size = 1),
          color = "gray20", 
          size = .5,
          stat = "identity",
          fill = "#EE8866") +
        coord_cartesian(xlim = c(
          input$daterange1[1], input$daterange1[2]
        )) +
        scale_y_continuous(labels = comma) +
        ylim(0, max_y*2) +
        theme_gdocs() +
        theme(text = element_text(family = "Arial", size = 8), 
              legend.position = "none"),
      tooltip = "text") %>%
      layout(
        title = list(
          text = paste("COVID-19 Case Rate in", input$sars_case_county, "County")
        ),
        xaxis = list(title = "COVID-19 Test Date", fixedrange = TRUE),
        yaxis = list(title = "COVID-19 Cases per 100,000 (3-day average)", fixedrange = TRUE)
      ) %>%
      config(
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"))
  })
  
  #county level hospitalizations
  output$hospGraph_sars <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    
    SARSCountyHospGraphData <- sars_hospdata %>%
      filter(County %in% input$sars_hosp_county) 
    
    max_y <- SARSCountyHospGraphData %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    max_y <- max(max_y$County_hosp_3dayavg_r100Kutil, na.rm = TRUE)
    max_y <- abs(max_y)
    
    ggplotly(
      ggplot() +
        geom_bar(
          data = SARSCountyHospGraphData,
          aes(
            x = measure_date,
            y = County_hosp_3dayavg_r100Kutil,
            text = paste0("<b>Date: </b>", measure_date,
                          "<br><b>Number of Cases: <b>",
                          format(round(as.numeric(County_hosp_3dayavg_r100Kutil),1), nsmall = 0, big.mark = ",")),
            size = 1),
          color = "gray20",
          size = .5,
          stat = "identity",
          fill = "#44BB99") +
        coord_cartesian(xlim = c(
          input$daterange1[1], input$daterange1[2]
        )) +
        scale_y_continuous(labels = comma) +
        ylim(0, max_y*2) +
        theme_gdocs() +
        theme(text = element_text(family = "Arial", size = 8),
              legend.position = "none"),
      tooltip = "text") %>%
      layout(
        title = list(
          text = paste("COVID-19 Hospitalization Rate in", input$sars_hosp_county, "County")
        ),
        xaxis = list(title = "Hospital Admission Date", fixedrange = TRUE),
        yaxis = list(title = "Hospitalization Rate per 100,000 (3-day average)", fixedrange = TRUE)
      ) %>%
      config(
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"))
    
  })
  
  
  
  
  #################################
  ##        RSV Data          ###
  #################################
  
  #set tab show/hide
  observe({
    req(credentials()$user_auth)
    credentials()$info
    
    req(input$utility)
    if (input$utility %in% Utilities_RSV) {
      showTab(inputId = "Pathogen_Tabs", target = "RSV_Tab")
    }
    else hideTab(inputId = "Pathogen_Tabs", target = "RSV_Tab")
  }) 
  
  #viral concentration
  viralconc_rsv_input <- reactive({
    req(credentials()$user_auth)
    credentials()$info
    
    #input <- data.frame(utility = "Metro Wastewater RWHTF - CC", daterange1 = as.Date(c("2024-11-01", "2025-12-31")), rsv_outbreak_county = "Adams")
    
    
    req(input$log_button_rawconc_rsv)
    
    if(input$log_button_rawconc_rsv == "Linear Scale"){
      VirusGraphData <- wwdatafull %>%
        filter(utility %in% input$utility) %>%
        filter(pcr_target %in% c("RSV_A", "RSV_B", "RSV A + B Combined")) %>%
        mutate(pcr_target = case_when(pcr_target == 'RSV_A' ~ 'RSV A', TRUE ~ pcr_target)) %>%
        mutate(pcr_target = case_when(pcr_target == 'RSV_B' ~ 'RSV B', TRUE ~ pcr_target)) %>%
        filter(!is.na(viral_conc_raw)) %>%
        group_by(pcr_target) %>%
        rename(output_viral_conc = viral_conc_raw) 
      
      max_trenddate <- VirusGraphData %>%
        group_by(pcr_target) %>%
        filter(!detection_status == "") %>%
        select(pcr_target, measure_date) %>%
        arrange(desc(measure_date)) %>%
        rename(max_trenddate = measure_date) %>%
        slice_head(n=1)
      
      VirusGraphData <- VirusGraphData %>%
        left_join(max_trenddate, by = c("pcr_target")) %>%
        mutate(detection_status = case_when(is.na(detection_status) ~ "Historic Data", TRUE ~ detection_status)) %>%
        mutate(detection_status = ifelse(measure_date < max_trenddate,"Historic Data", detection_status)) %>%
        ungroup()
      
      
    }
    else if(input$log_button_rawconc_rsv == "Log Scale"){
      VirusGraphData <- wwdatafull %>%
        filter(utility %in% input$utility) %>%
        filter(pcr_target %in% c("RSV_A", "RSV_B", "RSV A + B Combined")) %>%
        mutate(pcr_target = case_when(pcr_target == 'RSV_A' ~ 'RSV A', TRUE ~ pcr_target)) %>%
        mutate(pcr_target = case_when(pcr_target == 'RSV_B' ~ 'RSV B', TRUE ~ pcr_target)) %>%
        filter(!is.na(log_viral_conc_raw)) %>%
        group_by(pcr_target) %>%
        rename(output_viral_conc = log_viral_conc_raw) 
      
      max_trenddate <- VirusGraphData %>%
        group_by(pcr_target) %>%
        filter(!detection_status == "") %>%
        select(pcr_target, measure_date) %>%
        arrange(desc(measure_date)) %>%
        rename(max_trenddate = measure_date) %>%
        slice_head(n=1)
      
      VirusGraphData <- VirusGraphData %>%
        left_join(max_trenddate, by = c("pcr_target")) %>%
        mutate(detection_status = case_when(is.na(detection_status) ~ "Historic Data", TRUE ~ detection_status)) %>%
        mutate(detection_status = ifelse(measure_date < max_trenddate,"Historic Data", detection_status)) %>%
        ungroup()
    }
    
    VirusGraphData$detection_status <- factor(VirusGraphData$detection_status, levels = c("Persistent detection", "Detection", "No recent detection", "No recent data", "Historic Data"))
    
    return(VirusGraphData)
  })
  
  
  
  output$wwConcGraph_rsv <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    
    max_y <- viralconc_rsv_input() %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    max_y <- max(max_y$output_viral_conc, na.rm = TRUE)
    max_y <- abs(max_y)
    
    lod_lp2 <- viralconc_rsv_input() %>%
      group_by(utility) %>%
      complete(measure_date = seq.Date(as.Date("2024-07-12"), to = as.Date("2024-09-29"), by="day")) %>%
      filter(measure_date < "2024-09-30") %>%
      mutate(lod_limit = 2000) 
    
    lod_lp3 <- viralconc_rsv_input() %>%
      group_by(utility) %>%
      complete(measure_date = seq.Date(as.Date("2024-09-30"), to = Sys.Date(), by="day")) %>%
      filter(measure_date >= "2024-09-30") %>%
      mutate(lod_limit = 1200)
    
    ggplotly(
      ggplot() +
        geom_tile(
          data = lod_lp2, 
          aes(x = measure_date, y = lod_limit/2,
              height = 2000,
              text = paste(
                "<b>Date:</b>",
                measure_date,
                "<br><b>Limit of Detection:</b> ",
                lod_limit, "copies/L")
          ),
          fill = "lightblue", color = "lightblue"
        ) + 
        geom_tile(
          data = lod_lp3, 
          aes(x = measure_date, y = lod_limit/2,
              height = 1200,
              text = paste(
                "<b>Date:</b>",
                measure_date,
                "<br><b>Limit of Detection:</b> ",
                lod_limit, "copies/L")#,
          ),
          fill = "lightblue", color = "lightblue"
        ) +
      geom_point(
        data = viralconc_rsv_input() %>% filter(detection_status != "Historic Data"),  # Exclude "Historic Data" from legend
        aes(
          x = measure_date,
          y = output_viral_conc,
          color = factor(detection_status),
          text = paste(
            "<b>Sample Date:</b> ", measure_date,
            "<br><b>Sample ID:</b>", sample_id,
            "<br><b>Lab Phase:</b> ", lab_phase,
            "<br><b>Viral Concentration:</b>", format(round(as.numeric(output_viral_conc), 0), nsmall = 0, big.mark = ","), "copies/L",
            "<br><b>Detection Status:</b> ", detection_status,
            "<br><b>Sample Type:</b> ", sample_type
          )
        ),
        size = 2.5
      ) +
        geom_point(
          data = viralconc_rsv_input() %>% filter(detection_status == "Historic Data"),
          aes(
            x = measure_date,
            y = output_viral_conc,
            text = paste(
              "<b>Sample Date:</b> ", measure_date,
              "<br><b>Sample ID:</b>", sample_id,
              "<br><b>Lab Phase:</b> ", lab_phase,
              "<br><b>Viral Concentration:</b>", format(round(as.numeric(output_viral_conc), 0), nsmall = 0, big.mark = ","), "copies/L",
              "<br><b>Sample Type:</b> ", sample_type
            )),
          color = "gray",
          show.legend = FALSE  # <- Hides this layer from the legend
        ) +
        geom_line(
          data = viralconc_rsv_input(),
          aes(x = measure_date, y = output_viral_conc, color = factor(pcr_target))
        ) +
        geom_vline(xintercept = as.numeric(as.Date("2024-09-30")), linetype = "dashed") +
        geom_text(
          data = data.frame(measure_date = as.Date("2024-09-30"), output_viral_conc = max_y * 1.5),
          aes(
            x = measure_date, 
            y = output_viral_conc, 
            label = "",
            text = "Lab Phase 3 Begins: September 30th, 2024"
          ),
          vjust = -1,
          hjust = 0.5,
          show.legend = FALSE,
          color = "black"
        ) +
        scale_color_manual(name = "Detection Status", 
                           breaks = c(
                             'Persistent detection',
                             'Detection', 
                             'No recent detection', 
                             'No recent data',
                             # 'Historic Data',
                             'RSV A', 'RSV B', 'RSV A + B Combined'
                           ),
                           values = c(
                             'Persistent detection' = '#67001F', 
                             'Detection' = '#FDDBC7',
                             'No recent detection'= '#2166AC',
                             'No recent data' = '#BBBBBB', 
                             # 'Historic Data' = "#BBBBBB",
                             'RSV A'='gray50', 
                             'RSV B'='gray',
                             'RSV A + B Combined'= 'black'
                           ),
                           labels = c('Persistent detection',
                                      'Detection', 
                                      'No recent detection', 
                                      'No recent data',
                                      # 'Historic Data',
                                      'RSV A', 'RSV B', 'RSV A + B Combined'
                           )
        )+
        coord_cartesian(xlim = c(
          input$daterange1[1], input$daterange1[2]
        )) +
        scale_y_continuous(labels = comma) +
        ylim(0, max_y*2) +
        theme_gdocs() +
        theme(text = element_text(family = "Arial", size = 8)) +
        guides(
          color = guide_legend(title = "Detection Status", order = 1),  # Legend for detection_status
          linetype = guide_legend(title = "PCR Target", order = 2)      # Legend for pcr_target
        ),
      tooltip = "text"
    ) %>%
      layout(
        title = list(
          text = paste(
            "RSV Viral Concentration by Sampling Date for",
            input$utility
          )
        ),
        xaxis = list(title = "Sample Collection Date", fixedrange = TRUE),
        yaxis = list(title = "Viral Concentration (gene copies/Liter)", fixedrange = TRUE)
      ) %>%
      config(
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")
      )
  })
  
  
  #normalized viral conc plot
  normviralconc_rsv_input <- reactive({
    req(credentials()$user_auth)
    credentials()$info
    
    #input <- data.frame(utility = "Metro Wastewater RWHTF - CC", daterange1 = as.Date(c("2023-11-01", "2023-12-31")), rsv_outbreak_county = "Adams")
    
    
    req(input$log_button_normconc_rsv)
    if(input$log_button_normconc_rsv == "Linear Scale"){
      flowNormData <- wwdatafull %>%
        filter(utility %in% input$utility) %>%
        filter(pcr_target  %in% c("RSV_A", "RSV_B", "RSV A + B Combined")) %>%
        mutate(pcr_target = case_when(pcr_target == 'RSV_A' ~ 'RSV A', TRUE ~ pcr_target)) %>%
        mutate(pcr_target = case_when(pcr_target == 'RSV_B' ~ 'RSV B', TRUE ~ pcr_target)) %>%
        filter(!is.na(flow_rate))%>%
        filter(!is.na(viral_conc_raw))%>%
        filter(!is.na(population_served))%>%
        rename(output_viral_conc = viral_conc_fpnorm)
      
      max_trenddate <- flowNormData %>%
        group_by(pcr_target) %>%
        filter(!detection_status == "") %>%
        select(pcr_target, measure_date) %>%
        arrange(desc(measure_date)) %>%
        rename(max_trenddate = measure_date) %>%
        slice_head(n=1)
      
      flowNormData <- flowNormData %>%
        left_join(max_trenddate, by = c("pcr_target")) %>%
        mutate(detection_status = case_when(is.na(detection_status) ~ "Historic Data", TRUE ~ detection_status)) %>%
        mutate(detection_status = ifelse(measure_date < max_trenddate,"Historic Data", detection_status)) %>%
        ungroup()
      
      
    }
    else if(input$log_button_normconc_rsv == "Log Scale"){
      flowNormData <- wwdatafull %>%
        filter(utility %in% input$utility) %>%
        filter(pcr_target  %in% c("RSV_A", "RSV_B", "RSV A + B Combined")) %>%
        mutate(pcr_target = case_when(pcr_target == 'RSV_A' ~ 'RSV A', TRUE ~ pcr_target)) %>%
        mutate(pcr_target = case_when(pcr_target == 'RSV_B' ~ 'RSV B', TRUE ~ pcr_target)) %>%
        filter(!is.na(flow_rate))%>%
        filter(!is.na(log_viral_conc_raw))%>%
        filter(!is.na(population_served))%>%
        rename(output_viral_conc = log_viral_conc_fpnorm)
      
      
      max_trenddate <- flowNormData %>%
        group_by(pcr_target) %>%
        filter(!detection_status == "") %>%
        select(pcr_target, measure_date) %>%
        arrange(desc(measure_date)) %>%
        rename(max_trenddate = measure_date) %>%
        slice_head(n=1)
      
      flowNormData <- flowNormData %>%
        left_join(max_trenddate, by = c("pcr_target")) %>%
        mutate(detection_status = case_when(is.na(detection_status) ~ "Historic Data", TRUE ~ detection_status)) %>%
        mutate(detection_status = ifelse(measure_date < max_trenddate,"Historic Data", detection_status)) %>%
        ungroup()
      
    }
    
    flowNormData$detection_status <- factor(flowNormData$detection_status, levels = c("Persistent detection", "Detection", "No recent detection", "No recent data", "Historic Data"))
    
    return(flowNormData)
  })   
  
  output$flowNormGraph_rsv <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    
    max_y <- normviralconc_rsv_input() %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    max_y <- max(max_y$output_viral_conc, na.rm = TRUE)
    max_y <- abs(max_y)
    
    ggplotly(
      ggplot() +
      geom_point(
        data = normviralconc_rsv_input() %>% filter(detection_status != "Historic Data"),  # Exclude "Historic Data" from legend
        aes(
          x = measure_date,
          y = output_viral_conc,
          color = factor(detection_status),
          text = paste(
            "<b>Sample Date:</b> ", measure_date,
            "<br><b>Sample ID:</b>", sample_id,
            "<br><b>Lab Phase:</b> ", lab_phase,
            "<br><b>Viral Concentration:</b>", format(round(as.numeric(output_viral_conc), 0), nsmall = 0, big.mark = ","), "copies/L",
            "<br><b>Detection Status:</b> ", detection_status,
            "<br><b>Sample Type:</b> ", sample_type
          )
        ),
        size = 2.5
      ) +
        geom_point(
          data = normviralconc_rsv_input() %>% filter(detection_status == "Historic Data"),
          aes(
            x = measure_date,
            y = output_viral_conc,
            text = paste(
              "<b>Sample Date:</b> ", measure_date,
              "<br><b>Sample ID:</b>", sample_id,
              "<br><b>Lab Phase:</b> ", lab_phase,
              "<br><b>Viral Concentration:</b>", format(round(as.numeric(output_viral_conc), 0), nsmall = 0, big.mark = ","), "copies/L",
              "<br><b>Sample Type:</b> ", sample_type
            )),
          color = "gray",
          show.legend = FALSE  # <- Hides this layer from the legend
        ) +
        geom_line(
          data = normviralconc_rsv_input(),
          aes(x = measure_date, y = output_viral_conc, color = factor(pcr_target))
        ) +
        geom_vline(xintercept = as.numeric(as.Date("2024-09-30")), linetype = "dashed") +
        geom_text(
          data = data.frame(measure_date = as.Date("2024-09-30"), output_viral_conc = max_y * 1.5),
          aes(
            x = measure_date, 
            y = output_viral_conc, 
            label = "",
            text = "Lab Phase 3 Begins: September 30th, 2024"
          ),
          vjust = -1,
          hjust = 0.5,
          show.legend = FALSE,
          color = "black"
        ) +
      scale_color_manual(name = "Detection Status", 
                         breaks = c(
                           'Persistent detection',
                           'Detection', 
                           'No recent detection', 
                           'No recent data',
                           'Historic Data',
                           'Insufficient Data',
                           'RSV A', 'RSV B', 'RSV A + B Combined'
                         ),
                         values = c(
                           'Persistent detection' = '#67001F', 
                           'Detection' = '#FDDBC7',
                           'No recent detection'= '#2166AC',
                           'No recent data' = '#BBBBBB', 
                           # 'Historic Data' = "#BBBBBB",
                           # 'Insufficient Data' = "black",
                           'RSV A'='gray50', 
                           'RSV B'='gray',
                           'RSV A + B Combined'= 'black'
                         ),
                         labels = c('Persistent detection',
                                    'Detection', 
                                    'No recent detection', 
                                    'No recent data',
                                    'Historic Data',
                                    'Insufficient Data',
                                    'RSV A', 'RSV B', 'RSV A + B Combined'
                         )
      )+
        coord_cartesian(xlim = c(
          input$daterange1[1], input$daterange1[2]
        )) +
        scale_y_continuous(labels = comma) +
        ylim(0, max_y*2) +
        theme_gdocs() +
        theme(text = element_text(family = "Arial", size = 8)),
      tooltip = "text") %>%
      layout(
        title = list(
          text = paste(
            "Normalized RSV Viral Concentration by Sampling Date for",
            input$utility
          )
        ),
        xaxis = list(title = "Sample Collection Date", fixedrange = TRUE),
        yaxis = list(title = "Viral Concentration (gene copies per person per day", fixedrange = TRUE)
      ) %>%
      config(
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")
      )
    
  })
  
  #county level case data
  output$county_cases_rsv <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    

    rsvCountyCaseGraphData <- rsv_casedata %>%
      filter(County %in% input$rsv_case_county) %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    rsvCountyCaseGraphData_diagnosed <- rsvCountyCaseGraphData %>%
      filter(metric == "% RSV ED Visits")
    
    rsvCountyCaseGraphData_Hosp <- rsv_hosp %>%
      filter(County %in% input$rsv_case_county) %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    

    #calcualte max y so y-axis auto adjusts based on data presented in selected date range
    max_y <- rsvCountyCaseGraphData %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    max_y <- max(max_y$CountyCases, na.rm = TRUE)
    max_y <- abs(max_y)
    

    # Calculate max y so y-axis auto adjusts based on data presented in selected date range
    max_y_cases <- rsvCountyCaseGraphData %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2]) %>%
      summarize(max_cases = max(CountyCases, na.rm = TRUE)) %>%
      pull(max_cases)
    
    max_y_hosp <- rsvCountyCaseGraphData_Hosp %>%
      summarize(max_hospitalizations = max(Hospitalized_CaseCount_r100Kutil, na.rm = TRUE)) %>%
      pull(max_hospitalizations)
    
 
    #try hospitalization on second axis 
    rsv_case_plot <- plot_ly() %>%
      add_trace(
        data = rsvCountyCaseGraphData,
        x = ~measure_date,
        y = ~CountyCases,
        type = "scatter",
        mode = "lines+markers",
        color = ~metric,  # Color points based on the "metric" column
        colors = c("#332288", "#33bbee"),  # Color points based on the "metric" column
        legendgroup = ~metric,  # Group traces by "metric" in the legend
        name = ~metric,
        text = ~paste("<b>Metric:</b> ", metric, 
                      "<br><b>Date:</b> ", measure_date, 
                      "<br><b>Percent of Cases: </b>", 
                      paste0(format(round(as.numeric(CountyCases),1), nsmall = 0, big.mark = ",")), "%"),
        hoverinfo = "text"  # Set hoverinfo to "text" to display only the custom text
      ) %>%
      add_trace(
        data = rsvCountyCaseGraphData_Hosp,
        x = ~measure_date,
        y = ~Hospitalized_CaseCount_r100Kutil,
        yaxis = "y2",
        name = "RSV Hospitalized Admission Rate",
        type = "scatter",
        mode = "lines+markers",
        color = "#332288",
        text = ~paste("<b>Metric:</b> RSV Hospitalized Admission Rate", 
                      "<br><b>Date:</b> ", measure_date, 
                      "<br><b>Number of Admissions: </b>", 
                      format(round(as.numeric(Hospitalized_CaseCount_r100Kutil),1), nsmall = 0, big.mark = ",")),
        hoverinfo = "text"  # Set hoverinfo to "text" to display only the custom text
      ) %>%
    add_trace(
      data = rsvCountyCaseGraphData_diagnosed,
      x = ~measure_date,
      y = ~CountyCases,  # Replace YourMetricVariable with the actual variable you want for the area plot
      type = "scatter",
      mode = "lines",
      fill = "tozeroy",  # Specify "tozeroy" for area plot
      color = ~metric,  # Color points based on the "metric" column
      colors = c("#332288", "#33bbee"),  # Color points based on the "metric" column        
      line = list(color = "rgba(255,255,255,0)"),  # Set line color to transparent for area plot
      showlegend = FALSE,  # Do not show legend for the area plot
      text = ~paste("<b>Metric:</b> ", metric, 
                    "<br><b>Date:</b> ", measure_date, 
                    "<br><b>Percent of Cases: </b>", 
                    paste0(format(round(as.numeric(CountyCases),1), nsmall = 0, big.mark = ",")), "%"),
      hoverinfo = "text"  # Skip hoverinfo for the area plot
    ) %>%
      layout(
        title = list(
          text = paste(
            "RSV Clinical Measures for",
            input$rsv_case_county,
            "County"
          ),
          font = list(size = 14, family = "Arial")
        ),
        xaxis = list(title = "Test Date", fixedrange = TRUE, tickfont = list(size = 10,  family = "Arial")),
        yaxis = list(side = 'left', title = 'Percent of Total ED Visits', tickfont = list(size = 10,  family = "Arial"),
                     showgrid = FALSE, zeroline = FALSE, range=list(0, max(rsvCountyCaseGraphData$CountyCases)*1.5)
        ),
        yaxis2 = list(side = 'right', overlaying = 'y', title = 'RSV-Associated Hospitalizations per 100,000',
                      tickfont = list(size = 10,  family = "Arial"),
                      range=list(0, max(rsvCountyCaseGraphData_Hosp$Hospitalized_CaseCount_r100Kutil)*1.5)),
        legend = list(orientation = "h",
                      y = -.2  # Adjust this value to move the legend lower
        ),
        margin = list(r = 50),# Increase the right margin value to create more space
        bargap = 0.8  # Adjust this value to set the thickness of the bars
        
      )
    ggplotly(rsv_case_plot)
  })
  
 
  
  #################################
  ##        Influenza Data          ###
  #################################
  
  #set tab show/hide
  observe({
    req(credentials()$user_auth)
    credentials()$info
    
    req(input$utility)
    if (input$utility %in% Utilities_Flu) {
      showTab(inputId = "Pathogen_Tabs", target = "Flu_Tab")
    }
    else hideTab(inputId = "Pathogen_Tabs", target = "Flu_Tab")
  }) 
  
  #viral concentration
  viralconc_flu_input <- reactive({
    req(credentials()$user_auth)
    credentials()$info
    
        #input <- data.frame(utility = "Alamosa", daterange1 = as.Date(c("2024-07-11", "2024-09-30")))

    req(input$log_button_rawconc_flu)
    if(input$log_button_rawconc_flu == "Linear Scale"){
      VirusGraphData <- wwdatafull %>%
        filter(utility %in% input$utility) %>%
        filter(pcr_target %in% c("FLUAV", "FLUBV")) %>%
        mutate(pcr_target = case_when(pcr_target == 'FLUAV' ~ 'Influenza A', TRUE ~ pcr_target)) %>%
        mutate(pcr_target = case_when(pcr_target == 'FLUBV' ~ 'Influenza B', TRUE ~ pcr_target)) %>%
        filter(!is.na(viral_conc_raw)) %>%
        group_by(pcr_target) %>%
        rename(output_viral_conc = viral_conc_raw) 
      
      max_trenddate <- VirusGraphData %>%
        group_by(pcr_target) %>%
        filter(!detection_status == "") %>%
        select(pcr_target, measure_date) %>%
        arrange(desc(measure_date)) %>%
        rename(max_trenddate = measure_date) %>%
        slice_head(n=1)
      
      VirusGraphData <- VirusGraphData %>%
        left_join(max_trenddate, by = c("pcr_target")) %>%
        mutate(detection_status = case_when(is.na(detection_status) ~ "Historic Data", TRUE ~ detection_status)) %>%
        mutate(detection_status = ifelse(measure_date < max_trenddate,"Historic Data", detection_status)) %>%
        ungroup()
      
    }
    else if(input$log_button_rawconc_flu == "Log Scale"){
      VirusGraphData <- wwdatafull %>%
        filter(utility %in% input$utility) %>%
        filter(pcr_target %in% c("FLUAV", "FLUBV")) %>%
        mutate(pcr_target = case_when(pcr_target == 'FLUAV' ~ 'Influenza A', TRUE ~ pcr_target)) %>%
        mutate(pcr_target = case_when(pcr_target == 'FLUBV' ~ 'Influenza B', TRUE ~ pcr_target)) %>%
        filter(!is.na(log_viral_conc_raw)) %>%
        group_by(pcr_target) %>%
        rename(output_viral_conc = log_viral_conc_raw) 
      
      max_trenddate <- VirusGraphData %>%
        group_by(pcr_target) %>%
        filter(!detection_status == "") %>%
        select(pcr_target, measure_date) %>%
        arrange(desc(measure_date)) %>%
        rename(max_trenddate = measure_date) %>%
        slice_head(n=1)
      
      VirusGraphData <- VirusGraphData %>%
        left_join(max_trenddate, by = c("pcr_target")) %>%
        mutate(detection_status = case_when(is.na(detection_status) ~ "Historic Data", TRUE ~ detection_status)) %>%
        mutate(detection_status = ifelse(measure_date < max_trenddate,"Historic Data", detection_status)) %>%
        ungroup()
      
    }
    VirusGraphData$detection_status <- factor(VirusGraphData$detection_status, levels = c("Persistent detection", "Detection", "No recent detection", "No recent data", "Historic Data"))
    
    return(VirusGraphData)
  })
  
  
  
  output$wwConcGraph_flu <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    
    max_y <- viralconc_flu_input() %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    max_y <- max(max_y$output_viral_conc, na.rm = TRUE)
    max_y <- abs(max_y)
    
    lod_lp2 <- viralconc_flu_input() %>%
      group_by(utility) %>%
      complete(measure_date = seq.Date(as.Date("2024-07-12"), to = as.Date("2024-09-29"), by="day")) %>%
      filter(measure_date < "2024-09-30") %>%
      mutate(lod_limit = 2000)
    
    lod_lp3 <- viralconc_flu_input() %>%
      group_by(utility) %>%
      complete(measure_date = seq.Date(as.Date("2024-09-30"), to = Sys.Date(), by="day")) %>%
      filter(measure_date >= "2024-09-30") %>%
      mutate(lod_limit = 1200)
    
    ggplotly(
      ggplot() +
        geom_tile(
          data = lod_lp2,
          aes(x = measure_date, y = lod_limit/2,
              height = 2000,
              text = paste(
                "<b>Date:</b>",
                measure_date,
                "<br><b>Limit of Detection:</b> ",
                lod_limit, "copies/L")#,
          ),
          fill = "lightblue", color = "lightblue"
        ) +
        geom_tile(
          data = lod_lp3,
          aes(x = measure_date, y = lod_limit/2,
              height = 1200,
              text = paste(
                "<b>Date:</b>",
                measure_date,
                "<br><b>Limit of Detection:</b> ",
                lod_limit, "copies/L")#,
          ),
          fill = "lightblue", color = "lightblue"
        ) +
      geom_point(
        data = viralconc_flu_input() %>% filter(detection_status != "Historic Data"),  # Exclude "Historic Data" from legend
        aes(
          x = measure_date,
          y = output_viral_conc,
          color = factor(detection_status),
          text = paste(
            "<b>Sample Date:</b> ", measure_date,
            "<br><b>Sample ID:</b>", sample_id,
            "<br><b>Lab Phase:</b> ", lab_phase,
            "<br><b>Viral Concentration:</b>", format(round(as.numeric(output_viral_conc), 0), nsmall = 0, big.mark = ","), "copies/L",
            "<br><b>Detection Status:</b> ", detection_status,
            "<br><b>Sample Type:</b> ", sample_type
          )
        ),
        size = 2.5
      ) +
        geom_point(
          data = viralconc_flu_input() %>% filter(detection_status == "Historic Data"),
          aes(
            x = measure_date,
            y = output_viral_conc,
            text = paste(
              "<b>Sample Date:</b> ", measure_date,
              "<br><b>Sample ID:</b>", sample_id,
              "<br><b>Lab Phase:</b> ", lab_phase,
              "<br><b>Viral Concentration:</b>", format(round(as.numeric(output_viral_conc), 0), nsmall = 0, big.mark = ","), "copies/L",
              "<br><b>Sample Type:</b> ", sample_type
            )),
          color = "gray",
          show.legend = FALSE  # <- Hides this layer from the legend
        ) +
        
        geom_line(
          data = viralconc_flu_input(),
          aes(x = measure_date, y = output_viral_conc, color = factor(pcr_target))
        ) +
        geom_vline(xintercept = as.numeric(as.Date("2024-09-30")), linetype = "dashed") +
        geom_text(
          data = data.frame(measure_date = as.Date("2024-09-30"), output_viral_conc = max_y * 1.5),
          aes(
            x = measure_date,
            y = output_viral_conc,
            label = "",
            text = "Lab Phase 3 Begins: September 30th, 2024"
          ),
          vjust = -1,
          hjust = 0.5,
          show.legend = FALSE,
          color = "black"
        ) +
        scale_color_manual(name = "Detection Status",
                           breaks = c(
                             'Persistent detection',
                             'Detection',
                             'No recent detection',
                             'No recent data',
                             # 'Historic Data',
                             'Influenza A',
                             'Influenza B'
                           ),
                           values = c(
                             'Persistent detection' = '#67001F',
                             'Detection' = '#FDDBC7',
                             'No recent detection'= '#2166AC',
                             'No recent data' = '#BBBBBB',
                             'Influenza A'='black',
                             'Influenza B'='gray50'
                           ),
                           labels = c('Persistent detection',
                                      'Detection',
                                      'No recent detection',
                                      'No recent data',
                                      'Influenza A',
                                      'Influenza B'
                           )
        )+
        coord_cartesian(xlim = c(
          input$daterange1[1], input$daterange1[2]
        )) +
        scale_y_continuous(labels = comma) +
        ylim(0, max_y*2) +
        theme_gdocs() +
        theme(text = element_text(family = "Arial", size = 8)),
      tooltip = "text"
    ) %>%
      layout(
        title = list(
          text = paste(
            "Influenza Viral Concentration by Sampling Date for",
            input$utility
          )
        ),
        xaxis = list(title = "Sample Collection Date", fixedrange = TRUE),
        yaxis = list(title = "Viral Concentration (gene copies/Liter)", fixedrange = TRUE)
      ) %>%
      config(
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")
      )
  })
  
  
  
  
  
  #normalized viral conc plot
  normviralconc_flu_input <- reactive({
    req(credentials()$user_auth)
    credentials()$info
    
    req(input$log_button_normconc_flu)
    
    if(input$log_button_normconc_flu == "Linear Scale"){
      flowNormData <- wwdatafull %>%
        filter(utility %in% input$utility) %>%
        filter(pcr_target  %in% c("FLUAV", "FLUBV")) %>%
        mutate(pcr_target = case_when(pcr_target == 'FLUAV' ~ 'Influenza A', TRUE ~ pcr_target)) %>%
        mutate(pcr_target = case_when(pcr_target == 'FLUBV' ~ 'Influenza B', TRUE ~ pcr_target)) %>%
        filter(!is.na(flow_rate))%>%
        filter(!is.na(viral_conc_raw))%>%
        filter(!is.na(population_served))%>%
        rename(output_viral_conc = viral_conc_fpnorm)
      
      max_trenddate <- flowNormData %>%
        group_by(pcr_target) %>%
        filter(!detection_status == "") %>%
        select(pcr_target, measure_date) %>%
        arrange(desc(measure_date)) %>%
        rename(max_trenddate = measure_date) %>%
        slice_head(n=1)
      
      flowNormData <- flowNormData %>%
        left_join(max_trenddate, by = c("pcr_target")) %>%
        mutate(detection_status = case_when(is.na(detection_status) ~ "Historic Data", TRUE ~ detection_status)) %>%
        mutate(detection_status = ifelse(measure_date < max_trenddate,"Historic Data", detection_status)) %>%
        ungroup()
      
    }
    else if(input$log_button_normconc_flu == "Log Scale"){
      flowNormData <- wwdatafull %>%
        filter(utility %in% input$utility) %>%
        filter(pcr_target  %in% c("FLUAV", "FLUBV")) %>%
        mutate(pcr_target = case_when(pcr_target == 'FLUAV' ~ 'Influenza A', TRUE ~ pcr_target)) %>%
        mutate(pcr_target = case_when(pcr_target == 'FLUBV' ~ 'Influenza B', TRUE ~ pcr_target)) %>%
        filter(!is.na(flow_rate))%>%
        filter(!is.na(log_viral_conc_raw))%>%
        filter(!is.na(population_served))%>%
        rename(output_viral_conc = log_viral_conc_fpnorm)
      
      max_trenddate <- flowNormData %>%
        group_by(pcr_target) %>%
        filter(!detection_status == "") %>%
        select(pcr_target, measure_date) %>%
        arrange(desc(measure_date)) %>%
        rename(max_trenddate = measure_date) %>%
        slice_head(n=1)
      
      flowNormData <- flowNormData %>%
        left_join(max_trenddate, by = c("pcr_target")) %>%
        mutate(detection_status = case_when(is.na(detection_status) ~ "Historic Data", TRUE ~ detection_status)) %>%
        mutate(detection_status = ifelse(measure_date < max_trenddate,"Historic Data", detection_status)) %>%
        ungroup()
      
    }
    flowNormData$detection_status <- factor(flowNormData$detection_status, levels = c("Persistent detection", "Detection", "No recent detection", "No recent data", "Historic Data"))
    
    return(flowNormData)
  })   
  
  output$flowNormGraph_flu <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    
    max_y <- normviralconc_flu_input() %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    max_y <- max(max_y$output_viral_conc, na.rm = TRUE)
    max_y <- abs(max_y)
    
    
    ggplotly(
      ggplot() +
      geom_point(
        data = normviralconc_flu_input() %>% filter(detection_status != "Historic Data"),  # Exclude "Historic Data" from legend
        aes(
          x = measure_date,
          y = output_viral_conc,
          color = factor(detection_status),
          text = paste(
            "<b>Sample Date:</b> ", measure_date,
            "<br><b>Sample ID:</b>", sample_id,
            "<br><b>Lab Phase:</b> ", lab_phase,
            "<br><b>Viral Concentration:</b>", format(round(as.numeric(output_viral_conc), 0), nsmall = 0, big.mark = ","), "copies/L",
            "<br><b>Detection Status:</b> ", detection_status,
            "<br><b>Sample Type:</b> ", sample_type
          )
        ),
        size = 2.5
      ) +
        geom_point(
          data = normviralconc_flu_input() %>% filter(detection_status == "Historic Data"),
          aes(
            x = measure_date,
            y = output_viral_conc,
            text = paste(
              "<b>Sample Date:</b> ", measure_date,
              "<br><b>Sample ID:</b>", sample_id,
              "<br><b>Lab Phase:</b> ", lab_phase,
              "<br><b>Viral Concentration:</b>", format(round(as.numeric(output_viral_conc), 0), nsmall = 0, big.mark = ","), "copies/L",
              "<br><b>Sample Type:</b> ", sample_type
            )),
          color = "gray",
          show.legend = FALSE  # <- Hides this layer from the legend
        ) +
        geom_line(
          data = normviralconc_flu_input(),
          aes(x = measure_date, y = output_viral_conc, color = factor(pcr_target))
        ) +
        geom_vline(xintercept = as.numeric(as.Date("2024-09-30")), linetype = "dashed") +
        geom_text(
          data = data.frame(measure_date = as.Date("2024-09-30"), output_viral_conc = max_y * 1.5),
          aes(
            x = measure_date, 
            y = output_viral_conc, 
            label = "",
            text = "Lab Phase 3 Begins: September 30th, 2024"
          ),
          vjust = -1,
          hjust = 0.5,
          show.legend = FALSE,
          color = "black"
        ) +
      scale_color_manual(name = "Detection Status", 
                         breaks = c(
                           'Persistent detection',
                           'Detection', 
                           'No recent detection', 
                           'No recent data',
                           # 'Historic Data',
                           'Influenza A', 
                           'Influenza B'
                         ),
                         values = c(
                           'Persistent detection' = '#67001F', 
                           'Detection' = '#FDDBC7',
                           'No recent detection'= '#2166AC',
                           'No recent data' = '#BBBBBB', 
                           'Influenza A'='black',
                           'Influenza B'='gray50'
                         ),
                         labels = c('Persistent detection',
                                    'Detection', 
                                    'No recent detection', 
                                    'No recent data',
                                    'Influenza A', 
                                    'Influenza B'
                         )
      )+
        
        coord_cartesian(xlim = c(
          input$daterange1[1], input$daterange1[2]
        )) +
        scale_y_continuous(labels = comma) +
        ylim(0, max_y*2) +
        theme_gdocs() +
        theme(text = element_text(family = "Arial", size = 8)),
      tooltip = "text") %>%
      layout(
        title = list(
          text = paste(
            "Normalized Influenza Viral Concentration by Sampling Date for",
            input$utility
          )
        ),
        xaxis = list(title = "Sample Collection Date", fixedrange = TRUE),
        yaxis = list(title = "Viral Concentration (gene copies per person per day", fixedrange = TRUE)
      ) %>%
      config(
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")
      )
    
  })     
  
  
  ##flu a subtyping
  output$flua_subtyping <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    
    #input <- data.frame(utility = "Alamosa", daterange1 = as.Date(c("2024-07-11", "2024-09-30")))
  
    
    flua_subtyping_detection <- flua_subtyping_data %>%
      select(utility, sample_id, measure_date, 
             h1_detected, h3_detected, h5_detected) %>%
      pivot_longer(cols = c(h5_detected, h3_detected, h1_detected), 
                   names_to = "pcr_target", 
                   values_to = "detection_status") %>%
      mutate(pcr_target = case_when(pcr_target == "h5_detected" ~ "H5",
                                              pcr_target == "h3_detected" ~ "H3",
                                              pcr_target == "h1_detected" ~ "H1",
                                              TRUE ~ pcr_target))
      
      
    flua_subtyping_conc_data <- flua_subtyping_data %>%
      select(utility, sample_id, measure_date, 
             h1_pcr_target_avg_conc, h3_pcr_target_avg_conc, h5_pcr_target_avg_conc) %>%
      pivot_longer(cols = c(h5_pcr_target_avg_conc, h3_pcr_target_avg_conc, h1_pcr_target_avg_conc), 
                   names_to = "pcr_target", 
                   values_to = "pcr_target_avg_conc") %>%
      mutate(pcr_target = case_when(pcr_target == "h5_pcr_target_avg_conc" ~ "H5",
                                    pcr_target == "h3_pcr_target_avg_conc" ~ "H3",
                                    pcr_target == "h1_pcr_target_avg_conc" ~ "H1",
                                    TRUE ~ pcr_target))
    
    viralconc_flua_subtyping <-  flua_subtyping_conc_data %>%
      left_join(flua_subtyping_detection, by = c("utility", "sample_id", "measure_date", "pcr_target")) %>%
      mutate(sample_id = case_when(sample_id == "" ~ NA, TRUE ~ sample_id)) %>%
      rename(output_viral_conc = pcr_target_avg_conc) %>%
      filter(utility %in% input$utility) %>%
      select(sample_id, utility, pcr_target, measure_date, 
             output_viral_conc, detection_status#, pcr_target_below_lod
             ) %>% 

      filter(!is.na(detection_status))


    
    ggplotly(
      ggplot(data = viralconc_flua_subtyping, 
             aes(x = measure_date, y = pcr_target,
                 text = paste0("<b>Subtype: </b>", pcr_target,
                               "<br><b>Date: </b>", measure_date,
                               "<br><b>Result: </b>", detection_status,
                               "<br><b>Viral Concentration: </b>", output_viral_conc#,
                               #"<br><b>Limit of Detection: </b>", output_viral_conc
                               )),) + 
        geom_tile(aes(fill = factor(detection_status)), color = "white",# size = 1.2
                  ) + 
        scale_fill_manual(name = "", na.value = "#DDDDDD",
                          values = c("Detected" = "#CC6677",
                                     "Not Detected"="#44AA99",
                                     "No Sample" = "#DDDDDD",
                                     "Not Tested" = "#DDDDDD",
                                     "Sample Processing" = "gray")) +
        labs(x = "Date", y = "Influenza A Subtype", fill = "detection_status") +
        coord_cartesian(xlim = c(
          input$daterange1[1], input$daterange1[2]
        )) +
        theme_gdocs() +
        theme(text = element_text(family = "Arial", size = 8)),
      tooltip = "text"
    ) %>%
      layout(
        title = list(
          text = paste(
            "Influenza A Subtyping by Sampling Date for", input$utility
          )
        ),
        yaxis = list(title = "Influenza A Subtype", fixedrange = TRUE),
        legend = list(
          orientation = "h",    # Horizontal orientation
          x = 0.5,              # Center the legend horizontally
          y = -0.2,             # Move the legend below the plot
          xanchor = "center",   # Align horizontally by the center
          yanchor = "top"       # Align vertically by the top
        )
      ) %>%
      config(
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")
      )
  })
  
  #flu A conc for subtyping plot
  output$flua_subtyping_conc <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    
    #input <- data.frame(utility = "Alamosa", daterange1 = as.Date(c("2024-05-01", "2024-09-01")))
    
    viralconc_flua <- wwdatafull %>%
      filter(utility %in% input$utility) %>%
      filter(pcr_target %in% c("FLUAV")) %>%
      mutate(pcr_target = case_when(pcr_target == 'FLUAV' ~ 'Influenza A', TRUE ~ pcr_target)) %>%
      filter(!is.na(viral_conc_raw)) %>%
      group_by(pcr_target) %>%
      rename(output_viral_conc = viral_conc_raw)
    
    max_y <- viralconc_flua %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    max_y <- max(max_y$output_viral_conc, na.rm = TRUE)
    max_y <- abs(max_y)
    
    
    ggplotly(
      ggplot() +
        geom_point(
          data = viralconc_flua,
          aes(
            x = measure_date,
            y = output_viral_conc,
            text = paste(
              "<b>Sample Date:</b> ",
              measure_date,
              "<br><b>Sample ID:</b> ",
              sample_id,              
              "<br><b>Lab Phase:</b> ",
              lab_phase,
              "<br><b>Viral Concentration:</b>",
              format(round(as.numeric(output_viral_conc), 0), nsmall = 0, big.mark = ","), 
              "copies/L<br><b>Sample Type:</b> ",sample_type),
            #color = factor(pcr_target),
            alpha = I(0.6))
        ) +
        geom_line(
          data = viralconc_flua,
          aes(x = measure_date, y = output_viral_conc, #color = factor(pcr_target)
          )
        ) +
        geom_vline(xintercept = as.numeric(as.Date("2024-09-30")), linetype = "dashed") +
        geom_text(
          data = data.frame(measure_date = as.Date("2024-09-30"), output_viral_conc = max_y * 1.5),
          aes(
            x = measure_date, 
            y = output_viral_conc, 
            label = "",
            text = "Lab Phase 3 Begins: September 30th, 2024"
          ),
          vjust = -1,
          hjust = 0.5,
          show.legend = FALSE,
          color = "black"
        ) +

        coord_cartesian(xlim = c(
          input$daterange1[1], input$daterange1[2]
        )) +
        scale_y_continuous(labels = comma) +
        ylim(0, max_y*2) +
        theme_gdocs() +
        theme(text = element_text(family = "Arial", size = 8)),
      tooltip = "text"
    ) %>%
      layout(
        title = list(
        ),
        xaxis = list(title = "Sample Collection Date", fixedrange = TRUE),
        yaxis = list(title = "Viral Concentration (gene copies/Liter)", fixedrange = TRUE)
      ) %>%
      config(
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")
      )
  })
  
  
  
  
  
  #county level case data with hospitalizations
  output$county_cases_flu <- renderPlotly({
    req(credentials()$user_auth)
    credentials()$info
    
    fluCountyCaseGraphData <- flu_casedata %>%
      filter(County %in% input$flu_case_county) %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    fluCountyCaseGraphData_diagnosed <- fluCountyCaseGraphData %>%
      filter(metric == "% Influenza ED Visits")
    
    fluCountyGraphData_Hosp <- flu_hosp %>%
      filter(County %in% input$flu_case_county) %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    
    #calcualte max y so y-axis auto adjusts based on data presented in selected date range
    max_y <- fluCountyCaseGraphData %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2])
    
    max_y <- max(max_y$CountyCases, na.rm = TRUE)
    max_y <- abs(max_y)
  
    # Calculate max y so y-axis auto adjusts based on data presented in selected date range
    max_y_cases <- fluCountyCaseGraphData %>%
      filter(measure_date >= input$daterange1[1] & measure_date <= input$daterange1[2]) %>%
      summarize(max_cases = max(CountyCases, na.rm = TRUE)) %>%
      pull(max_cases)
    
  
    max_y_hosp <- fluCountyGraphData_Hosp %>%
      summarize(max_hosp = max(Hospitalized_CaseCount_r100Kutil, na.rm = TRUE)) %>%
      pull(max_hosp)
    
    flu_case_plot <- plot_ly() %>%
      add_trace(
        data = fluCountyCaseGraphData,
        x = ~measure_date,
        y = ~CountyCases,
        type = "scatter",
        mode = "lines+markers",
        color = ~metric,
        colors = c("#d6604d", "#fd9a44"),
        legendgroup = ~metric,  # Group traces by "metric" in the legend
        name = ~metric,
        text = ~paste("<b>Metric:</b> ", metric, 
                      "<br><b>Date:</b> ", measure_date, 
                      "<br><b>Percent of Cases: </b>", 
                      format(round(as.numeric(CountyCases),1), nsmall = 0, big.mark = ",")),
        hoverinfo = "text"  # Set hoverinfo to "text" to display only the custom text
      ) %>%
      add_trace(
        data = fluCountyGraphData_Hosp,
        x = ~measure_date,
        y = ~Hospitalized_CaseCount_r100Kutil,
        yaxis = "y2",
        name = "Influenza Hospital Admission Rate",
        type = "scatter",
        mode = "lines+markers",
        color = "#d6604d",
        text = ~paste("<b>Metric:</b> Influenza Hospital Admission Rate", 
                      "<br><b>Date:</b> ", measure_date, 
                      "<br><b>Number of Admissions: </b>", 
                      format(round(as.numeric(Hospitalized_CaseCount_r100Kutil),1), nsmall = 0, big.mark = ",")),
        hoverinfo = "text"  # Set hoverinfo to "text" to display only the custom text
      ) %>%
    add_trace(
      data = fluCountyCaseGraphData_diagnosed,
      x = ~measure_date,
      y = ~CountyCases,  # Replace YourMetricVariable with the actual variable you want for the area plot
      type = "scatter",
      mode = "lines",
      fill = "tozeroy",  # Specify "tozeroy" for area plot
      color = ~metric,
      colors = c("#d6604d", "#fd9a44"),
      line = list(color = "rgba(255,255,255,0)"),  # Set line color to transparent for area plot
      showlegend = FALSE,  # Do not show legend for the area plot
      text = ~paste("<b>Metric:</b> ", metric, 
                    "<br><b>Date:</b> ", measure_date, 
                    "<br><b>Percent of Cases: </b>", 
                    paste0(format(round(as.numeric(CountyCases),1), nsmall = 0, big.mark = ",")), "%"),
      hoverinfo = "text"  # Skip hoverinfo for the area plot
    ) %>%
      layout(
        title = list(
          text = paste(
            "Influenza Clinical Measures for",
            input$flu_case_county,
            "County"
          ),
          font = list(size = 14, family = "Arial")
        ),
        xaxis = list(title = "Test Date", tickfont = list(size = 10, family = "Arial"), fixedrange = TRUE),
        yaxis = list(side = 'left', title = 'Percent of Total ED Visits', tickfont = list(size = 10, family = "Arial"),
                     showgrid = FALSE, zeroline = FALSE, range=list(0, max(fluCountyCaseGraphData$CountyCases)*1.5)
        ),
        yaxis2 = list(side = 'right', overlaying = 'y', title = 'Influenza-Associated Hospitalizations per 100,000', tickfont = list(size = 10, family = "Arial"),
                      range=list(0, max(fluCountyGraphData_Hosp$Hospitalized_CaseCount_r100Kutil)*1.5)),
        legend = list(orientation = "h",
                      y = -.2  # Adjust this value to move the legend lower
        ),
        margin = list(r = 50),  # Increase the right margin value to create more space
        bargap = 0.8  # Adjust this value to set the thickness of the bars
      )
    ggplotly(flu_case_plot)
  })
  
  
  
 
  
  ###################################
  ###       Extras              #####
  ###################################
  
  #prep data for data download buttons
  data <- wwdatafull %>%
    select(-c(trend, p_val, slope, first_date, lpha1_contactname, lpha1_contactemail, lab_id, quality_flag)) %>%
    rename(`ViralConcentration_Linear` = viral_conc_raw) %>%
    rename(`ViralConcentration_Log` = log_viral_conc_raw) %>%
    rename(`Sewershed_Population` = population_served) %>%
    rename(`Sample_Collect_Date` = measure_date) %>%
    rename(`Normalized_ViralConcentration_Linear` = viral_conc_fpnorm) %>%
    rename(`Normalized_ViralConcentration_Log` = log_viral_conc_fpnorm) %>%
    filter(!pcr_target %in% c("Mpox Clade I", "Mpox Clade II"))
  
  #download all
  output$downloadData_all <- downloadHandler(
    
    filename = function() {
      req(credentials()$user_auth)
      credentials()$info
      
      paste('Wastewater_Viral_Load_Data_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      req(credentials()$user_auth)
      credentials()$info
      
      choices <- credentials()$info %>%
        dplyr::select(allowed_utilities) %>%
        unlist() %>%
        as.character()
      
      write.csv(data %>%
                  filter(utility %in% choices),
                con)
    }
  )
  
  #download selected
  output$downloadData_selected <- downloadHandler(
    
    filename = function() {
      req(credentials()$user_auth)
      credentials()$info
      
      paste('Wastewater_Viral_Load_Data_Selected_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      req(credentials()$user_auth)
      credentials()$info
      
      write.csv(data %>%
                  filter(utility %in% input$utility) %>%
                  filter(`Sample_Collect_Date` >= input$daterange1[1] & `Sample_Collect_Date` <= input$daterange1[2]),
                con)
    }
  )
  
  #download data dictionary
  output$downloadData_dictionary <- downloadHandler(
    
    filename = function() {
      req(credentials()$user_auth)
      credentials()$info
      
      paste('Wastewater_Viral_Load_Data_Dictionary_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      req(credentials()$user_auth)
      credentials()$info
      
      choices <- credentials()$info %>%
        dplyr::select(allowed_utilities) %>%
        unlist() %>%
        as.character()
      
      write.csv(data_dictionary,
                con)
    }
  )
  
  
  ##explanations for text boxes
  output$totalUtils_text <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML(totalUtils))
    }
    else {
      tags$div("")
      
    }
  })
  
  output$totalPop_text <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML(totalPop))
    }
    else {
      tags$div("")
      
    }
  })
  
  output$totalCounties_text <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML(totalCounties))
    }
    else {
      tags$div("")
      
    }
  })
  
  output$welcome_text <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML("<b>Welcome to the  [ADD JURISDICTION NAME HERE] Wastewater Internal Dashboard!</b>
                          <br> [ADD INFORMATION HERE ABOUT YOUR JURISDICTION’S SURVEILLANCE PROGRAM; TARGETS MONITORED, SAMPLING FREQUENCY, DASHBOARD UPDATE CADENCE, ETC.]
              
            <br>
            <br>
            
            <b><i>  Why is Wastewater Surveillance Useful?</i></b>
            <br>
                    Monitored pathogens can be shed in stool by asymptomatic individuals, before someone exhibits symptoms or tests positive for an infectious disease in clinic, and therefore the genetic material of these pathogens can be found in our wastewater. Wastewater surveillance data complements existing communicable disease surveillance systems, such as clinical testing and hospitalization data. Wastewater surveillance data serves as an anonymized community sample — viral genomes cannot be used to identify individuals — and it can also provide an additional source of trend data in our communities without solely relying on access to healthcare and clinical testing within a population. [ADD ANY OTHER KEY BENEFITS OF WASTEWATER SURVEILLANCE HERE]"))
    }
    else {
      tags$div(HTML("<center><b> Please log in to access the dashboard. </b></center>"))
      
    }
  })
  
  output$aboutwwprogram_text <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML(paste(
        "The [ADD JURISDICTION/AGENCY NAME HERE]Wastewater Surveillance Program is engaged in <b>", totalUtils, "</b> active partnerships across [ADD JURISDICTION NAME HERE] to collect wastewater samples [ADD SAMPLING FREQUENCY].
              <br>
              <br>
              Samples are transported to the laboratory via[ADD TRANSPORTATION METHOD HERE (E.G., COURIER OR FEDEX)]  for [ADD LAB METHOD HERE (E.G., digital polymerase chain reaction (dPCR)] analysis to quantify viral concentration levels. Results are analyzed and posted to this internal dashboard before we disseminate the data on our public-facing dashboard.
              <br>
              <br>
              The [ADD JURISDICTION NAME HERE] Wastewater Utility map shows our publicly shared sewershed areas. Use your mouse to hover over a sewershed area for more information, including the utility name, the date of the most recent sample collected, the sewershed area population estimate, and the primary LPHA for each sewershed area."
      ))
      )
      
    }
    else {
      tags$div("")
      
    }
  })
  
  
  output$trends_text_allutilities <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML(
        "The Utility Trend Summary represents a snapshot of current trends and SARS-CoV-2 viral concentration levels of the most recent sample submitted by each utility participating in [ADD JURISDICTION NAME HERE] Wastewater Surveillance Program.
                   <br>
                   <br>
         The <b> Viral Concentration Percentile</b> represents the viral concentration level of the most recent sample relative to historical values for that utility. For example, if a sample concentration falls at the 75th percentile level, the recent sample concentration value is higher than 75% percent of samples collected since [ADD DATE]. Historical values only include Lab Phase 2 data (see Methodology section below for more details about lab phases).
                   <br>
                   <br>
        The <b>Slope of the Recent Trend</b> indicates the direction and magnitude of the trend line calculated with the most 
        recent sample data. For example, a higher, positive value presents a steep and increasing trend; a lower, negative value 
        represents a mildly decreasing trend. <b>Statistical significance</b> of the trend is denoted by a gray dot and black outline; a 
        non-significant trend indicates the trend is plateauing and not definitively increasing or decreasing (see Methodology 
        section below for more details about trend calculations).
                   <br>
                   <br>
        The <b>size of the utility dot</b> is proportional to the contributing population of the utility. 
                   <br>
                   <br>
        These two measures together provide a statewide and regional perspective on both the current trend and SARS-CoV-2 
        viral concentration levels.
                  <br>
                  <br>
        The table provides a summary of the utilities in each quadrant and it automatically updates when a new region and/or classification is selected.
                  <br>
                  <br>
        Users can zoom in and out of a section of the plot by selecting the plus (+) and minus (-) sign at the top of the plot. 
        The ‘Reset axes’ button will return the plot to the original bounds."
      ))
    }
    else {
      tags$div("")
      
    }
  })
  
  output$trends_text_individualutility <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML(
        "The Utility Trend Summary plot represents the trend and viral concentration levels of the three most recent 
        samples submitted by the selected utility. 
                   <br>
                   <br>
        The three most recent samples are displayed on the plot to indicate the recent trend and concentration levels detected. Statistical significance of the trend is denoted by a gray dot and black outline; a non-significant trend indicates that the trend is plateauing and not definitively increasing or decreasing (see Methodology section below for more details about trend calculations).
                   <br>
                   <br>
        This summary plot provides details about the recent changes in the concentration level and both the magnitude and direction of recent trends. For example, a dot that moves from an orange quadrant to a purple quadrant represents a shift from an increasing trend to a decreasing trend.
                  <br>
                  <br>
        Users can zoom in and out of a section of the plot by selecting the plus (+) and minus (-) sign at the 
        top of the plot. The ‘Reset axes’ button will return the plot to the original bounds."
      ))
    }
    else {
      tags$div("")
      
    }
  })
  
  output$viralconc_details_sars <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML(
        "The Viral Concentration graph shows the raw number of gene copies detected per liter 
          in the wastewater sample by the date the sample was collected. 
                   <br>
                   <br>
        In the SARS-CoV-2 graph, the most recent sample is color-coded by the trend calculated for that sample; 
        either an “increase”, “plateau”, “decrease”, or “Insufficient Data” trend. A trend will appear as “Insufficient Data” 
        when there are not enough (<5) samples within a 21-day period. This sample-level classification is used to generate the 
        weekly trend categories displayed on our public-facing dashboard. See the methodology section below for more details about
        trend classifications and Lab Phases. 
                   <br>
                   <br>
                   The blue horizontal shaded region represents the Limit of Detection (LOD). Lab Phase 1 and 2 results have a LOD of 2000 copies/L, 
                   while Lab Phase 3 results have a LOD of 1200 copies/L.The LOD for Lab Phase 3 is lower due to the implementation of more sensitive 
                   laboratory methods for quantifying viral concentration. 
                   <br>
                   <br>
                   The dashed vertical line represents the lab method transition date. Viral concentration data are not directly comparable between lab phases.
                   <br>
                   <br>
                   Navigation tips: 
                   <br>  <li style='margin-left: 25px;'> Hover over each dot on the line to view the wastewater sample details, 
                            including the sample collection date, viral concentration, and sample collection type. 
                   <br>  <li style='margin-left: 25px;'> Click on a legend item to remove that category from view.
                   <br>  <li style='margin-left: 25px;'> Use the “Select a Metric” feature to toggle between the linear and log-adjusted concentration values."))
    }
    else {
      tags$div("")
      
    }
  })
  
  output$normviralconc_details_sars <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML("The Normalized Viral Concentration line graph shows the normalized concentration of viral genetic material in the wastewater. 
                    These values are normalized by the flow rate for each sample and the population of the sewershed. Please see the methodology details 
                    below for more information about this calculation. 
                 <br>
                 <br>
                   The dashed vertical line represents the lab method transition date. Viral concentration data are not directly comparable between lab phases.
                   <br>
                   <br>
                Navigation Tips: 
                <br>  <li style='margin-left: 25px;'> Hover over each dot on the line to view the flow-and-population normalized wastewater sample details, 
                        including the sample collection date, normalized concentration value, and sample collection type.
                <br>  <li style='margin-left: 25px;'> Click on a legend item to remove that category from view.
                <br>  <li style='margin-left: 25px;'> Use the “Select a Metric” feature to toggle between the linear and log-adjusted concentration values; 
                        log-adjustment is the preferred data transformation for viewing virological data."))
      
    }
    else {
      tags$div("")
      
    }
  })
  
  output$omicron_text <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML("The SARS-CoV-2 Variant Heatmap displays information about the variants monitored and detected in wastewater samples through whole genome sequencing.
                    <br>
                    <br>
                    Confirmed variants in the wastewater are labeled as “Detection”. When we confirm there is no variant present, these are labeled as “No Detection”. Samples are labeled as “Insufficient Signal” when there is not a sufficient amount of virus in the sample for successful sequencing. This does not necessarily mean that the original wastewater sample was of poor quality, but rather that there wasn't enough virus to confidently classify variants of concern through whole genome sequencing.
                 <br>
                 <br>
                 Navigation Tips: 
                  <br> <li style='margin-left: 25px;'> Hover over the heatmap to view details about the sample date and variant detection. For more details about whole genome sequencing results, 
                          please see the methodology section below.
                  <br> <li style='margin-left: 25px;'> Click on a legend item to remove that category from view."))
    }
    else {
      tags$div("")
      
    }
  })
  
  output$caserate_text_sars <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML("The COVID-19 Cases per 100,000 graph shows the 3-day rolling case rate for each county. 
                    The COVID-19 case rate is determined by the number of confirmed cases in a given county, 
                    multiplied by 100,000, and divided by the county population. Cases are averaged over 3 days.  
                <br>
                <br>
                    While sewershed areas don’t typically align with county boundaries, there is often a relationship between the viral 
                concentration in overlapping sewersheds and the cases detected in the county. Calculating the case rate per 100,000 people 
                allows for easy comparison of rates between counties.  
                <br>
                <br>
                Navigation Tips: 
                  <br>  <li style='margin-left: 25px;'> The counties that overlap with the selected sewershed are listed below.
                  <br>  <li style='margin-left: 25px;'> Use the drop-down menu to view the case rate within each of these counties to 
                                                        compare with viral concentration levels in the overlapping sewershed area."))
      
    }
    else {
      tags$div("")
      
    }
  })
  

  output$hosprate_text_sars <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML("The COVID-19 Hospitalization Rate graph shows the 3-day rolling average of the total COVID-19 hospital admissions per 100,000 by test collection date.  
                 <br>
                 <br>
                  While sewershed areas don’t typically align with county boundaries, there is often a relationship between the viral concentration in 
                 overlapping sewersheds and COVID-19 hospitalizations. Calculating COVID-19 hospitalization rates is a useful metric for determining 
                 COVID-19 disease severity, and can indicate when the severity of outcomes is increasing.
                <br>
                <br>
                Navigation Tips: 
                  <br>  <li style='margin-left: 25px;'> The counties that overlap with the selected sewershed are listed below.
                  <br>  <li style='margin-left: 25px;'> Use the drop-down menu to view the percent positivity within each of these counties to compare 
                                                        with viral concentration levels in the overlapping sewershed area."))
      
    }
    else {
      tags$div("")
      
    }
  })
  
  

  
  output$viralconc_details_rsv <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML("The Viral Concentration graph shows the raw number of gene copies detected per liter in the wastewater sample by the date the sample was collected. 
                               <br>
                               <br>
                               Samples submitted by participating wastewater utilities for Respiratory Syncytial Virus monitoring are tested 
                               for both RSV-A and RSV-B. RSV is contagious and it usually causes mild, cold-like symptoms, and can cause more 
                               severe outcomes in infants and older adults.
                               <br>
                               <br>
                               The blue horizontal shaded region represents the Limit of Detection (LOD). Lab Phase 1 and 2 results have a LOD of 2000 copies/L, 
                               while Lab Phase 3 results have a LOD of 1200 copies/L.The LOD for Lab Phase 3 is lower due to the implementation of more sensitive 
                               laboratory methods for quantifying viral concentration. 
                               <br>
                               <br>
                               The dashed vertical line represents the lab method transition date. Viral concentration data are not directly comparable between lab phases."), 
               tags$a(href="https://www.cdc.gov/rsv/index.html", "(CDC)"),
               HTML("<br>
                                <br>
               Navigation tips: 
               <br>  <li style='margin-left: 25px;'> Hover over each dot on the line to view the wastewater sample details, 
                        including the sample collection date, viral concentration, and sample collection type.
               <br>  <li style='margin-left: 25px;'> Click on a legend item to remove that category from view.
               <br>  <li style='margin-left: 25px;'> Use the “Select a Metric” feature to toggle between the linear and log-adjusted concentration values."))
      
    }
    else {
      tags$div("")
      
    }
  })
  
  
  output$normviralconc_details_rsv <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML("The Normalized Viral Concentration line graph shows the normalized concentration of viral genetic material in the wastewater. 
                    These values are normalized by the flow rate for each sample and the population of the sewershed. Please see the methodology details 
                    below for more information about this calculation. 
                 <br>
                 <br>
                   The dashed vertical line represents the lab method transition date. Viral concentration data are not directly comparable between lab phases.
                   <br>
                   <br>
                Navigation Tips: 
                <br>  <li style='margin-left: 25px;'> Hover over each dot on the line to view the flow-and-population normalized wastewater sample details, 
                        including the sample collection date, normalized concentration value, and sample collection type.
                <br>  <li style='margin-left: 25px;'> Click on a legend item to remove that category from view.
                <br>  <li style='margin-left: 25px;'> Use the “Select a Metric” feature to toggle between the linear and log-adjusted concentration values; 
                        log-adjustment is the preferred data transformation for viewing virological data."))
      
    }
    else {
      tags$div("")
      
    }
  })
  
  output$caserate_text_rsv <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML("The County RSV-Related Case Rates graphs show recent sentinel and syndromic case data metrics by county.
                    <br>
                    <br>The <b>Percent of RSV ED Visits</b> displays the weekly percentage of total ED visits with a RSV diagnosis.
                    <br>
                    <br>The <b>RSV Hospital Admission Rate</b> represents the weekly rate of RSV-diagnosed hospital admissions per 100,000 people based on the county population.
                    <br>
                    <br>These metrics are calculated with sentinel and syndromic surveillance data, which represent a sample of cases in each county. Some counties do not have an associated sentinel or syndromic site, and therefore no county-level case data is available. In these instances, it is recommended to view data for nearby counties to compare with wastewater levels."))
      
    }
    else {
      tags$div("")
      
    }
  })
  
  output$viralconc_details_flu <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML(
        "The Viral Concentration graph shows the raw number of gene copies detected per liter in the wastewater sample by 
                   the date the sample was collected. 
                   <br>
                   <br>
                   Samples submitted by participating wastewater utilities for Influenza monitoring are tested for both Influenza A and Influenza B. 
                   Flu is a contagious respiratory virus that infects the nose, throat, and sometimes the lungs. It can cause mild to severe illness, 
                   and at times can lead to death.
                   <br>
                   <br>
                   The blue horizontal shaded region represents the Limit of Detection (LOD). Lab Phase 1 and 2 results have a LOD of 2000 copies/L, 
                   while Lab Phase 3 results have a LOD of 1200 copies/L.The LOD for Lab Phase 3 is lower due to the implementation of more sensitive 
                   laboratory methods for quantifying viral concentration. 
                   <br>
                   <br>
                   The dashed vertical line represents the lab method transition date. Viral concentration data are not directly comparable between lab phases."),
        tags$a(href="https://www.cdc.gov/flu/about/keyfacts.htm", "(CDC)"),
        HTML("<br>
  <br>
               Navigation tips: 
               <br>  <li style='margin-left: 25px;'> Hover over each dot on the line to view the wastewater sample details, 
                        including the sample collection date, viral concentration, and sample collection type. 
               <br>  <li style='margin-left: 25px;'> Click on a legend item to remove that category from view.
               <br>  <li style='margin-left: 25px;'> Use the “Select a Metric” feature to toggle between the linear and log-adjusted concentration values."))
      
    }
    else {
      tags$div("")
      
    }
  })
  
  output$normviralconc_details_flu <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML("The Normalized Viral Concentration line graph shows the normalized concentration of viral genetic material in the wastewater. 
                    These values are normalized by the flow rate for each sample and the population of the sewershed. Please see the methodology details 
                    below for more information about this calculation. 
                 <br>
                 <br>
                   The dashed vertical line represents the lab method transition date. Viral concentration data are not directly comparable between lab phases.
                   <br>
                   <br>
                Navigation Tips: 
                <br>  <li style='margin-left: 25px;'> Hover over each dot on the line to view the flow-and-population normalized wastewater sample details, 
                        including the sample collection date, normalized concentration value, and sample collection type.
                <br>  <li style='margin-left: 25px;'> Click on a legend item to remove that category from view.
                <br>  <li style='margin-left: 25px;'> Use the “Select a Metric” feature to toggle between the linear and log-adjusted concentration values; 
                        log-adjustment is the preferred data transformation for viewing virological data."))
      
      
    }
    else {
      tags$div("")
      
    }
  })
  
  
  output$flua_subtyping_details <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML("Influenza A and B viruses can cause seasonal epidemics in humans. Influenza A viruses are monitored closely 
    due to their pandemic potential, and are further classified into subtypes based on the properties of their surface proteins, 
    hemagglutinin (H or HA) and neuraminidase (N or NA). There are 18 different HA subtypes. Increased levels of influenza A in 
    the wastewater prompts further subtyping and genetic sequencing to determine the presence of H1, H3, or H5. 
    <br>
    <br>
    The data to the left displays the genetic sequencing data for H1, H3, and H5 in wastewater samples from participating utilities. 
    The detection indicator shows the dates that wastewater samples were collected for that utility, and the sequencing results. 
    The presence of these subtypes may be indicative of human or animal inputs in the wastewater; detections warrant further 
    monitoring for clinical cases in the region. A viral concentration graph is also available for each utility, displaying the 
    levels of influenza A (gene copies/liter) in the wastewater samples for each collection date. This allows users to view the 
    viral concentration levels of influenza A over time.
               <br>
               <br>
              Navigation Tips: 
              <br>  <li style='margin-left: 25px;'> Click on a legend item to remove that category from view."))
      
      
    }
    else {
      tags$div("")
      
    }
  })
  
  output$flua_subtyping_additionaldetails <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML("<b>H5:</b>  H5 is a type A strain of the influenza virus that causes avian flu, or bird flu. It can infect 
    many types of birds,livestock, and other mammals, but rarely spreads to humans. Individuals who work with poultry, 
    waterfowl, and livestock are at greater risk. At present, there is no documented person-to-person spread of this subtype 
    in the U.S. Detections of H5 in the wastewater may indicate that sick animals and/or affected farms are contributing to the 
    wastewater. The most likely inputs to the wastewater are permitted discharges from industries that process animal by-products 
    such as dairies, poultry farms or plants, or meat processing plants. There is also the possibility of infected animal input 
    into the wastewater system that could be contributing to H5 detections, such as wild bird droppings. Current wastewater
    sample testing technology does not allow for identification of the source of the positive detection (poultry, bovine, 
    or human), and it is unclear if the H5 detections are the result of infected human cases contributing to the wastewater 
    samples. When H5 is detected in a wastewater sample, CDPHE conducts further investigations to determine the possible 
    input contributing to the detection. 
               <br>
               <br>
    <b>H3:</b> H3 influenza refers to a subtype of influenza A virus that is endemic and can infect humans, pigs, birds, horses, 
    and other mammals. H3 is a major strain of seasonal flu that spreads from person to person every year, and is included 
    in the annual vaccine. H3 detections in the wastewater samples could indicate increased clinical rates of human cases 
    in the area that contribute to the sewershed via viral shedding in stool, which is why wastewater surveillance data is
    always interpreted in conjunction with clinical or syndromic surveillance data.        
              <br>
              <br>
    <b>H1:</b> The H1 subtype refers to a subtype of influenza A virus that is endemic in humans and is the main contributor to 
    person to person spread of seasonal influenza every year. H1 is included in the annual flu vaccine. H1 detections in 
    the wastewater samples could indicate increased clinical rates of human cases in the area that contribute to the 
    sewershed via viral shedding in stool, which is why wastewater surveillance data is always interpreted in conjunction
    with clinical or syndromic surveillance data."))
      
      
    }
    else {
      tags$div("")
      
    }
  })
  
  
  
  output$caserate_text_flu <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML("The County Flu-Related Case Rates graphs show recent sentinel and syndromic case data metrics by county.
                    <br>
                    <br>The <b>Percent of Influenza ED Visits</b> displays the weekly percentage of total ED visits with an Influenza diagnosis.
                    <br>
                    <br>The <b>Influenza Hospital Admission Rate</b> represents the weekly rate of Influenza-diagnosed hospital admissions per 100,000 people based on the county population.
                    <br>
                    <br>These metrics are calculated with sentinel and syndromic surveillance data, which represent a sample of cases in each county. Some counties do not have an associated 
                    sentinel or syndromic site, and therefore no county-level case data is available. In these instances, it is recommended to view data for nearby counties to compare with 
                    wastewater levels."))
    }
    else {
      tags$div("")
      
    }
  })
  

  
  
  output$text_detectionstatus_rsv <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML(
        "The most recent sample is color-coded by the trend calculated for that sample:
          <br><b><li style='margin-left: 25px;'> Persistent detection:</b> 
          <br><p style='margin-left: 25px;'> The virus that causes RSV was detected in more than 80% of samples in the past 4 weeks AND the most recent detection was within the past 2 weeks.
          <br><b><li style='margin-left: 25px;'> Detection:</b> 
          <br><p style='margin-left: 25px;'> The virus that causes RSV was detected in 1% to 80% of samples in the past 4 weeks AND the most recent detection was within the past 2 weeks.
          <br><b><li style='margin-left: 25px;'> No recent detection:</b>
          <br><p style='margin-left: 25px;'> The virus that causes RSV was not detected in any samples in the past 2 weeks.
          <br><b><li style='margin-left: 25px;'> No recent data:</b> 
          <br><p style='margin-left: 25px;'> Fewer than 3 samples were submitted in the past 4 weeks.
          <br>
          <br>
          </li style='margin-left: 25px;'>For more information relating to these classifications, please refer to the "),
        tags$a(href="https://www.cdc.gov/nwss/wastewater-surveillance/mpox-data.html", " National Wastewater Surveillance System"),
        HTML("website."))
      
    }
    else {
      tags$div("")
      
    }
  })
  
  output$text_detectionstatus_flu <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML(
        "The most recent sample is color-coded by the trend calculated for that sample:
          <br><b><li style='margin-left: 25px;'> Persistent detection:</b> 
          <br><p style='margin-left: 25px;'> The virus that causes influenza was detected in more than 80% of samples in the past 4 weeks AND the most recent detection was within the past 2 weeks.
          <br><b><li style='margin-left: 25px;'> Detection:</b> 
          <br><p style='margin-left: 25px;'> The virus that causes influenza was detected in 1% to 80% of samples in the past 4 weeks AND the most recent detection was within the past 2 weeks.
          <br><b><li style='margin-left: 25px;'> No recent detection:</b>
          <br><p style='margin-left: 25px;'> The virus that causes influenza was not detected in any samples in the past 2 weeks.
          <br><b><li style='margin-left: 25px;'> No recent data:</b> 
          <br><p style='margin-left: 25px;'> Fewer than 3 samples were submitted in the past 4 weeks.
          <br>
          <br>
          </li style='margin-left: 25px;'>For more information relating to these classifications, please refer to the "),
        tags$a(href="https://www.cdc.gov/nwss/wastewater-surveillance/mpox-data.html", " National Wastewater Surveillance System"),
        HTML("website."))
      
    }
    else {
      tags$div("")
      
    }
  })
  
  
  output$methodology_text <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML("<b> [ADD YOUR JURISDICTION'S METHODOLOGY HERE. THE FOLLOWING IS AN EXAMPLE: </b>
                <br>
                <br>
                    
                    
                    <b>Objectives:</b>"),
               tags$ol(
                 tags$li("Identify SARS-CoV-2 viral concentration trends over time by wastewater utilities participating in the [# ADD JURISDICTION NAME HERE] Wastewater Surveillance Program."),
                 tags$li("Present data and trends as a resource for local public health agencies (LPHAs) and wastewater utilities to 
                                          monitor and interpret wastewater viral concentration data in consideration with other measures of disease 
                                          burden in a community for public health planning and action.")),
               HTML(
                 "<br>
                             <b>Data collection:</b>
                             <br>
                                      The [ADD JURISDICTION NAME HERE] Wastewater Surveillance Program partners with wastewater utilities across [ADD JURISDICTION NAME HERE] to collect influent wastewater samples twice weekly. Our utility partners collect 24-hour flow-weighted composite samples or grab samples in circumstances when a utility does not have access to an autosampler.
                            <br>
                            <br>
                                     Collected samples are transported to the laboratory for digital polymerase chain reaction (dpcr) analysis to quantify viral concentration levels and for whole genome sequencing analysis and detection of specific variants of concern. 
                            <br>
                            <br>
                            <b>Data analysis:</b>
                            <br>
                                      Data received from the [ADD JURISDICTION NAME HERE] lab is manually reviewed and then aggregated in R Studio 2022.02.1+461. 
                            
                            <br>
                            <br> A"), tags$a(href="chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://cran.r-project.org/web/packages/bsts/bsts.pdf", "Bayesian Structural Time Series (BSTS)"),                  
               HTML("linear modeling (LM) approach is used to evaluate and characterize trends for each  sewershed based on 
                                      longitudinal sample data. This model was selected for its ability to fit wastewater data from participating utilities with wide-ranging variability 
                                      in local characteristics, including population size and flux. 
                            <br>
                            <br>
                                      A sample-level trend category is determined based on the model fit for each sample. A 21-day window is used to determine the trend category for each sample; there must be at least 5 samples within this 21-day period to determine the trend category for each individual sample reliably. 
                            <br>
                            <br>"),
               tags$ol(
                 tags$li("slope > 0 and p-value < 0.05 \uD83E\uDC32 Increasing"),
                 tags$li("slope < 0 and p-value < 0.05 \uD83E\uDC32 Decreasing"),
                 tags$li("p-value > 0.05 \uD83E\uDC32 Plateau")),
               HTML(
                 "Weekly wastewater trends are characterized by examining the BSTS-LM trend categories for the <u>two most recent samples</u> from each WWTF. 
                                      The weekly trend categorizations are <u>not</u> displayed on the internal dashboard; they are only determined for weekly trend updates to the 
                                      public dashboard.                             
                            <br>
                            <br>
                            The weekly trend classifications are as follows according to the modeling results:
                            <br>
                            <br>  <li style='margin-left: 25px;'> If both samples are “increasing” \uD83E\uDC32 Steady Increase 
                            <br>  <li style='margin-left: 25px;'> If one sample is “increasing” and the other “plateau” \uD83E\uDC32 Increase 
                            <br>  <li style='margin-left: 25px;'> If both samples are “plateau” \uD83E\uDC32 “Plateau” 
                            <br>  <li style='margin-left: 25px;'> If one sample is “decreasing” and the other “plateau” \uD83E\uDC32 Decrease 
                            <br>  <li style='margin-left: 25px;'> If both samples are “decreasing” \uD83E\uDC32 Steady Decrease
                            <br>  <li style='margin-left: 25px;'> If there are <2 samples submitted within 15 days, or if the wastewater utility does not have 5 samples 
                                      submitted within the 21 day window required for the model, we are unable to classify trends and the trend is 
                                      categorized as “Insufficient Data”. </li>
                            <br>
                            <b>Limitations:</b>
                              <br>  <li style='margin-left: 25px;'> Values below the level of detection (LOD) are all estimated to be ½ of the level of detection (½ * 4000 gene copies per Liter = 2000 gene copies/Liter) per CDC’s guidance. 
                              <br>  <li style='margin-left: 25px;'> Rolling onboarding of wastewater utilities creates challenges with uniform comparison of wastewater trends and identification of a singular threshold value.
                              <br>  <li style='margin-left: 25px;'> Due to the inherent differences between wastewater treatment facilities, we are unable to directly compare viral concentration values between sewersheds."))
      
    }
    else {
      tags$div("")
      
    }
  })
  
  
  output$glossary_text <- renderUI({
    if(credentials()$user_auth){
      tags$div(HTML("<b> [ADD YOUR JURISDICTION'S METHODOLOGY HERE. THE FOLLOWING IS AN EXAMPLE: </b>
                            <br>
                            <br>
                            <b>Bayesian Structural Time Series (BSTS) Model:</b>
                                      A statistical technique used for time series forecasting designed to work with current and historic time series data.
                            <br>
                            <br>
                            <b>EV-D68 or Enterovirus D68:</b>
                                      A virus that can cause respiratory infections and in rare cases, a serious illness called acute flaccid myelitis (AFM)."), tags$a(href="https://www.cdc.gov/non-polio-enterovirus/about/ev-d68.html", "(CDC)"),                  
        HTML("
                            <br>
                            <br>
                            <b>Flow rate:</b>
                                      Volume of wastewater influent collected over a defined period of time. Measured in millions of gallons per day. 
                            <br>
                            <br>
                            <b>Gene copies detected per liter:</b>
                                      The unit used to measure the concentration of a pathogen’s genetic material in wastewater. 
                            <br>
                            <br>
                            <b>Gene:</b>
                                      A segment of genetic material that may act as a set of instructions for the associated cell or pathogen. In wastewater surveillance, a target gene refers to a specific gene or genetic marker that is monitored or analyzed to provide information about the abundance of specific pathogens in a wastewater sample.
                            <br>
                            <br>
                            <b>Genome:</b>
                                      All of the genetic material in an organism made of DNA (or RNA in some viruses).
                            <br>
                            <br>
                            <b>Grab Sample:</b>
                                      Collection of a sample of wastewater at a single time point as opposed to a composite sample collected over a defined period of time. 
                            <br>
                            <br>
                            <b>Influenza:</b>
                                      A contagious respiratory illness caused by influenza viruses that infect the nose, throat, and sometimes the lungs. Influenza A and B are the two main types that routinely spread in humans and cause seasonal flu epidemics."), tags$a(href="https://www.cdc.gov/flu/about/index.html", "(CDC)"),                  
        HTML("
                            <br>
                            <br>

                            <b>Limit of Detection (LOD):</b>
                                      The ability of an assay to detect a particular analyte at low levels; for CDPHE’s lab methodology, the LOD is represented by at least three positive partitions in a given sample. Three positive partitions is the lower limit for which we can confidently distinguish a positive detection from 0 and is a qualitative cutoff for the assay. It is important to assess both the LOD and LOQ for quality assurance of sample testing methods. 
                            <br>
                            <br>
                            <b>Limit of Quantification (LOQ):</b>
                                      The lowest amount of a target pathogen in a sample that can be quantitatively determined given a particular lab process; for CDPHE’s lab methodology, the LOQ is 4,000 copies per liter for all pathogens assessed in Lab Phase 1 and 2, with 1,200 copies per liter for all pathogens assessed in Lab Phase 3. It is important to assess both the LOD and LOQ for quality assurance of sample testing methods.
                            <br>
                            <br>
                            <b>Linear Regression Model:</b>
                                      A way of understanding the relationship between two quantitative variables by fitting a linear equation, or a straight line, to the data. The linear regression model will help to estimate the dependent variable based on the independent (or explanatory) variable. In conjunction with the Bayesian Structural Time Series (BSTS) Model, linear modeling is applied to predict trends in wastewater viral concentration over time. 
                            <br>
                            <br>
                            <b>Linear Viral Concentration:</b>
                                      The raw (unadjusted) viral concentration of the pathogen found in wastewater as quantified by digital polymerase chain reaction (dPCR).  Measured in gene copies per liter.
                            <br>
                            <br>
                            <b>Log scale:</b>
                                      The logarithmic scale with a base of 10 is a commonly used scale to display a wide range of numeric values. Logarithmic scales are particularly useful when dealing with data that spans a wide range of magnitudes, as they compress large ranges into more manageable visualizations. Viral concentrations tend to vary over a wide range; therefore, using this scale allows for easier comparison between the low levels and high levels.
                            <br>
                            <br>
                            <b> Normalized Viral Concentration:</b>
                                      The concentration of viral genetic material in the wastewater standardized by the flow rate during the collection period for each sample and the population of the sewershed. Measured in million gene copies per person per day.                             <br>
                            <br>
                            <b>SARS-CoV-2 variants:</b>
                                      Wastewater samples undergo genomic sequencing to identify variants of SARS-CoV-2, the virus that causes COVID-19. These variants are characterized by genetic mutations and may vary in severity of associated symptoms and patterns of fecal shedding.
                            <br>
                            <br>
                            <b>Percent positivity:</b>
                                      An estimate of the prevalence of an illness within a population. This is calculated by dividing the number of positive tests by the total number of tests conducted and multiplying that value by 100 to get a percent.  
                                      <br>
                                      Percent Positivity = (Number of positive tests / Total number of tests conducted) x 100
                            <br>
                            <br>
                            <b>Digital Polymerase Chain Reaction (dPCR):</b>
                                      A laboratory method that allows for rapid amplification of a specific segment of genetic code (DNA or RNA), allowing for detection and quantification of specific gene sequences.
                            <br>
                            <br>
                            <b>Rolling average:</b>
                                      A calculation used to continuously estimate a daily average within a time-series dataset based on a defined period of time (e.g. 7-days). The purpose of a rolling average is to smooth out data points over a specific amount of time by taking the average of a specific number of data points to help with interpretation. 
                            <br>
                            <br>
                            <b>Respiratory Syncytial Virus or RSV:</b>
                                      A common respiratory virus that usually causes mild, cold-like symptoms. Most people recover in a week or two, but RSV can be serious. Infants and older adults are more likely to develop severe RSV and need hospitalization."), tags$a(href="https://www.cdc.gov/rsv/index.html", "(CDC)"),                  
        HTML("
                            <br>
                            <br>
                            <b>SARS-CoV-2 or Severe Acute Respiratory Syndrome Coronavirus-2:</b>
                                      The virus that causes COVID-19."), tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/index.html", "(CDC)"),                  
        HTML("
                            <br>
                            <br>
                            <b>Sewershed:</b>
                                       An area of land representing the catchment area for a sewer system that drains into a wastewater treatment plant; this area is indicative of the population contributing to that particular sewershed. 
                            <br>
                            <br>                            
                            <b>Statistical significance:</b>
                                      Using statistical analyses, statistical significance is a determination that the results in the data are not explainable by chance alone but can instead be attributed to a specific cause.
                            <br>
                            <br>
                            <b>Whole Genome Sequencing:</b>
                                      A laboratory procedure that determines the order of nucleotide bases in the genome of an organism. 
                            <br>
                            <br>
                            <b>WWTP:</b>
                                      Wastewater treatment plant.
                            <br>
                            <br>
                 
                 
                 
                 
                <br>"))
      
    }
    else {
      tags$div("")
      
    }
  })
  
  
  
  
  
  
  
}  
# run app
#shinyApp(ui, server)
#if(interactive())shinyApp(ui=ui,server=server)
shinyApp(ui = ui, server = server)
