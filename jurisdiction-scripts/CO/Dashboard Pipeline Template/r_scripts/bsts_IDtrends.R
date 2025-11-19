

## create adjusted system data
SystemData_bsts <- SystemData %>%
  select(wwtp_name, county_names, population_served, capacity_mgd, lpha1, lpha2, lpha3) %>%
  right_join(IDB_masterlist, by = c("wwtp_name")) %>%
  filter(pcr_target == "sars-cov-2")

             
#Recruit all data for sewershed and location to categorize LPHAs
ww_summ_pop <- read.csv("output/working data files/BSTS_Prep_Sars-CoV-2.csv") %>%
  mutate(`New Cases Utility` = NA,
         `New Hospitalizations Utility` = NA) %>%
  select(Utility, Date, `SARS.2.Copies.L..uncorrected.`,
         `New Cases Utility`, 
         `New Hospitalizations Utility`,
         lab_phase) %>%
  dplyr::rename(utility = Utility,
                measure_date = Date,
                new_cases_utility = `New Cases Utility`,
                new_hosp_utility = `New Hospitalizations Utility`,
                sars = `SARS.2.Copies.L..uncorrected.`) %>%
  left_join(SystemData_bsts, by = c("utility" = "wwtp_name"), relationship = "many-to-many") %>%
  mutate(measure_date = lubridate::as_date(measure_date, format="%Y-%m-%d")) %>%
  filter(lab_phase == "Lab Phase 3") %>%
  select(-lab_phase)



source("r_scripts/prod_bsts_lm_utility.R")


trend_out <- read_csv(paste0("output/trends/bsts_output/utility_bsts_lm_", today(), "_21d.csv"), 
                      col_types = cols(lpha3 = col_character())) #fixing an issue with lpha3 turning into boolean and giving a warning

#merge back with ww_summ_pop 
ww_trend_final <- ww_summ_pop %>%
  left_join(trend_out %>% 
              dplyr::select(utility,measure_date,classification,slope,p_val#, #flow_rate
                            ),by = c("measure_date", "utility"), relationship = "many-to-many")



# extract values for each utility's two most recent samples
slice1 <- ww_trend_final %>%
  arrange(utility, desc(measure_date))%>%
  filter(is.na(sars)==FALSE)%>%
  group_by(utility)%>%
  slice_head(n=1) %>%
  mutate(classification1 = classification, 
         measure_date1 = measure_date) %>%
  select(utility, measure_date1, classification1)


slice2 <- ww_trend_final %>%
  arrange(utility, desc(measure_date)) %>%
  filter(is.na(sars)==FALSE) %>%
  group_by(utility) %>%
  slice_head(n=2)%>%
  slice_tail(n=1)%>%
  mutate(classification2=classification, 
         measure_date2 = measure_date)%>%
  select(utility, measure_date2, classification2)

# set cut-off date for recent samples - 15 days
twoweekperiod <- today()-ddays(x=16)

slicejoin <- slice1 %>%
  full_join(slice2, by="utility")

trendsclassified <- slicejoin %>%
  mutate(recenttrend = case_when(
    measure_date1<=twoweekperiod ~ "Insufficient Data", 
    classification1=="Increasing" && classification2=="Increasing" ~ "Steady Increase", 
    classification1=="Decreasing" && classification2=="Decreasing" ~ "Steady Decrease", 
    classification1=="Increasing" && classification2=="Decreasing" ~ "Decrease -> Increase", #I haven't seen these, but added this category in case of quick change in trend
    classification1=="Decreasing" && classification2=="Increasing" ~ "Increase -> Decrease", #I haven't seen these, but added this category in case of quick change in trend
    classification1=="Increasing" || classification2== "Increasing" ~ "Increase", 
    classification1=="Decreasing" || classification2== "Decreasing" ~ "Decrease", 
    classification1=="Plateau" && classification2 == "Plateau" ~"Plateau", 
    TRUE ~ "Insufficient Data"))


Active_utilities_list <- IDB_masterlist %>%
  filter(Surv_System == "SSS") %>% #SSS only utilities
  filter(participation_status == "Active")


public_utilities_list <- Active_utilities_list %>%
  filter(public_repository == "Publish") 


#trendsclassified <- trendsclassified[(trendsclassified$utility %in% Inactive_utilities_list), ]

trendsclassified <- trendsclassified %>%
  filter(utility %in% public_utilities_list$wwtp_name)

trendsclassified$recenttrend <- factor(trendsclassified$recenttrend, levels = c("Steady Increase", 
                                                                                "Increase", 
                                                                                "Plateau", 
                                                                                "Decrease", 
                                                                                "Steady Decrease", 
                                                                                "Insufficient Data"))

  # quick summary
  trendsclassified %>%
    group_by(recenttrend) %>%
    summarise(count = n()) %>%
    complete(recenttrend) %>%
    mutate(count = coalesce(count, 0))  #changes count NAs to 

trendoutput <- trendsclassified %>%
  select(utility, recenttrend)


# write trends
write.csv(trendoutput, paste0("output/trends/bsts_output/wwweeklytrend_", Sys.Date(), "_21d.csv"), row.names=FALSE)







#####################################
###       Trend Summary Sheet     ###
#####################################

#write trend summary for monthly submitters meeting
Style_SteadyIncrease <- createStyle(fgFill="firebrick", fontColour = "white")
Style_Increase <- createStyle(fgFill="indianred", fontColour = "white")
Style_Plateau <- createStyle(fgFill="mediumpurple3", fontColour = "white")
Style_Decrease <- createStyle(fgFill="steelblue", fontColour = "white")
Style_SteadyDecrease <- createStyle(fgFill="darkblue", fontColour = "white")
Style_Insufficient <- createStyle(fgFill="gray20", fontColour = "white")
Style_Inactive <- createStyle(fgFill="black", fontColour = "white")
Style_noborder <- createStyle(borderColour = "white", border = "TopBottomLeftRight", borderStyle = "thick")

trendcat <- data.frame(recenttrend = c("Steady Increase", 
                                        "Increase", 
                                        "Plateau", 
                                        "Decrease", 
                                        "Steady Decrease", 
                                        "Insufficient Data", 
                                        "Inactive Utility"),
                       utility = rep(NA, 7))

trendoutput_summary <- trendoutput %>%
  rbind(trendcat) %>%
  #reshape2::dcast(utility ~ recenttrend, value.var = "utility") %>%
  reshape2::dcast(utility ~ recenttrend, value.var = "utility", fun.aggregate = function(x) paste(x, collapse = ", ")) %>%
  filter(!is.na(utility)) %>%
  select(-utility) %>%
  select(c(`Steady Increase`, `Increase`, `Plateau`, `Decrease`, `Steady Decrease`, `Insufficient Data`, `Inactive Utility`)) %>%
  mutate_all(~ na_if(., "")) %>%
  arrange_all(.by_column = TRUE, na.last = "last")

# Function to remove NA and shift column up
shift_column_up <- function(column) {
  non_na_values <- column[!is.na(column)]
  length_diff <- length(column) - length(non_na_values)
  c(non_na_values, rep(NA, length_diff))
}

# Loop through each column and apply the function
for (col in names(trendoutput_summary)) {
  trendoutput_summary[[col]] <- shift_column_up(trendoutput_summary[[col]])
}





RecentTrends_Workbook <- createWorkbook()
addWorksheet(RecentTrends_Workbook, sheetName = today()) 
addStyle(RecentTrends_Workbook, today(), Style_SteadyIncrease, rows = 2, cols = 2)
addStyle(RecentTrends_Workbook, today(), Style_Increase, rows = 2, cols = 3)
addStyle(RecentTrends_Workbook, today(), Style_Plateau, rows = 2, cols = 4)
addStyle(RecentTrends_Workbook, today(), Style_Decrease, rows = 2, cols = 5)
addStyle(RecentTrends_Workbook, today(), Style_SteadyDecrease, rows = 2, cols = 6)
addStyle(RecentTrends_Workbook, today(), Style_Insufficient, rows = 2, cols = 7)
addStyle(RecentTrends_Workbook, today(), Style_Inactive, rows = 2, cols = 8)
addStyle(RecentTrends_Workbook, today(), Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 1)
addStyle(RecentTrends_Workbook, today(), Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(RecentTrends_Workbook, today(), Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 2)
addStyle(RecentTrends_Workbook, today(), Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 3)
addStyle(RecentTrends_Workbook, today(), Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 4)
addStyle(RecentTrends_Workbook, today(), Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 5)
addStyle(RecentTrends_Workbook, today(), Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 6)
addStyle(RecentTrends_Workbook, today(), Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 7)
addStyle(RecentTrends_Workbook, today(), Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 8)
addStyle(RecentTrends_Workbook, today(), Style_noborder, rows = 3:nrow(trendoutput_summary), cols = 9)
writeData(RecentTrends_Workbook, today(), trendoutput_summary, startCol = 2,startRow = 2) 

#write workbook
saveWorkbook(RecentTrends_Workbook, paste("output/trends/bsts_output/trend_summary_", today(), "_21d.xlsx"), overwrite=TRUE) 

#write trends to K drive
#remove non-public data

trend_folder <- "output/trends/bsts_output/"
public_trendoutput <- trendoutput %>%
  filter(utility %in% public_utilities_list$wwtp_name)
  
write.xlsx(public_trendoutput, paste0(trend_folder,"wwweeklytrend_", Sys.Date(), ".xlsx"), rowNames=FALSE )



