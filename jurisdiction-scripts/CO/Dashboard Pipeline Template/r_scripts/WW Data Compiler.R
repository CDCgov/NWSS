
print("### Running WW Data Compiler ###")


##############################################################################################
###                           Import Historic Lab Results                                  ###
##############################################################################################

#Read in any historic lab results saved from previous pipeline code
print("Loading historic wastewaterdata")

##
OutputForSavingCDPHE <- read.csv( "import/viral_conc_results/OutforSavingCDPHE_Historic.csv") %>%
  select(-c(X)) %>%
  mutate(test_result_date = format(as.Date(test_result_date, format = '%m/%d/%Y'), '%m/%d/%Y')) %>%
  mutate(sample_collect_date = format(as.Date(sample_collect_date, format = '%m/%d/%Y'), '%m/%d/%Y')) %>%
  mutate(sample_collect_time = case_when(grepl("^-?\\d+$", sample_collect_time) ~ suppressWarnings(format(
                                as.POSIXct(as.numeric(sample_collect_time), origin = "1970-01-01", tz = "UTC"),
                                "%H:%M"
                              )),
                              grepl("^\\d{1,2}:\\d{2}$", sample_collect_time) ~ format(strptime(sample_collect_time, format = "%H:%M"), "%H:%M"),
                              TRUE ~ NA_character_)) 



##############################################################################################
###                         Import 35mL CDPHE Lab Results                                  ###
##############################################################################################

###add in 35mL data
print("Loading CDPHE 35mL data")
CDPHEfiles_35mL <- list.files("import/viral_conc_results/") %>%
  data.frame() %>%
  `colnames<-`("filename") %>%
  mutate(extension = file_ext(filename)) %>%
  filter(extension %in% "xlsx") %>%
  mutate(date = str_split_fixed(filename,pattern="_",n=2)) %>%
  mutate(date = ymd(date[,1])) 

CDPHEData_35mL <- data.frame(stringsAsFactors = FALSE) 

for(i in 1:nrow(CDPHEfiles_35mL)){
  selFilename = CDPHEfiles_35mL[i,1]
  selDate = CDPHEfiles_35mL[i,3]
  
  print(selFilename)
  
  try(CDPHEData_35mL <-read_excel(paste("import/viral_conc_results/",
                                        selFilename, sep=""),
                                  sheet = "Sheet2") %>%
        dplyr::mutate(zipcode = as.character(zipcode)) %>%  
        dplyr::mutate(population_served = as.character(population_served)) %>%  
        dplyr::mutate(test_result_date = as.character(test_result_date)) %>%
        dplyr::mutate(tot_conc_vol = as.character(tot_conc_vol)) %>% 
        mutate(sample_collect_time = as.POSIXct(sample_collect_time, format = "%I:%M:%S %p")) %>%
        dplyr::mutate(ntc_amplify = case_when(ntc_amplify == "N" ~ "no", ntc_amplify == "Y" ~ "yes", TRUE ~ ntc_amplify)) %>%
        dplyr::mutate(filename = selFilename) %>%
        bind_rows(CDPHEData_35mL))
}


CDPHEData_35mL <- CDPHEData_35mL %>%
  mutate(pcr_gene_target = if_else(is.na(pcr_gene_target), pcr_target, pcr_gene_target)) %>%
  mutate(sample_collect_date = as.Date(sample_collect_date, format = "%m/%d/%Y")) %>%
  mutate(test_result_date =  format(as.Date(test_result_date, format = '%Y-%m-%d'), '%m/%d/%Y')) %>%
  mutate(pcr_target_units = "copies/L wastewater",
         pasteurized = "no", 
         pretreatment = "no") %>%
  mutate(county_names = case_when(county_names == "NA" ~ NA, TRUE ~ county_names))


CDPHEData_35mL$sample_collect_date <- CDPHEData_35mL$sample_collect_date %>%
  format(format = "%m/%d/%Y") %>%
  as.character()
  
CDPHEData_35mL$sample_collect_time <-  CDPHEData_35mL$sample_collect_time %>%
  format("%H:%M") %>%
  as.character()


#Add missing county data
SystemDataCounties <- SystemData %>%
  select(wwtp_name, FIPSc, zipcode, population_served, epaid, capacity_mgd) %>%
  dplyr::mutate(population_served = as.character(population_served)) %>%
  dplyr::mutate(zipcode = as.character(zipcode))

CDPHEData_35mL %<>%
  left_join(SystemDataCounties, by = "wwtp_name") %>%
  mutate(
    zipcode = zipcode.y,
    population_served = population_served.y,
    epaid = epaid.y,
    capacity_mgd = capacity_mgd.y,
    county_names = FIPSc
  ) %>%
  select(
    -ends_with(".x"),
    -zipcode.y,
    -population_served.y,
    -epaid.y,
    -capacity_mgd.y,
    -FIPSc
  ) %>%
  filter(!is.na(county_names))


CDPHERows_35mL <- nrow(CDPHEData_35mL) 

CDPHEData_35mL %<>%
  # added fields
  mutate(analysis_ignore = character(length = CDPHERows_35mL),
         major_lab_method = character(length = CDPHERows_35mL), 
         major_lab_method_desc = character(length = CDPHERows_35mL), 
         other_norm_conc = numeric(length = CDPHERows_35mL), 
         other_norm_unit = character(length = CDPHERows_35mL), 
         qc_ignore = character(length = CDPHERows_35mL), 
         solids_separation = character(length = CDPHERows_35mL),
         validated_method = logical(length = CDPHERows_35mL)) %>% 
  # removed fields
  select(-hum_frac_phys_conc, 
         -hum_frac_phys_unit, 
         -hum_frac_target_phys, 
         -hum_frac_target_phys_ref,
         -reporting_state, 
         -state, 
         -pH)


print("Outputting wastewater data")

OutputForSavingCDPHE_35mL <- CDPHEData_35mL %>%
  mutate(test_result_date =  format(as.Date(test_result_date, format = '%m/%d/%Y'), '%m/%d/%Y')) %>%
  mutate(sample_collect_date =  format(as.Date(sample_collect_date, format = '%m/%d/%Y'), '%m/%d/%Y')) %>%
  #re-order variable names
  select("reporting_jurisdiction", "county_names", "other_jurisdiction", "zipcode", "population_served", 
         "sewage_travel_time", "sample_location", "sample_location_specify", "institution_type", "epaid", 
         "wwtp_name", "wwtp_jurisdiction", "capacity_mgd", "industrial_input", "stormwater_input", "influent_equilibrated", 
         "sample_type", "composite_freq", "sample_matrix", "collection_storage_time", "collection_storage_temp", 
         "pasteurized", "pretreatment", "pretreatment_specify", "solids_separation", "concentration_method", "extraction_method", 
         "pre_conc_storage_time", "pre_conc_storage_temp", "pre_ext_storage_time", "pre_ext_storage_temp", "tot_conc_vol",
         "ext_blank", "rec_eff_target_name", "rec_eff_spike_matrix", "rec_eff_spike_conc", "pcr_target", 
         "pcr_gene_target", "pcr_gene_target_ref", "pcr_type", "lod_ref", "hum_frac_target_mic", "hum_frac_target_mic_ref",
         "hum_frac_target_chem", "hum_frac_target_chem_ref", "other_norm_name", "other_norm_ref", "quant_stan_type", 
         "stan_ref", "inhibition_method", "num_no_target_control", "sample_collect_date", "sample_collect_time", 
         "flow_rate", "ph", "conductivity", "tss", "collection_water_temp", "equiv_sewage_amt", "sample_id", "lab_id", 
         "qc_ignore", "analysis_ignore", "validated_method","test_result_date", "pcr_target_units", "pcr_target_avg_conc", "pcr_target_std_error",
         "pcr_target_cl_95_lo", "pcr_target_cl_95_up", "pcr_target_below_lod", "lod_sewage", "ntc_amplify", "rec_eff_percent", 
         "inhibition_detect", "inhibition_adjust", "hum_frac_mic_conc", "hum_frac_mic_unit", "hum_frac_chem_conc", 
         "hum_frac_chem_unit", "other_norm_conc", "other_norm_unit", "quality_flag", "major_lab_method", "major_lab_method_desc", "qc_internal")


#Rename variables to match NWSS 
OutputForSavingCDPHE_35mL <- OutputForSavingCDPHE_35mL %>%
  mutate(
    pcr_target = case_when(pcr_target == "Inf_A-MP" ~ "Inf_A",
                                                             pcr_target == "Inf_B-MP" ~ "Inf_B",
                                                             pcr_target == "PanRSV-MP" ~ "PanRSV",
                                                             pcr_target == "SC2" ~ "sars-cov-2",
                                                             pcr_target == "Sars-CoV-2 - MP" ~ "sars-cov-2",
                                                             pcr_target == "Sars-CoV-2-MP" ~ "sars-cov-2",
                                                             pcr_target == "N2-MP" ~ "sars-cov-2",
                                                             pcr_target == "N2" ~ "sars-cov-2",
                                                             pcr_target == "Verily_H1" ~ "H1",
                                                             pcr_target == "flu_H1" ~ "H1",
                                                             pcr_target == "Verily_H3" ~ "H3",
                                                             pcr_target == "flu_H3" ~ "H3",
                                                             pcr_target == "Verily_H5" ~ "H5",
                                                             pcr_target == "flu_H5" ~ "H5",
                                                             TRUE ~ pcr_target)) 

#remove duplicates based on test_result_date
OutputForSavingCDPHE_35mL <- OutputForSavingCDPHE_35mL %>%
  group_by(pcr_target, wwtp_name, sample_collect_date, sample_id) %>%
  arrange(pcr_gene_target, wwtp_name, sample_collect_date, sample_id, desc(test_result_date)) %>%
  slice_head(n=1) %>%
  ungroup()





##############################################################################################
###                                       Combine Data                                     ###
##############################################################################################

# combined data together
OutputForSaving <- rbind(OutputForSavingCDPHE, OutputForSavingCDPHE_35mL)

OutputForSaving <- OutputForSaving %>% 
  mutate(pcr_gene_target = case_when(pcr_target == "Inf_A" ~ "InfA1 and InfA2 combined",
                                    pcr_target == "Inf_B" ~ "InfB",
                                    TRUE ~ pcr_gene_target))

#Rename variables to match NWSS 
OutputForSaving <- OutputForSaving %>%
  mutate(
    pcr_target = case_when(pcr_target == "Inf_A-MP" ~ "Inf_A",
                           pcr_target == "Inf_B-MP" ~ "Inf_B",
                           pcr_target == "PanRSV-MP" ~ "PanRSV",
                           pcr_target == "SC2" ~ "sars-cov-2",
                           pcr_target == "Sars-CoV-2 - MP" ~ "sars-cov-2",
                           pcr_target == "Sars-CoV-2-MP" ~ "sars-cov-2",
                           pcr_target == "N2-MP" ~ "sars-cov-2",
                           pcr_target == "N2" ~ "sars-cov-2",
                           pcr_target == "Verily_H1" ~ "H1",
                           pcr_target == "flu_H1" ~ "H1",
                           pcr_target == "Verily_H3" ~ "H3",
                           pcr_target == "flu_H3" ~ "H3",
                           pcr_target == "Verily_H5" ~ "H5",
                           pcr_target == "flu_H5" ~ "H5",
                           TRUE ~ pcr_target)) %>%
  mutate(
    pcr_gene_target = case_when(
      pcr_gene_target == "H5" | pcr_gene_target == "flu_H5" | pcr_gene_target == "h5n1" | pcr_gene_target == "H5N1" | pcr_gene_target == "verily_h5" | pcr_gene_target == "verily_H5" |pcr_gene_target == "Verily_H5" ~ "INFA_H5 (Verily)",
      pcr_gene_target == "H3" | pcr_gene_target == "flu_H3" | pcr_gene_target == "verily_h3" | pcr_gene_target == "Verily_H3" ~ "FLUAV A H3_Cv1",
      pcr_gene_target == "H1" | pcr_gene_target == "flu_H1" | pcr_gene_target == "verily_h1" | pcr_gene_target == "Verily_H1" ~ "FLUAV A H1_Cv1",
      pcr_gene_target == "N[517, 83]" ~ "RSV-A and RSV-B combined",
      pcr_gene_target == "sc2" | pcr_gene_target == "m1, ns2" | pcr_gene_target == "M1, NS2" | pcr_gene_target == "SC2" ~ "N2",
      TRUE ~ pcr_gene_target)) %>%
  mutate(
    stan_ref = case_when(
      pcr_target == "sars-cov-2" ~ "(2019-nCoV/USA-WA1/2020)",
      pcr_target == "Inf_A" ~ "MR-541",
      pcr_target == "Inf_B" ~ "MR-541",
      pcr_target == "PanRSV" ~ "NATtrol RSV positive Control NATRSV-6C, Zeptometrix",
      pcr_target == "H5" ~ "OR051630.1, OR051629.1",
      pcr_target == "H3" ~ "NC_007366, NC_007367, NC_007368, NC_007369, NC_007370, NC_007371, NC_007372, NC_007373",
      pcr_target == "H1" ~ "NC_026431, NC_026432, NC_026433, NC_026434, NC_026435, NC_026436, NC_026437, NC_026438",
      TRUE ~ pcr_gene_target))

#update rec_eff_target_name to match NWSS Data Dictionary
OutputForSaving %<>% mutate(rec_eff_target_name = case_when(rec_eff_target_name == "MHV" ~ "MHV (PREvalence)",  
                                                            TRUE ~ rec_eff_target_name))
#correct date format
OutputForSaving <- OutputForSaving %>%
  mutate(sample_collect_date = as.Date(sample_collect_date, format = "%m/%d/%Y")) %>%
  mutate(test_result_date = as.Date(test_result_date, format = "%m/%d/%Y"))

#adjust sample ids to upper
OutputForSaving <- OutputForSaving %>%
  mutate(sample_id = toupper(sample_id))

  
#adjust test_result_date if comes before collection date
OutputForSaving %<>% mutate(test_result_date = case_when(test_result_date < sample_collect_date ~ sample_collect_date,  
                                                         TRUE ~ test_result_date))

OutputForSaving <- OutputForSaving %>%
  mutate(institution_type = "not institution specific")

#Fix county_names issues
SystemDataCountiesSubset <- SystemDataCounties %>%
  select(wwtp_name, FIPSc)

OutputForSaving <- OutputForSaving %>%
  left_join(SystemDataCountiesSubset, by = "wwtp_name") %>%
  mutate(county_names = ifelse(!is.na(FIPSc), FIPSc, NA)) %>%
  select(-FIPSc)

#clear "other..." columns to match NWSS Data Dictionary
OutputForSaving %<>% mutate(other_norm_conc = NA_real_, 
                            other_norm_name = NA_character_, 
                            other_norm_ref = NA_character_, 
                            other_norm_unit = NA_character_)


# Check sample_types and update source or code above if sample_types do not align with NWSS Data Dictionary
print(table(OutputForSaving$sample_type))

# Rename sample_location column per NWSS data dictionary
OutputForSaving$sample_location [OutputForSaving$sample_location=="primary influent"] <- "wwtp"

OutputForSaving$lod_ref [OutputForSaving$lod_ref=="pending"] <- "Bio-Rad. (2021). Qualitative assay for use on the QX200(tm) and QXDx(tm) Droplet Digital(tm) PCR Systems: Instructions for Use. Retrieved from https://www.fda.gov/media/137579/download"
OutputForSaving$concentration_method [OutputForSaving$extraction_method=="kingfisher MAGMax viral enrichment + extraction"] <- "thermo magmax wastewater ultra nucleic acid isolation kit"
OutputForSaving$extraction_method [OutputForSaving$extraction_method=="kingfisher MAGMax viral enrichment + extraction"] <- "thermo magmax wastewater ultra nucleic acid isolation kit"

#Adjusting the Flu pcr_target values for the flu samples
OutputForSaving$pcr_target[OutputForSaving$pcr_target=="Inf_A"] <- "FLUAV"
OutputForSaving$pcr_target[OutputForSaving$pcr_target=="Inf_B"] <- "FLUBV"

#Adjusting the Flu PCR_Gene_target column
OutputForSaving$pcr_gene_target[OutputForSaving$pcr_target == 'FLUAV' & OutputForSaving$pcr_gene_target == 'M1, NS2'] <- 'InfA1 and InfA2 combined' 
OutputForSaving$pcr_gene_target[OutputForSaving$pcr_target == 'FLUBV' & OutputForSaving$pcr_gene_target == 'M1, NS2'] <- 'InfB'


unique(OutputForSaving$pcr_target)
unique(OutputForSaving$wwtp_name)



##############################################################################################
###                         QC Report: report_failed_QC                                    ###
##############################################################################################

#remove any samples that didn't pass internal QC, print report
report_failed_QC <- OutputForSaving %>%
  filter(qc_internal == "FAIL" & analysis_ignore == "no") 

if (nrow(report_failed_QC) == 0) {
  report_failed_QC <- data.frame(
    reporting_jurisdiction = "no data")
}

write.csv(report_failed_QC, paste0("output/validation_reports/failed_qc_internal_", Sys.Date(), ".csv"))

#remove failed QC data from pipeline
OutputForSaving <- OutputForSaving %>%
  anti_join(report_failed_QC)



##############################################################################################
###                                 Identify Lab Phases                                    ###
##############################################################################################

#label lab phases
lp2_first <- OutputForSaving %>%
  arrange(pcr_target, wwtp_name, sample_collect_date) %>%
  filter(grepl("thermo", extraction_method))%>%
  group_by(wwtp_name, pcr_target) %>%
  slice_head() %>%
  mutate(LP2Date = sample_collect_date) %>%
  select(wwtp_name, LP2Date) %>%
  mutate(LP2Date =  as.Date(LP2Date, format = '%m/%d/%y'))

lp3_first <- OutputForSaving %>%
  arrange(pcr_target, wwtp_name, sample_collect_date) %>%
  filter(tot_conc_vol == 35 & sample_collect_date > "2024-06-23") %>% #lab phase 3 started 6/23/2023
  group_by(wwtp_name, pcr_target) %>%
  slice_head() %>%
  mutate(LP3Date = sample_collect_date) %>%
  select(wwtp_name, LP3Date) %>%
  mutate(LP3Date =  as.Date(LP3Date, format = '%m/%d/%y'))

OutputForSaving <- OutputForSaving %>%
  mutate(sample_collect_date =  as.Date(sample_collect_date, format = '%m/%d/%y')) %>%
  left_join(lp2_first, by=c("wwtp_name", "pcr_target")) %>%
  mutate(lab_phase = case_when(LP2Date > sample_collect_date ~ "Lab Phase 1", 
                               LP2Date <= sample_collect_date ~ "Lab Phase 2",
                               TRUE ~ "Lab Phase 1")) %>% # This accounts for all utils that stopped sampling before Lab Phase 2
  left_join(lp3_first, by=c("wwtp_name", "pcr_target")) %>%
  mutate(lab_phase = case_when(LP3Date <= sample_collect_date & tot_conc_vol == 35 ~ "Lab Phase 3", 
                               TRUE ~ lab_phase)) %>%
  select(-c(LP2Date, LP3Date))


######  Remove Duplicates
OutputForSaving <- OutputForSaving %>%
  #grouping because want pcr_targets to be separated before slicing; sample id included b/c don't want to slice out duplicate sample ids that are for multiple pathogens
  group_by(pcr_target, wwtp_name, sample_collect_date, sample_id, lab_phase) %>% 
  arrange(desc(test_result_date)) %>%
  slice_head(n=1) %>%
  ungroup()


#create major lab methods columns
OutputForSaving <- OutputForSaving %>%
  mutate(
    major_lab_method = case_when(
      lab_phase== 'Lab Phase 1' & lab_id== 'ColoStateUni' ~ '1',
      lab_phase== 'Lab Phase 1' & lab_id== 'CDPHE' ~ '2',
      lab_phase== 'Lab Phase 2' & lab_id== 'CDPHE' ~ '3',
      lab_phase== 'Lab Phase 3' & lab_id== 'CDPHE' ~ '4'
    ))  %>%
  mutate(
    major_lab_method_desc = case_when(
      major_lab_method == '1' ~ "Samples processed at Colorado State University, using innovaprep ultrafiltration concentration methods and Qiagen QiaAmp buffers with Epoch Columns extraction methods.",
      major_lab_method == '2' ~ "Samples processed at Colorado Department of Health State Lab, using innovaprep ultrafiltration concentration methods and EZ-1 extraction methods.",
      major_lab_method == '3' ~ "Samples processed at Colorado Department of Health State Lab, using Ceres Nanotrap enrichment + Kingfisher MagMax extraction methods. Sample starting volume is 10mL of wastewater.",
      major_lab_method == '4' ~ "Samples processed at Colorado Department of Health State Lab, using Ceres Nanotrap enrichment + Kingfisher MagMax extraction methods. Sample starting volume is 35mL of wastewater."
    ))


##############################################################################################
###                               Output Compiled Results                                  ###
##############################################################################################

#write copy of compiled results before too much manipulation (this helps with troubleshooting)
write.csv(OutputForSaving, paste0("output/compiled_data/Compiled_Results_",Sys.Date()," .csv"))



##############################################################################################
###         Adjust rec_eff_percent / rec_eff_spike_conc / pcr_target_below_lod             ###
##############################################################################################

##Missing rec_eff_percent values
rec_eff_percent <- read.csv("reference_sheets/Recovery_efficiency_additions.csv") %>%
  mutate(sample_id = toupper(sample_id)) %>%
  rename(adjusted_rec_eff_percent = rec_eff_percent)

OutputForSaving <- OutputForSaving %>%
  left_join(rec_eff_percent, by = "sample_id", relationship = "many-to-many") %>%
  mutate(rec_eff_percent = adjusted_rec_eff_percent) %>%
  select(-adjusted_rec_eff_percent)

## Missing rec_eff_spike_conc values
rec_eff_spike_conc <- read.csv("reference_sheets/missing_rec_eff_spike_conc.csv") %>%
  mutate(sample_id = toupper(sample_id)) %>%
  rename(adjusted_rec_eff_spike_conc = rec_eff_spike_conc) %>%
  select(sample_id, adjusted_rec_eff_spike_conc)

OutputForSaving <- OutputForSaving %>%
  left_join(rec_eff_spike_conc, by = "sample_id", relationship = "many-to-many") %>%
  mutate(rec_eff_spike_conc = adjusted_rec_eff_spike_conc) %>%
  select(-adjusted_rec_eff_spike_conc)

#pcr_target_below_lod adjustments
OutputForSaving <- OutputForSaving %>%
  mutate(pcr_target_below_lod = ifelse(pcr_target_below_lod == 'yes', pcr_target_below_lod, 'no'))%>%
  mutate(pcr_target_below_lod = ifelse(is.na(pcr_target_below_lod), 'no', pcr_target_below_lod))


##############################################################################################
###                              QC Report: Missing Flow Rates                             ###
##############################################################################################
# Read in missing flow rate dataset 
flowrates <- read_csv("reference_sheets/Missing Flowrates.csv") %>%
  rename(flow_rate_addon = flow_rate, sample_type_addon = sample_type) %>%
  select(flow_rate_addon, sample_type_addon, sample_id)

ignoreflowrates <- read_excel("reference_sheets/missing_flowrates_to_ignore.xlsx") %>%
  select(sample_id)

# Adapt 'Output for Saving' to include updated flow rate values
OutputForSavingFlow <- OutputForSaving %>%
  select(wwtp_name, sample_collect_date, flow_rate, sample_id, pcr_target) %>%
  rename(flow_rate_orig = flow_rate) %>%
  left_join(flowrates, by=c("sample_id"), relationship = "many-to-many") %>%
  mutate(flow_rate_status = case_when(
    is.na(flow_rate_addon) ~ "No Update",
    TRUE ~ "Update Flow Rate")) %>%
  mutate(flow_rate = case_when(
    flow_rate_status == "Update Flow Rate" ~ flow_rate_addon, 
    flow_rate_status == "No Update" ~ flow_rate_orig)) %>%
  filter(is.na(wwtp_name)==FALSE) %>%
  select(sample_id, flow_rate, flow_rate_status) 

OutputForSavingSampleType <- OutputForSaving %>%
  select(wwtp_name, sample_collect_date, sample_type, sample_id, pcr_target)%>%
  rename(sample_type_orig = sample_type) %>%
  left_join(flowrates, by=c("sample_id"), relationship = "many-to-many") %>%
  mutate(sample_type_status = case_when(
    is.na(sample_type_addon) ~ "No Update",
    TRUE ~ "Update Sample Type")) %>%
  mutate(sample_type = case_when(
    sample_type_status == "Update Sample Type" ~ sample_type_addon, 
    sample_type_status == "No Update" ~ sample_type_orig)) %>%
  filter(is.na(wwtp_name)==FALSE) %>%
  select(sample_id, sample_type, sample_type_status) 

OutputForSaving %<>% 
  select(-flow_rate, -sample_type) %>%
  left_join(OutputForSavingFlow, by="sample_id", relationship = "many-to-many") %>%
  group_by(pcr_target, wwtp_name, sample_collect_date, sample_id, lab_phase) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  left_join(OutputForSavingSampleType, by="sample_id", relationship = "many-to-many") %>%
  group_by(pcr_target, wwtp_name, sample_collect_date, sample_id, lab_phase) %>%
  slice_head(n=1)

# print report with remaining missing flow rates
FlowReportName = paste0("output/validation_reports/Flow_Rate_Check_Report_",Sys.Date()," .csv")
MissingFlowRateReport <- OutputForSaving %>%
  mutate(flow_check = case_when(is.na(flow_rate) ~ "check flow", 
                                flow_rate > 80 ~ "check flow", 
                                TRUE ~ "flow OK")) %>%
  mutate(sample_type_check = case_when(is.na(sample_type) ~ "check sample type",
                                       sample_type == "not specified" ~ "check sample type",
                                       TRUE ~ "sample type OK")) %>%
  filter(flow_check == "check flow" | sample_type_check == "check sample type", !is.na(wwtp_name)) %>%
  select(wwtp_name, sample_collect_date, flow_rate, sample_type, sample_id, lab_id, test_result_date, pcr_target, flow_check, sample_type_check) 

MissingFlowRateReport <- MissingFlowRateReport %>% 
  filter(!sample_id %in% ignoreflowrates$sample_id) %>%
  arrange(wwtp_name, sample_collect_date) 

if (nrow(MissingFlowRateReport) == 0) {
  MissingFlowRateReport <- data.frame(
    lab_phase = "no data")
}

write_csv(MissingFlowRateReport, FlowReportName, na = "",quote="all")



##############################################################################################
###                                        Adjust sample_type                             ###
##############################################################################################
# Update naming conventions for sample_type
OutputForSaving %<>%
  mutate(sample_type = case_when(sample_type == "24-hr flow-weighted composite\r\n" ~ "24-hr flow-weighted composite", 
                                 sample_type == "composite" ~ "24-hr flow-weighted composite", 
                                 sample_type == "Composite" ~ "24-hr flow-weighted composite", 
                                 sample_type == "Grab" ~ "grab", 
                                 sample_type == "Headworks Grab Sample" ~ "grab", 
                                 TRUE ~ sample_type))




##############################################################################################
###                              QC Report: ntc_amplify_report                             ###
##############################################################################################
#ntc_amplify check QC report
ntc_amplify_report <- OutputForSaving %>%
  select(pcr_target, wwtp_name, sample_collect_date, test_result_date, sample_id, ntc_amplify, pcr_target_below_lod, quality_flag) %>%
  filter(ntc_amplify != "no")

if (nrow(ntc_amplify_report) == 0) {
  ntc_amplify_report <- data.frame(
    pcr_target = "no data")
}

write_csv(ntc_amplify_report, paste0("output/validation_reports/ntc_amplify_report_", Sys.Date(), ".csv"))


##############################################################################################
###                               QC Report: Missing pcr_target                            ###
##############################################################################################
PCRTargetName = paste0("output/validation_reports/PCR_Target_Check_Report_",Sys.Date()," .csv")
MissingPCRTargetReport <- OutputForSaving %>%
  select(wwtp_name, sample_collect_date, test_result_date, sample_id, pcr_target, lab_id) %>%
  filter(is.na(pcr_target) | !(pcr_target %in% c(IDB_masterlist$pcr_target))) %>%
  arrange(wwtp_name, sample_collect_date)

# Check if the dataframe is empty, and if so, add a row with "no data"
if (nrow(MissingPCRTargetReport) == 0) {
  MissingPCRTargetReport <- data.frame(
    wwtp_name = "no data",
    sample_collect_date = NA,
    test_result_date = NA,
    sample_id = NA,
    pcr_target = NA,
    lab_id = NA
  )
}
  
write.csv(MissingPCRTargetReport, PCRTargetName)
  
#print number of rows with missing PCR target data
num_missingPCRTarget <- OutputForSaving %>%
  filter(is.na(pcr_target) | !(pcr_target %in% c(IDB_masterlist$pcr_target))) %>%
  nrow()

if(!num_missingPCRTarget == 0){
  print(paste("*************DO NOW!****************** Check the PCR_Target_Check_Report file in your output folder. There are ", num_missingPCRTarget, " rows of data with missing or incorrect PCR Targets",
              sep = ""))
}

##############################################################################################
###                              QC Report: extraction_method                              ###
##############################################################################################

###quality control check for missing extraction_method
ExtractionMethodName = paste0("output/validation_reports/ExtractionMethod_Check_Report_",Sys.Date()," .csv")
MissingExtractionMethodReport <- OutputForSaving %>%
  select(wwtp_name, sample_collect_date, test_result_date, sample_id, pcr_target, extraction_method, lab_id) %>%
  filter(is.na(extraction_method)) %>%
  arrange(wwtp_name, sample_collect_date)

if (nrow(MissingExtractionMethodReport) == 0) {
  MissingExtractionMethodReport <- data.frame(
    wwtp_name = "no data"
  )
}

write.csv(MissingExtractionMethodReport, ExtractionMethodName)

#print number of rows with missing PCR target data
num_missingExtractionMethod <- OutputForSaving %>%
  filter(is.na(extraction_method)) %>%
  nrow()

if(!num_missingExtractionMethod == 0){
  print(paste("*************DO NOW!****************** Check the ExtractionMethod_Check_Report file in your output folder. There are ", num_missingExtractionMethod, " rows of data with missing the Extraction Method",
              sep = ""))
}


##############################################################################################
###                             QC Report: ND Values Check Report                          ###
##############################################################################################  

# Create collection of ND values and change to half the total of the LOD as per CDC methodology guidelines. 
# Some historical ND viral concentration values were manually changed to 0. Logic developed to address these, and
# may need to be updated to handle CSU vs. CDPHE data differently.
# Print report with the original concentration values that will be updated with case_when below
NDReportName = paste0("output/validation_reports/ND_Value_Check_Report_",Sys.Date()," .csv")
NDValueModify <- OutputForSaving %>%
  group_by(pcr_target, wwtp_name) %>%
  filter((is.na(pcr_target_avg_conc) | pcr_target_avg_conc ==0) & is.na(pcr_target_cl_95_lo)) %>%
  dplyr::select(wwtp_name, sample_id, sample_collect_date, test_result_date, pcr_target_avg_conc, pcr_target_std_error, 
                pcr_target_cl_95_lo, pcr_target_cl_95_up, lab_id, pcr_target) %>%
  filter(!is.na(wwtp_name)) %>%
  mutate(sample_collect_date = as.Date(sample_collect_date, format = '%m/%d/%y')) %>%
  arrange(wwtp_name, sample_collect_date)

if (nrow(NDValueModify) == 0) {
  NDValueModify <- data.frame(
    wwtp_name = "no data"
  )
}

write.csv(NDValueModify, NDReportName)



##############################################################################################
###                                          Adjust LOD/LOQ                                ###
##############################################################################################
##update quality flag
OutputForSaving <- OutputForSaving %>%
  mutate(quality_flag = case_when(grepl("Below LOQ", quality_flag) ~ "yes",
                                  grepl("Below LO", quality_flag) ~ "yes",
                                  grepl("Below LoQ", quality_flag) ~ "yes",
                                  grepl("Below LoD", quality_flag) ~ "yes",
                                  grepl("Below LoD,Below LoD,", quality_flag) ~ "yes",
                                  grepl("Below minimum", quality_flag) ~ "yes",
                                  grepl("below minimum", quality_flag) ~ "yes",
                                  grepl("Below Minimum", quality_flag) ~ "yes",
                                  grepl("RSD_Pass", quality_flag) ~ "no", #verified by lab this means no
                                  grepl("RSD_Pass,RSD_Pass,", quality_flag) ~ "no", #verified by lab this means no
                                  is.na(quality_flag) ~ "no", #as per KW/lab
                                  TRUE ~ quality_flag)) %>%
  mutate(pcr_target_below_lod = case_when(pcr_target_below_lod == "" ~ NA,
                                          pcr_target_below_lod == "E" ~ NA,
                                          pcr_target_below_lod == "NA" ~ NA,
                                          TRUE ~ pcr_target_below_lod)) %>%
  filter(!quality_flag == "RSD Above Limit") #remove as per lab

#create adjusted columns based on original (using original (with updates; see code) for NWSS 2_CDPHE; using adjusted for 3_CDPHE)
OutputForSaving <- OutputForSaving %>%
  mutate(pcr_target_avg_conc_adjusted = pcr_target_avg_conc,
         pcr_target_std_error_adjusted = pcr_target_std_error,
         pcr_target_cl_95_lo_adjusted = pcr_target_cl_95_lo,
         pcr_target_cl_95_up_adjusted = pcr_target_cl_95_up)

# update pcr_target_avg_conc if sample is below LOD or 0 or NA
OutputForSaving %<>%
  mutate(pcr_target_avg_conc_adjusted = case_when((is.na(pcr_target_avg_conc_adjusted) | pcr_target_avg_conc_adjusted == 0) & is.na(pcr_target_cl_95_lo_adjusted) & lab_phase %in% c("Lab Phase 1", "Lab Phase 2") ~ 2000,
                                                  pcr_target_below_lod == "yes" & lab_phase %in% c("Lab Phase 1", "Lab Phase 2") ~ 2000,
                                                  TRUE ~ pcr_target_avg_conc_adjusted))


OutputForSaving %<>%
  mutate(pcr_target_avg_conc_adjusted = case_when((is.na(pcr_target_avg_conc_adjusted) | pcr_target_avg_conc_adjusted == 0) & is.na(pcr_target_cl_95_lo_adjusted) & lab_phase == "Lab Phase 3" ~ 600,
                                                  pcr_target_below_lod == "yes" & lab_phase == "Lab Phase 3" ~ 600,
                                                  TRUE ~ pcr_target_avg_conc_adjusted))%>%
  filter(!is.na(wwtp_name))%>%
  select(-flow_rate_status, -sample_type_status)


#update quantitative results columns if concentration is <= LOQ
##LP 1 + 2
OutputForSaving <- OutputForSaving %>% 
  mutate(pcr_target_avg_conc_adjusted = case_when(pcr_target_avg_conc_adjusted <= 4000 & lab_phase %in% c("Lab Phase 1", "Lab Phase 2") ~ 2000, TRUE ~ pcr_target_avg_conc_adjusted)) %>% 
  mutate(pcr_target_avg_conc_adjusted = case_when(quality_flag == 'yes' & lab_phase %in% c("Lab Phase 1", "Lab Phase 2") ~ 2000, TRUE ~ pcr_target_avg_conc_adjusted)) %>%
  mutate(pcr_target_std_error_adjusted = case_when(pcr_target_avg_conc_adjusted == 2000 & lab_phase %in% c("Lab Phase 1", "Lab Phase 2") ~ 2000, TRUE ~ pcr_target_std_error_adjusted)) %>% 
  mutate(pcr_target_cl_95_lo_adjusted = case_when(pcr_target_avg_conc_adjusted == 2000 & lab_phase %in% c("Lab Phase 1", "Lab Phase 2") ~ 2000, TRUE ~ pcr_target_cl_95_lo_adjusted)) %>% 
  mutate(pcr_target_cl_95_up_adjusted = case_when(pcr_target_avg_conc_adjusted == 2000 & lab_phase %in% c("Lab Phase 1", "Lab Phase 2") ~ 2000, TRUE ~ pcr_target_cl_95_up_adjusted))

OutputForSaving <- OutputForSaving %>% 
  mutate(pcr_target_below_lod = case_when(pcr_target_avg_conc_adjusted == 2000 & lab_phase %in% c("Lab Phase 1", "Lab Phase 2") ~ 'yes', TRUE ~ pcr_target_below_lod)) #update LOD based on LOQ

#LP3
OutputForSaving <- OutputForSaving %>% 
  mutate(pcr_target_avg_conc_adjusted = case_when(pcr_target_avg_conc_adjusted <= 1200 & lab_phase == "Lab Phase 3" ~ 600, TRUE ~ pcr_target_avg_conc_adjusted)) %>% 
  mutate(pcr_target_avg_conc_adjusted = case_when(quality_flag == 'yes' & lab_phase == "Lab Phase 3" ~ 600, TRUE ~ pcr_target_avg_conc_adjusted)) %>%
  mutate(pcr_target_std_error_adjusted = case_when(pcr_target_avg_conc_adjusted == 600 & lab_phase == "Lab Phase 3" ~ 600, TRUE ~ pcr_target_std_error_adjusted)) %>% 
  mutate(pcr_target_cl_95_lo_adjusted = case_when(pcr_target_avg_conc_adjusted == 600 & lab_phase == "Lab Phase 3" ~ 600, TRUE ~ pcr_target_cl_95_lo_adjusted)) %>% 
  mutate(pcr_target_cl_95_up_adjusted = case_when(pcr_target_avg_conc_adjusted == 600 & lab_phase == "Lab Phase 3" ~ 600, TRUE ~ pcr_target_cl_95_up_adjusted))

OutputForSaving <- OutputForSaving %>% 
  mutate(pcr_target_below_lod = case_when(pcr_target_avg_conc_adjusted == 600 & lab_phase == "Lab Phase 3" ~ 'yes', TRUE ~ pcr_target_below_lod)) #update LOD based on LOQ



##############################################################################################
###                                   QC Report: Quality Flags                             ###
##############################################################################################
#print LOD/LOQ report
quality_flag_yes <- OutputForSaving %>%
  select(wwtp_name, pcr_target, sample_collect_date, test_result_date, pcr_target_avg_conc_adjusted, pcr_target_below_lod, quality_flag, lab_phase) %>%
  filter(quality_flag == "yes" & pcr_target_below_lod == "no") #verify with lab why these aren't below LOD if quality flag is yes

if (nrow(quality_flag_yes) == 0) {
  quality_flag_yes <- data.frame(
    wwtp_name = "no data"
  )
}

QualityFlag_Issues <- OutputForSaving %>%
  select(wwtp_name, pcr_target, sample_collect_date, test_result_date, pcr_target_avg_conc_adjusted, pcr_target_below_lod, quality_flag, lab_phase ) %>%
  filter(!quality_flag %in% c("yes", "no"))

if (nrow(QualityFlag_Issues) == 0) {
  QualityFlag_Issues <- data.frame(
    wwtp_name = "no data"
  )
}

write.csv(QualityFlag_Issues, paste0("output/validation_reports/QualityFlag_Issues_", Sys.Date(), ".csv"))
write.csv(quality_flag_yes, paste0("output/validation_reports/QualityFlag_yes_", Sys.Date(), ".csv"))



##############################################################################################
###                              Output: Recent Sample Summary                             ###
##############################################################################################
# Review and output recent measure dates and viral concentration data
RecentSampleSummary_Workbook <- createWorkbook()

OutputForSaving <- OutputForSaving %>%
  mutate(pcr_target = case_when(is.na(pcr_target) ~ "missing", TRUE ~ pcr_target))

##output recent measure dates and viral concentration data
pathogen <- unique(OutputForSaving$pcr_target)


for(i in pathogen){
  addWorksheet(RecentSampleSummary_Workbook, sheetName = i) 
  
  slice1 <- OutputForSaving %>% 
    filter(pcr_target == i) %>%
    arrange(wwtp_name, desc(sample_collect_date)) %>%
    group_by(wwtp_name) %>%
    slice_head(n=1) %>%
    mutate(recent_date1 = sample_collect_date, 
           recent_conc1 = pcr_target_avg_conc_adjusted) %>%
    select(wwtp_name, recent_date1, recent_conc1)
  
  slice2 <- OutputForSaving %>%
    filter(pcr_target == i) %>%
    arrange(wwtp_name, desc(sample_collect_date)) %>%
    group_by(wwtp_name)%>%
    slice_head(n=2)%>%
    slice_tail(n=1)%>%
    mutate(recent_date2 = sample_collect_date, 
           recent_conc2 = pcr_target_avg_conc_adjusted) %>%
    select(wwtp_name, recent_date2, recent_conc2)
  
  RecentSampleSummary_result <- slice1 %>%
    full_join(slice2, by="wwtp_name") %>%
    as_tibble()
  assign(paste0("RecentSampleSummary_", i), RecentSampleSummary_result)
  
  writeData(RecentSampleSummary_Workbook, i, RecentSampleSummary_result) 
}

saveWorkbook(RecentSampleSummary_Workbook, paste0("output/validation_reports/Recent_Sample_Summary_",Sys.Date()," .xlsx"), overwrite=TRUE) 

rm(slice1, slice2, RecentSampleSummary_result)


OutputForSaving %<>%
  mutate(quality_flag = case_when(pcr_target_below_lod == "yes" ~ "yes",
                                  pcr_target_cl_95_lo_adjusted <= 0 ~ "yes", 
                                  TRUE ~ quality_flag))

metadata_survey <- SystemData %>%
  select(wwtp_name, surveyed_airport_contribution:surveyed_iandi_affect)

##############################################################################################
###                              Output: Data Check Report                                 ###
##############################################################################################
#35mL
# Calculate percentiles and create analytic report to check recent values 
WWDataCheck_Workbook_35mL <- createWorkbook()

WWDataCheck_Data <- OutputForSaving %>%
  filter(!is.na(pcr_target_avg_conc_adjusted))

pathogen_35mL <- pathogen[!pathogen %in% c("RSV_A", "RSV_B")]

for (i in pathogen_35mL) {
  addWorksheet(WWDataCheck_Workbook_35mL, sheetName = i)
  
  # Filter the data for the current pathogen
  WWDataCheck_filtered <- OutputForSaving %>%
    filter(lab_phase == "Lab Phase 3", pcr_target == i)
  
  # Check if there's any data left after filtering
  if (nrow(WWDataCheck_filtered) > 0) {
    WWDataCheck_result <- WWDataCheck_filtered %>%
      group_by(wwtp_name) %>%
      reframe(
        wwtp_mean = mean(pcr_target_avg_conc_adjusted, na.rm = TRUE),
        wwtp_median = median(pcr_target_avg_conc_adjusted, na.rm = TRUE),
        wwtp_sd = sd(pcr_target_avg_conc_adjusted, na.rm = TRUE),
        quantile(pcr_target_avg_conc_adjusted, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE),
        quant_cat = c("min", "q.10", "q.20", "q.30", "q.40", "q.50", "q.60", "q.70", "q.80", "q.90", "max")
      ) %>%
      pivot_wider(names_from = quant_cat, values_from = `quantile(...)`) %>%
      ungroup() %>%
      left_join(get(paste0("RecentSampleSummary_", i)), by = "wwtp_name", copy = TRUE) %>%
      mutate(
        current_percentile = case_when(
          recent_conc1 == min ~ "min",
          recent_conc1 < q.10 ~ "1-9th percentile",
          recent_conc1 < q.20 ~ "10-19th percentile",
          recent_conc1 < q.30 ~ "20-29th percentile",
          recent_conc1 < q.40 ~ "30-39th percentile",
          recent_conc1 < q.50 ~ "40-49th percentile",
          recent_conc1 < q.60 ~ "50-59th percentile",
          recent_conc1 < q.70 ~ "60-69th percentile",
          recent_conc1 < q.80 ~ "70-79th percentile",
          recent_conc1 < q.90 ~ "80-89th percentile",
          recent_conc1 < max ~ "90-99th percentile",
          recent_conc1 == max ~ "max"
        ),
        sd_above_mean = (recent_conc1 - wwtp_mean) / wwtp_sd,
        sd_above_median = (recent_conc1 - wwtp_median) / wwtp_sd
      ) %>%
      filter(!is.na(recent_conc1))
    
    # Assign and write data if processing was successful
    assign(paste0("WWDataCheck_", i), WWDataCheck_result)
    writeData(WWDataCheck_Workbook_35mL, i, WWDataCheck_result)
  } else {
    # Optionally, print a message or handle the case where no data exists
    message(paste("No data for pathogen:", i))
  }
}

saveWorkbook(WWDataCheck_Workbook_35mL, paste0("output/validation_reports/DataCheck_35mL_",Sys.Date(),".xlsx"), overwrite=TRUE)


rm(WWDataCheck_result)


##############################################################################################
###                                           Add Crosswalk                                ###
##############################################################################################

#Adding crosswalk to Output for saving and manipulating
crosswalk <- read.xlsx("reference_sheets/Crosswalk.xlsx")

crosswalk <- crosswalk %>% 
  rename('wwtp_name' = 'wwtp_name_nwss_dcipher')

crosswalk.subset <-  crosswalk %>% 
  dplyr::select(c('wwtp_name', 'site_id', 'epa_registry_id'))

#Joining crosswalk to output for saving
OutputForSaving <- full_join(OutputForSaving, crosswalk.subset, by = 'wwtp_name')

##change county_names to text to preserve leading 0's when writing as csv
OutputForSaving$county_names <- paste0(OutputForSaving$county_names,"\t") 



##############################################################################################
###                             Output: WW Data Compiler Outputs                           ###
##############################################################################################
### Print out all data for internal data use (includes utilities not published publicly)
OutputName = paste0("output/compiled_data/1_CDPHE_Wastewater_Data_",Sys.Date()," .csv")
OutputForSaving %>%
  select(-c(pcr_target_avg_conc_adjusted, pcr_target_std_error_adjusted, pcr_target_cl_95_lo_adjusted, pcr_target_cl_95_up_adjusted)) %>%
  write_csv(OutputName,na = "",quote="all")


### Create separate output for NWSS with BLS/UMUT suppressed for publication
SystemDataPRFlag <- NWSS_masterlist %>%
  select(wwtp_name, public_repository, pcr_target, sitename_id, activation_start, activation_end)

OutputName = paste0("output/compiled_data/2_CDPHE_Wastewater_Data_",Sys.Date()," .csv")
OutputForSaving %>%
  select(-c(pcr_target_avg_conc_adjusted, pcr_target_std_error_adjusted, pcr_target_cl_95_lo_adjusted, pcr_target_cl_95_up_adjusted)) %>%
  right_join(SystemDataPRFlag, by=c("wwtp_name", "pcr_target")) %>%
  mutate(pcr_target_cl_95_up = case_when((pcr_target_below_lod == 'yes' & pcr_target_avg_conc > 1200 & lab_phase == "Lab Phase 3")  ~ 600, TRUE ~ pcr_target_cl_95_up)) %>%
  mutate(pcr_target_cl_95_lo = case_when((pcr_target_below_lod == 'yes' & pcr_target_avg_conc > 1200 & lab_phase == "Lab Phase 3")  ~ 600, TRUE ~ pcr_target_cl_95_lo)) %>%
  mutate(pcr_target_avg_conc = case_when((pcr_target_below_lod == 'yes' & pcr_target_avg_conc > 1200 & lab_phase == "Lab Phase 3")  ~ 600, TRUE ~ pcr_target_avg_conc)) %>%  
  mutate(pcr_target = case_when(pcr_target == "H1" ~ "FLUAV A H1",
                                pcr_target == "H3" ~ "FLUAV A H3",
                                pcr_target == "H5" ~ "FLUAV A H5",
                                pcr_target == "PanRSV" ~ "RSV", #NWSS needs RSV as label
                                TRUE ~ pcr_target)) %>%
  mutate(sample_id = paste0(sitename_id, sample_collect_date)) %>%  #write new sample id to correct nwss flags
  mutate(sample_id = gsub("-", "", sample_id)) %>%
  select(-c(lab_phase, sitename_id)) %>%
  mutate(pcr_target_std_error = case_when(is.na(pcr_target_avg_conc) ~ 0, TRUE ~ pcr_target_std_error), #if conc is NA, sd error 0
         pcr_target_cl_95_up = case_when(is.na(pcr_target_avg_conc) ~ 0, TRUE ~ pcr_target_cl_95_up), #if conc is NA ci 95 up 0
         pcr_target_cl_95_lo = case_when(is.na(pcr_target_avg_conc) ~ 0, TRUE ~ pcr_target_cl_95_lo), #if conc is NA ci 95 lo 0
         pcr_target_avg_conc = case_when(is.na(pcr_target_avg_conc) ~ 0, TRUE ~ pcr_target_avg_conc)) %>% #if conc is NA, replace with 0
  filter(sample_collect_date >= activation_start) %>%
  filter(sample_collect_date <= activation_end | is.na(activation_end)) %>%
  select(-c(activation_start, activation_end)) %>%
  write_csv(OutputName, na = "", quote = "all")



### Print out all data for internal data use (includes utilities not published publicly)
OutputName = paste0("output/compiled_data/3_CDPHE_Wastewater_Data_",Sys.Date(),".csv")
#remove unadjusted values, rename adjusted as original remaining pipeline
OutputForSaving %>%
  select(-c(pcr_target_avg_conc, pcr_target_std_error, pcr_target_cl_95_lo, pcr_target_cl_95_up)) %>%
  rename(pcr_target_avg_conc = pcr_target_avg_conc_adjusted,
         pcr_target_std_error = pcr_target_std_error_adjusted,
         pcr_target_cl_95_lo = pcr_target_cl_95_lo_adjusted,
         pcr_target_cl_95_up = pcr_target_cl_95_up_adjusted) %>%
  write_csv(OutputName,na = "",quote="all")


#remove extra NWSS columns for remaining outputs
OutputForSaving <- OutputForSaving %>% 
  dplyr::select(-c(site_id, epa_registry_id))



###print output for Hx script
write.csv(OutputForSaving, "output/working data files/flu_hx_scriptinput.csv")

#Export names to the crosswalk maker
OutputForSaving %>% 
  ungroup() %>%
  dplyr::select(wwtp_name) %>%
  unique() %>%
  `colnames<-`("wwtp_name_nwss_dcipher") %>%
  write_csv(paste0("reference_sheets/crosswalk builder/ww data names.csv"))



################################################################################################
###                              QC Report: Participation Issues                             ###
################################################################################################
##   Find any typos in utilities/activation status sheet    
OutputForSaving_typos <- OutputForSaving %>%
  left_join(IDB_masterlist, by = c("pcr_target", "wwtp_name"), relationship = "many-to-many") %>%
  select(c(wwtp_name, pcr_target, Surv_System, activation_start, activation_end, sample_collect_date, test_result_date, pcr_target_avg_conc, sample_id)) %>%
  filter(!(wwtp_name %in% IDB_masterlist$wwtp_name))

if (nrow(OutputForSaving_typos) == 0) {
  OutputForSaving_typos <- data.frame(wwtp_name = "no data")
}

write.csv(OutputForSaving_typos, paste0("output/validation_reports/participation_issues_", Sys.Date(), ".csv"))


##############################################################################################
###                                  QC Report: Extra Results                              ###
##############################################################################################
##   Find any utilities/pcr_targets we should not be testing for    
OutputForSaving_extras <- OutputForSaving %>%
  left_join(IDB_masterlist, by = c("pcr_target", "wwtp_name"), relationship = "many-to-many") %>%
  select(c(wwtp_name, pcr_target, Surv_System, activation_start, activation_end, sample_collect_date, test_result_date, pcr_target_avg_conc, sample_id)) %>%
  filter(is.na(Surv_System)) %>%
  filter(sample_collect_date >= Sys.Date() - 31)

if (nrow(OutputForSaving_extras) == 0) {
  OutputForSaving_typos <- data.frame(wwtp_name = "no data")
}

write.csv(OutputForSaving_extras, paste0("output/validation_reports/extra_results_", Sys.Date(), ".csv"))

