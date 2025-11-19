

SystemData_sarssequencing <- IDB_masterlist %>%
  filter(pcr_target == "sars-cov-2") %>%
  left_join(SystemData, by = "wwtp_name") %>%
  select(sitename_id, wwtp_name) %>%
  rename(site_id = sitename_id)


###################################################################
##                 SARS-CoV-2 Sequencing Data                    ##
###################################################################
# ### pull from Google folder (skip down to line 52 if you want to save all sequencing data directly to import folder)
print("Connecting to Google Drive and downloading recent Sars-CoV-2 sequencing data")

folder_id <- "1Hv_3j0EgtLlqBIPWQvsEjZqNFs8YEHLD" #from https://drive.google.com/drive/u/0/folders/1Hv_3j0EgtLlqBIPWQvsEjZqNFs8YEHLD
files <- drive_ls(path = as_id(folder_id))

GDDownloader <- function(fileslist,savepath = "import/sars_sequencing_results/",overwritefiles = TRUE){
  for(i in 1:nrow(fileslist)){
    local_filename <- paste0(savepath, fileslist$name[i])
    
    # Skip if already downloaded and not overwriting
    if (!overwritefiles && file.exists(local_filename)) {
      print(paste0("Skipping file (already exists): ", fileslist$name[i]))
      next
    }
    
    
    print(paste0(
      "Saving file ",
      i,
      " of ",
      nrow(fileslist),
      ": ",
      fileslist$name[i]
    ))

    berra(drive_download(file = fileslist$id[i],
                         path = paste0(savepath, fileslist$name[i]),
                         overwrite = overwritefiles))

  }
}

##save a copy of new files from google drive
GDDownloader(fileslist = files,
             savepath = "import/sars_sequencing_results/",
             overwritefiles = FALSE)


##bind all files together 
print("Loading Sars-CoV-2 Sequencing Data")
sars_sequencing_files <- list.files("import/sars_sequencing_results/") %>%
  data.frame() %>%
  `colnames<-`("filename") %>%
  mutate(extension = file_ext(filename)) %>%
  filter(extension %in% "csv") %>%
  mutate(date = str_split_fixed(filename,pattern="_",n=6)) %>%
  mutate(date = ymd(date[,6])) 

sars_sequencing_data <- data.frame(stringsAsFactors = FALSE) 

for(i in 1:nrow(sars_sequencing_files)){
  selFilename = sars_sequencing_files[i,1]
  selDate = sars_sequencing_files[i,3]
  
  print(selFilename)
  
  try(sars_sequencing_data <-read.csv(paste0("import/sars_sequencing_results/",
                                        selFilename)) %>%
        mutate(test_result_date = selDate) %>%
        bind_rows(sars_sequencing_data))
}


sars_sequencing_data <- sars_sequencing_data %>%
  rename(DetectionStatus = note) %>%
  rename(variant = cdc_lineage) %>%
  rename(abundance = cdc_abundance) %>%
  rename(sample_collect_date = collection_date)

#update "None Detected" to Low Coverage
sars_sequencing_data <- sars_sequencing_data %>%
  mutate(variant = case_when(variant == "NoneDetected" ~ "Insufficient Signal",
                             TRUE ~ variant)) %>%
  mutate(DetectionStatus = case_when(DetectionStatus == "LowCoverageSample" ~ "Insufficient Signal",
                                     TRUE ~ DetectionStatus)) %>%
  mutate(pcr_target = "sars-cov-2")

##remove any duplicates, using more recent test_result_date
sars_sequencing_data <- sars_sequencing_data %>%
  filter(!DetectionStatus %in% c("FailedVariantCalling", "FailedSequencing")) %>%
  group_by(site_id, sample_id, variant) %>%
  arrange(site_id, sample_id, variant, desc(test_result_date)) %>%
  slice_head(n = 1) %>%
  select(-X) %>%
  left_join(SystemData_sarssequencing, by = "site_id")

#write to shared drive as compiled file
write.csv(sars_sequencing_data, paste0("output/compiled_data/SARS-CoV-2_Sequencing_Results_Combined_", Sys.Date()  ,".csv"))


###################################################################
##                     Write to PBD Dashboard                    ##
###################################################################

sars_sequencing_data_PDB <- sars_sequencing_data %>%
  filter(wwtp_name %in% PFD_masterlist_sars$wwtp_name) %>%
  select(c(wwtp_name, pcr_target, sample_collect_date, variant, abundance, sample_id))

write.xlsx(sars_sequencing_data_PDB, "dashboards/PublicDashboard/PDB_Sars-CoV-2_Sequencing.xlsx",
           sheetName = "sars_variants")

