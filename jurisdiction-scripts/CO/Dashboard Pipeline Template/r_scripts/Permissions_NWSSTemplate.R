library(tidyverse)
library(shinyauthr)
library(sodium)
library(dplyr)
library(redcapAPI)

REDCapAPI <- "ENTER TOKEN HERE" #Enter your redcap API token here


# run libraries and f(x) first
rcon <- redcapConnection(url = "https://cdphe.redcap.state.co.us/api/", token = REDCapAPI)

permission_request_colnames <- read_excel("dashboards/permissions_supporting_data/colnames.xlsx") %>%
  colnames()

# may need to find and replace mutate and rename codes to dplyr::
permission_request <- exportRecordsTyped(rcon = rcon) %>% 
  select(record_id:main_form_complete) %>%
 dplyr::mutate(record_id = as.numeric(record_id))%>%
  `colnames<-`(permission_request_colnames) %>%
  dplyr::mutate(dplyr::across(Alamosa:`Woodland Park`,
          ~ case_when(as.character(.) %in% "Checked" ~ TRUE,
                            as.character(.) %in% "Unchecked" ~ FALSE))) %>%
  filter(approved==TRUE) %>%
  dplyr::rename(Wellington = `Wellington Water and Sewer`)

# detach("package:plyr", unload=TRUE)

allowed_utilities <- permission_request %>%
  dplyr::mutate(across(Alamosa:`Woodland Park`,.fns = as.logical)) %>%
  pivot_longer(cols=Alamosa:`Woodland Park`) %>%
  group_by(user_email) %>%
  filter(record_id == max(record_id)) %>%
  filter(value == TRUE) %>%
  summarize(allowed_utilities = list(unique(name))) %>%
  unique()

permission_request <- permission_request %>%
  left_join(allowed_utilities,by="user_email")

user_base <- tibble::tibble(
  user = permission_request$user_email,
  password = purrr::map_chr(permission_request$password, sodium::password_store),
  user_name = permission_request$user_name,
  org_name = permission_request$org_name,
  position = permission_request$user_position,
  allowed_utilities = permission_request$allowed_utilities,
  sequencing = permission_request$sequencing
)

saveRDS(user_base, "dashboards/InternalDashboard/user_base.rds")

rm(user_base,permission_request,allowed_utilities)
