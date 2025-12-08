###Prepare the environment-----
library(tidyverse)
library(redcapAPI)
library(ggplot2)
library(ggthemes)
library(readr)
library(tools)
library(lubridate)
library(stringr)
library(readxl)
library(zoo)
library(googledrive)
library(DBI)
library(sf)
library(magrittr)
library(filesstrings)
library(openxlsx)
library(googlesheets4)
library(googledrive)
library(bigrquery)
library(padr)
library(skimr)
library(dplyr)
library(pacman)
library(janitor)
library(bsts)
library(reshape2)
library(rsconnect)


###Define Brian's Favorite Utility Functions----
"%!in%" <- function(x,y)!('%in%'(x,y))

berra <- function(expr){
  tryCatch(expr,
           error = function(e){
             message("An error occurred:\n", e)
           },
           warning = function(w){
             expr
             message("A warning occured:\n", w)
           },
           finally = {})
}

UserCheck <- function(userprompt = "Do you wish to proceed? ",
                yesans = "Great. Off you go! ",
                noans = "Alright. Well, let me know when you are then. ") {
  ANSWER <- readline(userprompt)
  ## a better version would check the answer less cursorily, and
  ## perhaps re-prompt
  if (substr(ANSWER, 1, 1) == "y")
    cat(yesans)
  else
    stop(noans)
}

print("Environment Prepared")