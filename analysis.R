# nothing here yet
#library(lubridate)
#library(dplyr)
#library(ggplot2)
library(tidyverse)
#
# FUNCTIONS
#

#
# LOAD DATA IN
#

dat <- read.csv("/home/user/Dropbox/HWF_data/hwf_data_renorm_reformat.csv")
dat <- data.frame(dat) #data.frame(dat)
#dat$timestamp <- as.Date(data$timestamp, "%Y-%m-%d %H:%M:%OS")
#data$influenza_test_date <- as.Date(data$influenza_test_date, "%Y-%m-d% %H:%M:%OS")
#data$covid19_test_date <- as.Date(data$covid19_test_date)

#
# LOAD OFFICIAL DATA
# 

# plot responses over time

#
# ANALYSIS
#
#ggplot(dat,  aes(preexisting_none, race_cat)) + geom_bar(aes(color=race_cat)) + theme_bw()

# subset by people who have

colnames(dat)[grepl("ext", colnames(dat))]
symptoms_ext = paste0("covid_19_ext_symptoms_", c("chills_shaking",
             "dry_cough",
             "fever",
             "loss_of_appetite",
             "loss_of_taste",
             "nasal_congestion",
             "shortness_of_breath",
             "tight_feeling_in_chest",
             "tinnitus",
             "abdominal_pain",
             "diarrhea",
             "facial_numbness",
             "irregular_heartbeat",
             "loss"
             ))

symptoms = paste0("symptoms_", c("chills_shaking",
             "dry_cough",
             "fever",
             "loss_of_appetite",
             "loss_of_taste",
             "nasal_congestion",
             "shortness_of_breath",
             "tight_feeling_in_chest",
             "tinnitus"
             ))

             

# which rows dont match

#not_match <- apply(dat[,symptoms] == dat[,symptoms_ext],1,function(x) all(x))
