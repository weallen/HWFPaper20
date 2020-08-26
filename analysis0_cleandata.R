######################################################
###           Load and clean HWF Data              ###
######################################################
##
## Edited: 05/13/2020

####################
### load libraries
library(tidyverse)

####################
### import data
dat <- read.csv("/ncf/xlin_covid/data/hwf_data_renorm_reformat.csv")
dat <- data.frame(dat)

####################
### clean data

## exclude <18
dat <- dat[dat$age>=18,]

## exclude NA session_id
dat <- dat[!is.na(dat$session_id),]

## exclude multiple logins per day (take the first)
dat <- (dat) %>% 
  group_by(session_id,date) %>%
  summarise_all(first)

## add variable for latest survey completed
dat <- dat%>%group_by(session_id)%>%arrange(session_id,date)%>%mutate(latest_surv=max(survey_version))

## remove conflicting data: anyone who took a survey and then took an older survey afterward
datk <- dat%>%group_by(session_id)%>%arrange(session_id,date)%>%summarise_all(last)
dat <- dat[!dat$session_id%in%which(datk$latest_surv!=datk$survey_version),]
rm(datk)

## remove conflicting data: anyone who changed gender between surveys
datg <- dat%>%group_by(session_id,gender)%>%summarise(n = n())
dat <- dat[!(dat$session_id %in% datg$session_id[duplicated(datg$session_id)]),]
rm(datg)

## remove vars whose gender/feeling/smoking is missing
dat <- dat[dat$feeling!="",]
dat$feeling <- droplevels(dat$feeling)
dat <- dat[dat$gender!="",]
dat$gender <- droplevels(dat$gender)
dat <- dat[dat$smoking_history!="",]
dat$smoking_history <- droplevels(dat$smoking_history)


## return to dataframe
dat <- data.frame(dat)

###########################
### Align survey versions
## 420 is 4.2, which added a question about cancelled appointments (which isnt in this dataset)
dat$survey_version[dat$survey_version==420 |dat$survey_version==421] <- 4


###########################
### Demographics
race_names <- c("race_ethnicity_african_american",
                "race_ethnicity_american_indian_or_alaska_native",
                "race_ethnicity_asian",
                "race_ethnicity_hawaiian_or_islander",
                "race_ethnicity_hispanic_latino",
                "race_ethnicity_white",
                "race_ethnicity_other")

race_names_all <- c("race_ethnicity_not_say",
                    race_names)

## missing: for those who havent done a survey 3 yet
for(ff in race_names){
  levels(dat[,ff]) <- c(levels(dat[,ff]),"missing")
  dat[dat$latest_surv<3,ff] <- "missing"
}

# ### new categorical variable for race
# ## set to "missing IF: no other race reported
# ## set to "multiracial" if more than one race reported
# dat$race_cat <- "missing"
# for(ff in race_names){
#   dat$race_cat[dat[,ff]=="True"] <- substring(ff,16)
# }
# dat$race_cat[apply(dat[,race_names],1,function(x) sum(x=="True")>1 ) ] <- "multiracial"
# dat$race_cat <- factor(dat$race_cat,levels=c("missing",substring(race_names,16),"multiracial"))
# 
# 
# ### new race variable: collapsing smallest categories
# dat$race_cat_collapsed <- fct_collapse(dat$race_cat,other=c("american_indian_or_alaska_native","hawaiian_or_islander","other"))

### Categorizing Race
#### if you report white and nothing else: white
#### else if you report aa and nothing else: aa
#### else if you report latino and nothing else: latino
#### else if you report asian and nothing else: asian
#### else if you report white + another race: white_multiracial
#### else if you reported at least one race but none of the above apply: other
#### else: missing
dat <- data.frame(dat)
dat$race_cat <- "missing"
dat$race_cat[apply(dat[,race_names],1,function(x) sum(x=="True"))>0] <- "other"
dat$race_cat[dat$race_ethnicity_white=="True" & apply(dat[,race_names],1,function(x) sum(x=="True"))==1] <- "white"
dat$race_cat[dat$race_ethnicity_hispanic_latino=="True" & apply(dat[,race_names],1,function(x) sum(x=="True"))==1] <- "hispanic_latino"
dat$race_cat[dat$race_ethnicity_african_american=="True" & apply(dat[,race_names],1,function(x) sum(x=="True"))==1] <- "african_american"
dat$race_cat[dat$race_ethnicity_asian=="True" & apply(dat[,race_names],1,function(x) sum(x=="True"))==1] <- "asian"
dat$race_cat[dat$race_ethnicity_white=="True" & apply(dat[,race_names],1,function(x) sum(x=="True"))>1] <- "multiracial_white"
dat$race_cat[dat$race_ethnicity_white!="True" & apply(dat[,race_names],1,function(x) sum(x=="True"))>1] <- "other" ## "multiracial_nonwhite"
dat$race_cat <- factor(dat$race_cat,levels=c("white","hispanic_latino","african_american","asian","other","multiracial_white","missing")) ##,"multiracial_nonwhite"



### new variable for race="not_say"
## equivalent to not reporting another race
## old version of "not_say" had overlap with some reported races, as well as some "False" observations while no other races reported
dat$race_ethnicity_not_say <- "False"
dat$race_ethnicity_not_say[dat$race_cat=="missing"] <- "True"
dat$race_ethnicity_not_say <- factor(dat$race_ethnicity_not_say,levels=c("False","True"))



### clean up labelling for age_range
## dat$age: some missingness. also some implausible ages (120)
## currently setting these just to 80+ in dat$age_range; could alternatively count as missing
dat$age_range_NEW <- "[18,30)"
dat$age_range_NEW[dat$age>=30] <- "[30,45)"
dat$age_range_NEW[dat$age>=45] <- "[45,60)"
dat$age_range_NEW[dat$age>=60] <- "[60,80)"
dat$age_range_NEW[dat$age>=80] <- "[80+)"
dat$age_range_NEW[is.na(dat$age)] <- "missing"
dat$age_range_NEW <- factor(dat$age_range_NEW,levels=c("[18,30)","[30,45)","[45,60)","[60,80)","[80+)","missing"))


# dat$household_members_count ##: no missingness in survey 4, only slight missingness in survey 2; lots of missingness in survey 3
# dat$household_members_affected: defined only in survey 2



###########################
### preexisting
preexisting_names <-c("preexisting_allergies",
                      "preexisting_asthma",
                      "preexisting_autoimmune_disease",
                      "preexisting_cancer",
                      "preexisting_cardiovascular_disease",
                      "preexisting_chronic_kidney_disease",
                      "preexisting_chronic_lung_disease",
                      "preexisting_diabetes",
                      "preexisting_hypertension",
                      "preexisting_immunodeficiency",
                      "preexisting_liver_disease",
                      "preexisting_pregnant")

preexisting_names_all <-c("preexisting_not_say",
                          "preexisting_none",
                          preexisting_names)

## missing: for those who havent done a survey 3 yet, some preexisting questions were not asked
for(ff in c("preexisting_allergies","preexisting_autoimmune_disease","preexisting_liver_disease","preexisting_pregnant")){
  levels(dat[,ff]) <- c(levels(dat[,ff]),"missing")
  dat[dat$latest_surv<3,ff] <- "missing"
}



### new var for number of preexisting conditions 
## NA if: no others reported AND preexisting_none="False"
dat$number_preexisting <- NA
dat$number_preexisting[dat$preexisting_none=="True"] <- 0
dat$number_preexisting[apply(dat[,preexisting_names],1,function(x) sum(x=="True"))>0] <- apply(dat[,preexisting_names],1,function(x) sum(x=="True"))[apply(dat[,preexisting_names],1,function(x) sum(x=="True"))>0]

## NA if: no others reported AND preexisting_none="False"
dat$number_preexisting_cat <- "missing"
dat$number_preexisting_cat[dat$number_preexisting==0] <- "0"
dat$number_preexisting_cat[dat$number_preexisting==1] <- "1"
dat$number_preexisting_cat[dat$number_preexisting>1] <- "2+"
dat$number_preexisting_cat <- factor(dat$number_preexisting_cat,levels=c("missing","0","1","2+"))

### new var for no preexisting
## True if: preexisting_none and also no others reported
dat$preexisting_none_NEW <- "False"
dat$preexisting_none_NEW[dat$number_preexisting==0] <- "True"
dat$preexisting_none_NEW <- factor(dat$preexisting_none_NEW,levels=c("False","True"))

### new var: not_say = no reported preexisting conditions and also did not report "preexisting_none"
## old preexisting_not_say had overlap with other conditions, and also was sometimes False when all others and "none" were unreported
dat$preexisting_not_say_NEW <- "False"
dat$preexisting_not_say_NEW[is.na(dat$number_preexisting)] <- "True"
dat$preexisting_not_say_NEW <- factor(dat$preexisting_not_say_NEW,levels=c("False","True"))

## replace
dat$preexisting_none <- dat$preexisting_none_NEW
dat$preexisting_not_say <- dat$preexisting_not_say_NEW
dat <- dat[,!colnames(dat)%in%c("preexisting_none_NEW","preexisting_not_say_NEW")]



###########################
### profession 
profession_names <- c("profession_construction",
                      "profession_critical_manufacturing",
                      "profession_delivery",
                      "profession_essential_retail",
                      "profession_food_production",
                      "profession_food_service",
                      "profession_healthcare",
                      "profession_law_first_responder",
                      "profession_public_works",
                      "profession_transportation")

profession_names_all <- c("profession_none",
                          profession_names)


## missing: for those who havent done a survey 4 yet
for(ff in c(profession_names_all)){
  levels(dat[,ff]) <- c(levels(dat[,ff]),"missing")
  dat[dat$latest_surv<4,ff] <- "missing"
  
}

### rename nonessential profession
colnames(dat)[colnames(dat)%in%c("profession_none")] <- "profession_nonessential"

### indicator of essential profession
dat$profession_essential <- "False"
dat$profession_essential[apply(dat[,profession_names],1,function(x) sum(x=="True"))>0] <- "True"
dat$profession_essential[dat$latest_surv<4] <- "missing"
dat$profession_essential <- factor(dat$profession_essential,levels=c("False","True","missing"))

## categorical for essential or healthcare or nonessential
## missing is either they werent asked, or didnt report nonessential OR essential
dat$profession_essential_cat <- "missing"
dat$profession_essential_cat[dat$profession_nonessential=="True"] <- "nonessential"
dat$profession_essential_cat[dat$profession_essential=="True"] <- "other_essential"
dat$profession_essential_cat[dat$profession_healthcare=="True"] <- "healthcare"
dat$profession_essential_cat <- factor(dat$profession_essential_cat,levels=c("nonessential","other_essential","healthcare","missing"))



###########################
### feeling
dat$feeling <- droplevels(dat$feeling)  ## defined in all surveys

## feeling safe to work defined only for survey 4
levels(dat$feeling_safe_to_work) <- c(levels(dat$feeling_safe_to_work),"not_asked")
dat$feeling_safe_to_work[dat$survey_version<4] <- "not_asked"
dat$feeling_safe_to_work <- fct_recode(dat$feeling_safe_to_work, "missing"="")




###########################
### Other attributes

## reorder levels
dat$smoking_history <- factor(dat$smoking_history, levels=c("never_smoked","used_to_smoke","currently_smoke"))


## not asked before survey 4
dat$sleep <- as.character(dat$sleep)
dat$sleep[dat$survey_version<4] <- "not_asked"
dat$sleep[dat$sleep==""] <- "missing"
dat$sleep <- factor(dat$sleep,levels=c("missing","less_than_5","5_to_6_hours","7_to_8_hours","9_to_10_hours","11_or_more_hours","not_asked"))


### new categorical var for number of contacts
## not asked before survey 4
## why is the minimum 1 and not 0?
dat$estimate_people_contact_cat <- "missing"
dat$estimate_people_contact_cat[dat$estimate_people_contact==1] <- "1"
dat$estimate_people_contact_cat[dat$estimate_people_contact>=2 & dat$estimate_people_contact<=4] <- "2-4"
dat$estimate_people_contact_cat[dat$estimate_people_contact>=5 & dat$estimate_people_contact<=9] <- "5-9"
dat$estimate_people_contact_cat[dat$estimate_people_contact>=10 & dat$estimate_people_contact<=19] <- "10-19"
dat$estimate_people_contact_cat[dat$estimate_people_contact>=20] <- "20+"
dat$estimate_people_contact_cat[dat$survey_version<4] <- "not_asked"
dat$estimate_people_contact_cat <- factor(dat$estimate_people_contact_cat, levels=c("missing","1","2-4","5-9","10-19","20+","not_asked"))





###########################
### Food source

food_source_names <- c("food_source_convenience_store",
                       "food_source_delivery",
                       "food_source_food_bank" ,
                       "food_source_other",
                       "food_source_restaurant",
                       "food_source_supermarket")

## not asked before survey 4
for(ff in food_source_names){
  levels(dat[,ff]) <- c(levels(dat[,ff]),"False","not_asked")
  dat[dat$survey_version<4,ff] <- "not_asked"
  dat[c(dat[,ff]==""),ff] <- "False"
  dat[,ff] <- droplevels(dat[,ff])
}

## indicator for not reporting food source
dat$food_source_not_say <- "False"
dat$food_source_not_say[apply(dat[,food_source_names],1,function(x) sum(x=="True"))==0] <- "True"
dat$food_source_not_say[dat$survey_version<4] <- "not_asked"
dat$food_source_not_say <- factor(dat$food_source_not_say,levels=c("False","True","not_asked"))

## number of food sources among those who reported food sources
dat$NEW_number_food_sources <- NA
dat$NEW_number_food_sources[apply(dat[,food_source_names],1,function(x) sum(x=="True"))>0] <- apply(dat[,food_source_names],1,function(x) sum(x=="True"))[apply(dat[,food_source_names],1,function(x) sum(x=="True"))>0]






###########################
### left home today

## only defined for survey 4
left_home_names <- c("left_home_left_for_exercise",
                     "left_home_left_for_other",
                     "left_home_left_for_work")
left_home_names_all <- c("left_home_stayed_home",
                         left_home_names)
## not asked before survey 4
for(ff in left_home_names_all){
  levels(dat[,ff]) <- c(levels(dat[,ff]),"False","not_asked")
  dat[dat$survey_version<4,ff] <- "not_asked"
  dat[c(dat[,ff]==""),ff] <- "False"
  dat[,ff] <- droplevels(dat[,ff])
  
}

# ### new version of left_home_not_say which is True if no reported reasons to leave and ALSO reported not staying home
# dat$left_home_not_say_NEW <- "False"
# dat$left_home_not_say_NEW[apply(dat[,left_home_names_all],1,function(x) sum(x=="True"))==0] <- "True"
# dat$left_home_not_say_NEW[dat$survey_version<4] <- "not_asked"
# dat$left_home_not_say_NEW <- factor(dat$left_home_not_say_NEW,levels=c("False","True","not_asked"))

### new version of left_home_stayed home which is True only if reported stay home AND no other reasons to leave home
dat$left_home_stayed_home_NEW <- "False"
dat$left_home_stayed_home_NEW[dat$left_home_stayed_home=="True" & apply(dat[,left_home_names],1,function(x) sum(x=="True"))==0] <- "True"
dat$left_home_stayed_home_NEW[dat$left_home_stayed_home=="False" & apply(dat[,left_home_names],1,function(x) sum(x=="True"))==0] <- "missing"
dat$left_home_stayed_home_NEW[dat$survey_version<4] <- "not_asked"
dat$left_home_stayed_home_NEW <- factor(dat$left_home_stayed_home_NEW,levels=c("missing","False","True","not_asked"))


### older question asked only in survey 3
dat$left_home_today <- as.character(dat$left_home_today)
dat$left_home_today[dat$survey_version!=3] <- "not_asked"
dat$left_home_today[dat$left_home_today==""] <- "missing"
dat$left_home_today <- factor(dat$left_home_today,levels=c("missing", "stayed_home","left_for_work","left_for_other","not_asked"))


### new combined version of "stayed home"
dat$combined_stayed_home <- "False"
dat$combined_stayed_home[dat$left_home_today=="stayed_home" | dat$left_home_stayed_home_NEW=="True"] <- "True"
dat$combined_stayed_home[dat$left_home_today=="missing" | dat$left_home_stayed_home_NEW=="missing"] <- "missing"
dat$combined_stayed_home[dat$survey_version==2] <- "not_asked"
dat$combined_stayed_home <- factor(dat$combined_stayed_home,levels=c("missing","False","True","not_asked"))

### new combined version of "left_for_work"
dat$combined_left_for_work <- "False"
dat$combined_left_for_work[dat$left_home_today=="left_for_work" | dat$left_home_left_for_work=="True"] <- "True"
dat$combined_left_for_work[dat$left_home_today=="missing" | dat$left_home_stayed_home_NEW=="missing"] <- "missing"
dat$combined_left_for_work[dat$survey_version==2] <- "not_asked"
dat$combined_left_for_work <- factor(dat$combined_left_for_work,levels=c("missing","False","True","not_asked"))

### new combined version of "left_for_other", to include left_for_other AND ALSO left_for_excercise
dat$combined_left_for_other <- "False"
dat$combined_left_for_other[dat$left_home_today=="left_for_other" | dat$left_home_left_for_other=="True" | dat$left_home_left_for_exercise=="True"] <- "True"
dat$combined_left_for_other[dat$left_home_today=="missing" | dat$left_home_stayed_home_NEW=="missing"] <- "missing"
dat$combined_left_for_other[dat$survey_version==2] <- "not_asked"
dat$combined_left_for_other <- factor(dat$combined_left_for_other,levels=c("missing","False","True","not_asked"))

# ## replace
# dat$left_home_not_say <- dat$left_home_not_say_NEW
# dat$left_home_stayed_home <- dat$left_home_stayed_home_NEW
# dat <- dat[,!colnames(dat)%in%c("left_home_not_say_NEW","left_home_not_say_NEW")]

## only leave combined vars
dat <- dat[,!colnames(dat)%in%c(left_home_names_all,"left_home_stayed_home_NEW","left_home_today")]


###########################
### protective measures
protective_names <- c("protective_measures_face_mask",
                      "protective_measures_other_face_covering",
                      "protective_measures_social_distancing")
protective_names_all <- c("protective_measures_none",
                          protective_names)

## not asked before survey 3 ## also not asked if didnt leave home
for(ff in protective_names_all){
  levels(dat[,ff]) <- c(levels(dat[,ff]),"False","not_asked")
  dat[dat$survey_version<3,ff] <- "not_asked"
  dat[dat$combined_stayed_home!="False",ff] <- "not_asked"
  # dat[!(dat$combined_left_for_work=="True" | dat$combined_left_for_other=="True"),ff] <- "not_asked"
  dat[c(dat[,ff]==""),ff] <- "False"
  dat[,ff] <- droplevels(dat[,ff])
  
}

## protective measures exists for survey 3 and 4
dat$number_protective <- "missing"
dat$number_protective[dat$protective_measures_none=="True"] <- "0"
dat$number_protective[apply(dat[,protective_names],1,function(x) sum(x=="True"))==1] <- "1"
dat$number_protective[apply(dat[,protective_names],1,function(x) sum(x=="True"))>1] <- "2+"
dat$number_protective[dat$survey_version<3] <- "not_asked"
dat$number_protective[dat$combined_stayed_home!="False"] <- "not_asked"
# dat$number_protective[!(dat$combined_left_for_work=="True" | dat$combined_left_for_other=="True")] <- "not_asked"
dat$number_protective <- factor(dat$number_protective,levels=c("missing","0","1","2+","not_asked"))

### new protective_measures_none in case some falsely reported as none and another measure is reported (not currently happening)
dat$protective_measures_none_NEW <- "False"
dat$protective_measures_none_NEW[dat$number_protective==0] <- "True"
dat$protective_measures_none_NEW[dat$survey_version<3] <- "not_asked"
dat$protective_measures_none_NEW[dat$combined_stayed_home!="False"] <- "not_asked"
# dat$protective_measures_none_NEW[!(dat$combined_left_for_work=="True" | dat$combined_left_for_other=="True")] <- "not_asked"
dat$protective_measures_none_NEW <- factor(dat$protective_measures_none_NEW,levels=c("False","True","not_asked"))

# replace
dat$protective_measures_none <- dat$protective_measures_none_NEW
dat <- dat[,!colnames(dat)%in%c("protective_measures_none_NEW")]


### before the protective measures question, isolation question was in survey 2
iso_names <- c("social_no_change_to_typical_activities",
               "social_social_distancing",
               "social_stay_at_home_as_much_as_possible",
               "social_in_home_isolation")

## not asked after survey 2
for(ff in iso_names){
  levels(dat[,ff]) <- c(levels(dat[,ff]),"False","not_asked")
  dat[dat$survey_version>2,ff] <- "not_asked"
  dat[c(dat[,ff]==""),ff] <- "False"
  dat[,ff] <- droplevels(dat[,ff])
  
}

## isolation: strongest reported form of distancing
dat$social_isolation_cat <- "missing"
dat$social_isolation_cat[dat$social_no_change_to_typical_activities=="True"] <- "No Change"
dat$social_isolation_cat[dat$social_social_distancing=="True"] <- "Distancing"
dat$social_isolation_cat[dat$social_stay_at_home_as_much_as_possible=="True"] <- "Home As Much as Possible"
dat$social_isolation_cat[dat$social_in_home_isolation=="True"] <- "Isolation"
dat$social_isolation_cat[dat$survey_version>2] <- "not_asked"
dat$social_isolation_cat <- factor(dat$social_isolation_cat,levels=c("missing","No Change","Distancing","Home As Much as Possible","Isolation","not_asked"))




###########################
### symptoms

# for survey 3 and 4, separate questions for symptoms and well_symptoms
# variable "none" is defined only in survey 2


symptom4_names <- c("symptoms_abdominal_pain",
                    "symptoms_chills_shaking",
                    "symptoms_diarrhea",
                    "symptoms_dry_cough",
                    "symptoms_facial_numbness",
                    "symptoms_fever",
                    # "symptoms_fever_temp_f",
                    "symptoms_irregular_heartbeat",
                    "symptoms_loss_of_appetite",
                    "symptoms_loss_of_smell",
                    "symptoms_loss_of_taste",
                    "symptoms_muscle_and_joint_pain",
                    "symptoms_nasal_congestion",
                    "symptoms_nausea_and_vomiting",
                    "symptoms_runny_nose",
                    "symptoms_shortness_of_breath",
                    "symptoms_sore_throat",
                    "symptoms_tight_feeling_in_chest",
                    "symptoms_tingling_sensation",
                    "symptoms_tinnitus",
                    "symptoms_wet_cough")



symptom3_names <- c("symptoms_abdominal_pain",
                    "symptoms_chills_shaking",
                    "symptoms_confusion",  
                    "symptoms_diarrhea",
                    "symptoms_dry_cough",
                    "symptoms_fatigue",  
                    "symptoms_fever",
                    # "symptoms_fever_temp_f",
                    "symptoms_headache",  ## not in 4
                    "symptoms_loss_of_appetite",
                    "symptoms_loss_of_taste_and_or_smell", ## not in 4
                    "symptoms_muscle_and_joint_pain",
                    "symptoms_nasal_congestion",
                    "symptoms_nausea_and_vomiting",
                    "symptoms_runny_nose",
                    "symptoms_shortness_of_breath",
                    "symptoms_sore_throat",
                    "symptoms_tight_feeling_in_chest",
                    "symptoms_wet_cough")

symptom2_names <- c("symptoms_chills_shaking",
                    "symptoms_confusion",  
                    "symptoms_diarrhea",
                    "symptoms_dry_cough",
                    "symptoms_fatigue",  
                    "symptoms_fever",
                    # "symptoms_fever_temp_f",
                    "symptoms_headache",  
                    "symptoms_loss_of_appetite",
                    "symptoms_loss_of_taste_and_or_smell", ## not in 4
                    "symptoms_muscle_and_joint_pain",
                    "symptoms_nasal_congestion",
                    "symptoms_nausea_and_vomiting",
                    "symptoms_runny_nose",
                    "symptoms_shortness_of_breath",
                    "symptoms_sore_throat",
                    "symptoms_tight_feeling_in_chest",
                    "symptoms_wet_cough")

## the symptoms that have been included in every survey
static_symptom_names <- intersect(intersect(symptom4_names,symptom3_names),symptom2_names)
## all symptoms, whether or not theyve been included in every survey
all_symptom_names <- union(union(symptom4_names,symptom3_names),symptom2_names)

## not all asked in every survey
for(ff in c(all_symptom_names,"symptoms_none")){
  levels(dat[,ff]) <- c(levels(dat[,ff]),"False","not_asked")
  
  if(!(ff%in%symptom4_names)){
    dat[dat$survey_version==4,ff] <- "not_asked"
  }
  if(!(ff%in%symptom3_names)){
    dat[dat$survey_version==3,ff] <- "not_asked"
  }
  if(!(ff%in%c(symptom2_names,"symptoms_none"))){
    dat[dat$survey_version==2,ff] <- "not_asked"
  }
  dat[c(dat[,ff]==""),ff] <- "False"
  dat[,ff] <- droplevels(dat[,ff])
  
}

### symptoms among those feeling well
well_symptom_names <- c("symptoms_well_chills",
                        "symptoms_well_cough",
                        "symptoms_well_headache",
                        "symptoms_well_loss_of_taste_and_or_smell",
                        "symptoms_well_low_grade_fever",
                        "symptoms_well_mild_fatigue",
                        "symptoms_well_muscle_and_joint_pain",
                        "symptoms_well_nasal_congestion",
                        "symptoms_well_runny_nose",
                        "symptoms_well_tight_feeling_in_chest")
well_symptom_names_all <- c("symptoms_well_none",
                            well_symptom_names)

## not asked before survey 3: replace with the mainsymptoms
for(ff in well_symptom_names_all){
  levels(dat[,ff]) <- c(levels(dat[,ff]),"False","not_asked")
  dat[dat$survey_version<3,ff] <- "not_asked"
  dat[c(dat[,ff]==""),ff] <- "False"
  dat[,ff] <- droplevels(dat[,ff])
}
dat[dat$survey_version<3,"symptoms_well_chills"] <- "False"
dat[dat$survey_version<3,"symptoms_well_headache"] <- "False"
dat[dat$survey_version<3,"symptoms_well_loss_of_taste_and_or_smell"] <- "False"
dat[dat$survey_version<3,"symptoms_well_low_grade_fever"] <- "False"
dat[dat$survey_version<3,"symptoms_well_mild_fatigue"] <- "False"
dat[dat$survey_version<3,"symptoms_well_muscle_and_joint_pain"] <- "False"
dat[dat$survey_version<3,"symptoms_well_nasal_congestion"] <- "False"
dat[dat$survey_version<3,"symptoms_well_runny_nose"] <- "False"
dat[dat$survey_version<3,"symptoms_well_tight_feeling_in_chest"] <- "False"
dat[dat$survey_version<3 & dat$feeling=="well","symptoms_well_chills"] <- dat[dat$survey_version<3 & dat$feeling=="well","symptoms_chills_shaking"]
dat[dat$survey_version<3 & dat$feeling=="well","symptoms_well_headache"] <- dat[dat$survey_version<3 & dat$feeling=="well","symptoms_headache"]
dat[dat$survey_version<3 & dat$feeling=="well","symptoms_well_loss_of_taste_and_or_smell"] <- dat[dat$survey_version<3 & dat$feeling=="well","symptoms_loss_of_taste_and_or_smell"]
dat[dat$survey_version<3 & dat$feeling=="well","symptoms_well_low_grade_fever"] <- dat[dat$survey_version<3 & dat$feeling=="well","symptoms_fever"]
dat[dat$survey_version<3 & dat$feeling=="well","symptoms_well_mild_fatigue"] <- dat[dat$survey_version<3 & dat$feeling=="well","symptoms_fatigue"]
dat[dat$survey_version<3 & dat$feeling=="well","symptoms_well_muscle_and_joint_pain"] <- dat[dat$survey_version<3 & dat$feeling=="well","symptoms_muscle_and_joint_pain"]
dat[dat$survey_version<3 & dat$feeling=="well","symptoms_well_nasal_congestion"] <- dat[dat$survey_version<3 & dat$feeling=="well","symptoms_nasal_congestion"]
dat[dat$survey_version<3 & dat$feeling=="well","symptoms_well_runny_nose"] <- dat[dat$survey_version<3 & dat$feeling=="well","symptoms_runny_nose"]
dat[dat$survey_version<3 & dat$feeling=="well","symptoms_well_tight_feeling_in_chest"] <- dat[dat$survey_version<3 & dat$feeling=="well","symptoms_tight_feeling_in_chest"]
dat[dat$survey_version<3,"symptoms_well_cough"] <- "False"
dat[dat$survey_version<3 & dat$feeling=="well" & (dat[,"symptoms_dry_cough"]=="True" | dat[,"symptoms_wet_cough"]=="True"),"symptoms_well_cough"] <- "True"


### symptoms reported by those feeling well in survey 2
dat$symptoms_well_confusion  <- "False"
dat$symptoms_well_diarrhea <- "False"
dat$symptoms_well_loss_of_appetite <- "False"
dat$symptoms_well_nausea_and_vomiting <- "False"
dat$symptoms_well_shortness_of_breath <- "False"
dat$symptoms_well_sore_throat <- "False"
dat$symptoms_well_confusion[dat$survey_version==2 & dat$feeling=="well"] <- as.character(dat$symptoms_confusion[dat$survey_version==2 & dat$feeling=="well"])      
dat$symptoms_well_diarrhea[dat$survey_version==2 & dat$feeling=="well"] <- as.character(dat$symptoms_diarrhea[dat$survey_version==2 & dat$feeling=="well"])        
dat$symptoms_well_loss_of_appetite[dat$survey_version==2 & dat$feeling=="well"] <- as.character(dat$symptoms_loss_of_appetite[dat$survey_version==2 & dat$feeling=="well"])
dat$symptoms_well_nausea_and_vomiting[dat$survey_version==2 & dat$feeling=="well"] <- as.character(dat$symptoms_nausea_and_vomiting[dat$survey_version==2 & dat$feeling=="well"])
dat$symptoms_well_shortness_of_breath[dat$survey_version==2 & dat$feeling=="well"] <- as.character(dat$symptoms_shortness_of_breath[dat$survey_version==2 & dat$feeling=="well"])
dat$symptoms_well_sore_throat[dat$survey_version==2 & dat$feeling=="well"] <- as.character(dat$symptoms_sore_throat[dat$survey_version==2 & dat$feeling=="well"])       
dat$symptoms_well_confusion[dat$survey_version>2] <- "not_asked"
dat$symptoms_well_diarrhea[dat$survey_version>2] <- "not_asked"
dat$symptoms_well_loss_of_appetite[dat$survey_version>2] <- "not_asked"
dat$symptoms_well_nausea_and_vomiting[dat$survey_version>2] <- "not_asked"
dat$symptoms_well_shortness_of_breath[dat$survey_version>2] <- "not_asked"
dat$symptoms_well_sore_throat[dat$survey_version>2] <- "not_asked"
dat$symptoms_well_confusion  <- factor(dat$symptoms_well_confusion,levels=c("False","True","not_asked"))
dat$symptoms_well_diarrhea <- factor(dat$symptoms_well_diarrhea,levels=c("False","True","not_asked"))
dat$symptoms_well_loss_of_appetite <- factor(dat$symptoms_well_loss_of_appetite,levels=c("False","True","not_asked"))
dat$symptoms_well_nausea_and_vomiting <- factor(dat$symptoms_well_nausea_and_vomiting,levels=c("False","True","not_asked"))
dat$symptoms_well_shortness_of_breath <- factor(dat$symptoms_well_shortness_of_breath,levels=c("False","True","not_asked"))
dat$symptoms_well_sore_throat <- factor(dat$symptoms_well_sore_throat,levels=c("False","True","not_asked"))


### now include only symptoms among unwell
colnames(dat)[colnames(dat)%in%c(all_symptom_names,"symptoms_none")] <- paste0("not_well_",colnames(dat)[colnames(dat)%in%c(all_symptom_names,"symptoms_none")])
## not asked before survey 3: replace with the mainsymptoms
for(ff in paste0("not_well_",c(all_symptom_names,"symptoms_none"))){
  dat[dat$survey_version==2 & dat$feeling=="well" & dat[,ff]=="True",ff] <- "False"
}
## remove original vars
dat <- dat[,!colnames(dat)%in%c(all_symptom_names,"symptoms_none")]


### combine loss of taste and loss of smell to loss of taste or smell
dat$not_well_symptoms_loss_of_taste_and_or_smell[dat$survey_version==4] <- "False"
dat$not_well_symptoms_loss_of_taste_and_or_smell[dat$survey_version==4 & (dat$not_well_symptoms_loss_of_taste=="True" | dat$not_well_symptoms_loss_of_smell=="True")] <- "True"
dat$not_well_symptoms_loss_of_taste_and_or_smell <- droplevels(dat$not_well_symptoms_loss_of_taste_and_or_smell)
dat <- dat[,!colnames(dat)%in%c("not_well_symptoms_loss_of_taste","not_well_symptoms_loss_of_smell")]



### new names for symptom vars
new_all_symptom_names <- paste0("not_well_",all_symptom_names)
new_all_symptom_names <- new_all_symptom_names[!new_all_symptom_names%in%c("not_well_symptoms_none","not_well_symptoms_loss_of_taste","not_well_symptoms_loss_of_smell")] 
new_well_symptom_names <- c(well_symptom_names,
                            "symptoms_well_confusion",
                            "symptoms_well_diarrhea",
                            "symptoms_well_loss_of_appetite",
                            "symptoms_well_nausea_and_vomiting",
                            "symptoms_well_shortness_of_breath",
                            "symptoms_well_sore_throat")



### as a final check, fix any observations that reported feeling well, yet reported not_well symptoms (<10 obs)
## not asked before survey 3: replace with the mainsymptoms
for(ff in new_all_symptom_names){
  dat[dat$feeling=="well" & c(dat[,ff]=="True"),ff] <- "False"
}
for(ff in new_well_symptom_names){
  dat[dat$feeling=="not_well" & c(dat[,ff]=="True"),ff] <- "False"
}



### create var for no symptoms reported 
##### different from the var symptoms_none, which was an option given
dat$not_well_symptoms_none_reported <- "False"
dat$not_well_symptoms_none_reported[apply(dat[,new_all_symptom_names],1,function(x) sum(x=="True"))==0] <- "True"
dat$not_well_symptoms_none_reported <- factor(dat$not_well_symptoms_none_reported,levels=c("False","True"))
dat$symptoms_well_none_reported <- "False"
dat$symptoms_well_none_reported[apply(dat[,new_well_symptom_names],1,function(x) sum(x=="True"))==0] <- "True"
dat$symptoms_well_none_reported <- factor(dat$symptoms_well_none_reported,levels=c("False","True"))





###########################
### symptoms at positive test

covid_19_ext_symptoms_names<-c("covid_19_ext_symptoms_abdominal_pain",
                               "covid_19_ext_symptoms_chills_shaking",
                               "covid_19_ext_symptoms_diarrhea",
                               "covid_19_ext_symptoms_dry_cough",
                               "covid_19_ext_symptoms_facial_numbness",
                               "covid_19_ext_symptoms_fever",
                               "covid_19_ext_symptoms_irregular_heartbeat",
                               "covid_19_ext_symptoms_loss_of_appetite",
                               "covid_19_ext_symptoms_loss_of_smell",
                               "covid_19_ext_symptoms_loss_of_taste",
                               "covid_19_ext_symptoms_muscle_and_joint_pain",
                               "covid_19_ext_symptoms_nasal_congestion",
                               "covid_19_ext_symptoms_nausea_and_vomiting",
                               "covid_19_ext_symptoms_none",
                               "covid_19_ext_symptoms_runny_nose",
                               "covid_19_ext_symptoms_shortness_of_breath",
                               "covid_19_ext_symptoms_sore_throat",
                               "covid_19_ext_symptoms_tight_feeling_in_chest",
                               "covid_19_ext_symptoms_tingling_sensation",
                               "covid_19_ext_symptoms_tinnitus",
                               "covid_19_ext_symptoms_wet_cough")

for(ff in covid_19_ext_symptoms_names){
  levels(dat[,ff]) <- c(levels(dat[,ff]),"False","not_asked")
  dat[dat$covid_19_swab_test_tested!="True" & dat$covid_19_antibodies_test_tested!="True",ff] <- "not_asked"
  dat[c(dat[,ff]==""),ff] <- "False"
  dat[,ff] <- droplevels(dat[,ff])
}




###########################
### location

location_names <- c("location_admin_level_1",
                    "location_admin_level_2",
                    "location_country",
                    "location_locality")

# location_locality
## if not valid
dat$zipcode[dat$zipcode>99999] <- NA


##########################
## Testing Related

### tested
levels(dat$covid_19_test_tested) <- c(levels(dat$covid_19_test_tested),"not_recorded")
dat$covid_19_test_tested[dat$covid_19_swab_test_tested=="True" | dat$covid_19_antibodies_test_tested=="True"] <- "True"
dat$covid_19_test_tested[dat$survey_version==4 & as.Date(dat$date)<as.Date("2020-05-01")] <- "not_recorded"

### test results
## first remove any falsely coded "not_tested"
dat$covid_19_antibodies_test_result[dat$covid_19_antibodies_test_result=="not_tested"] <- ""
dat$covid_19_swab_test_result[dat$covid_19_swab_test_result=="not_tested"] <- ""
## merge
levels(dat$covid_19_test_result) <- c(levels(dat$covid_19_test_result),"not_recorded")
dat$covid_19_test_result[dat$covid_19_antibodies_test_result!=""] <- dat$covid_19_antibodies_test_result[dat$covid_19_antibodies_test_result!=""]
dat$covid_19_test_result[dat$covid_19_swab_test_result!=""] <- dat$covid_19_swab_test_result[dat$covid_19_swab_test_result!=""]
dat$covid_19_test_result[dat$survey_version==4 & as.Date(dat$date)<as.Date("2020-05-01")] <- "not_recorded"

### test date
## merge
dat$covid_19_test_date <- as.character(dat$covid_19_test_date)
dat$covid_19_test_date[dat$covid_19_antibodies_test_date!=""] <- as.character(dat$covid_19_antibodies_test_date)[dat$covid_19_antibodies_test_date!=""]
dat$covid_19_test_date[dat$covid_19_swab_test_date!=""] <- as.character(dat$covid_19_swab_test_date)[dat$covid_19_swab_test_date!=""]
dat$covid_19_test_date[dat$survey_version==4 & as.Date(dat$date)<as.Date("2020-05-01")] <- "not_recorded"
## clean any nonsense dates
bad_dates <- as.Date(unique(dat$covid_19_test_date)[!unique(dat$covid_19_test_date)%in%c("","not_recorded")])
bad_dates <- bad_dates[as.Date(bad_dates)<as.Date("2020-01-01")]
dat$covid_19_test_date[dat$covid_19_test_date%in%as.character(bad_dates)] <- ""
dat$covid_19_test_date <- factor(dat$covid_19_test_date)

### deprecated
# levels(dat$covid_19_test_tested) <- c(levels(dat$covid_19_test_tested),"not_recorded")
# dat$covid_19_test_tested[dat$covid_19_swab_test_tested=="True" | dat$covid_19_antibodies_test_tested=="True"] <- "True"
# dat$covid_19_test_tested[dat$survey_version==4 & as.Date(dat$date)<as.Date("2020-05-01")] <- "not_recorded"
# 
# levels(dat$covid_19_test_result) <- c(levels(dat$covid_19_test_result),"not_recorded")
# dat$covid_19_test_result[dat$covid_19_antibodies_test_result!=""] <- dat$covid_19_antibodies_test_result[dat$covid_19_antibodies_test_result!=""]
# dat$covid_19_test_result[dat$covid_19_swab_test_result!=""] <- dat$covid_19_swab_test_result[dat$covid_19_swab_test_result!=""]
# dat$covid_19_test_result[dat$survey_version==4 & as.Date(dat$date)<as.Date("2020-05-01")] <- "not_recorded"
# 
# levels(dat$covid_19_test_date) <- c(levels(dat$covid_19_test_date),"not_recorded")
# dat$covid_19_test_date[dat$covid_19_antibodies_test_date!=""] <- dat$covid_19_antibodies_test_date[dat$covid_19_antibodies_test_date!=""]
# dat$covid_19_test_date[dat$covid_19_swab_test_date!=""] <- dat$covid_19_swab_test_date[dat$covid_19_swab_test_date!=""]
# dat$covid_19_test_date[dat$survey_version==4 & as.Date(dat$date)<as.Date("2020-05-01")] <- "not_recorded"


test_names <- c("covid_19_test_isolation",
                "influenza_test_date",
                "influenza_test_result",       
                "influenza_test_tested",        
                "other_test_tested" )

## unclear what is going on with variable coding
colnames(dat)[colnames(dat)%in%test_names] <- paste0("notclean_",colnames(dat)[colnames(dat)%in%test_names])


##########################
## Covid-19 related

covid_names <- c("covid_19_exposed_quarantine",
                 "covid_19_other_exposed",
                 "covid_19_other_lives_in_household",
                 "covid_19_other_test_date",
                 "covid_19_self_suspicion",
                 "covid_19_symptoms_in_household" )
ext_names <- c("covid_19_ext_exposed_quarantine",
               "covid_19_ext_other_exposed",
               "covid_19_ext_other_lives_in_household",
               "covid_19_ext_other_test_date",
               "covid_19_ext_self_suspicion",
               "covid_19_ext_symptoms_in_household")



## unclear what is going on with variable coding
colnames(dat)[colnames(dat)%in%covid_names] <- paste0("notclean_",colnames(dat)[colnames(dat)%in%covid_names])
colnames(dat)[colnames(dat)%in%ext_names] <- paste0("notclean_",colnames(dat)[colnames(dat)%in%ext_names])



### save dataset
write.csv(dat,paste0("/home/user/Dropbox/HWF_data/hwf_clean_",Sys.Date(),".csv"),col.names=TRUE,row.names=FALSE)
# write.csv(dat,paste0("/ncf/xlin_covid/data/hwf_clean_",Sys.Date(),".csv"),col.names=TRUE,row.names=FALSE)

# write.csv(dat,paste0("hwf_clean_",Sys.Date(),".csv"),col.names=TRUE,row.names=FALSE)



