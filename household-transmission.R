#' ---
#' title: 'How We Feel Analysis: Household Transmission'
#' author: "Andy Shi"
#' date: "`r Sys.Date()`"
#' output: html_document
#' ---

#' # Setup

#' We are interested in studying how having a member of one's household
#' test positive for SARS-CoV-2 will affect one's own probability of
#' testing positive. Here, we load the necessary libraries and read the
#' data.

library(dplyr)
library(ggplot2)
library(data.table)
library(broom)
library(knitr)
library(purrr)
library(stringr)
library(logistf)

#' This data is generated from `subset_testwindow.R`. This data contains
#' the responses only from individuals who were tested, and were
#' restricted to the time two weeks before and after their first test
#' date. For individuals who ever tested positive, the first test date
#' is the first date positive test. Otherwise, the first test date is
#' the earliest test date.

#' We then restrict the data to survey version 4 only, because we are
#' going to be using symptom data.

dat_raw <- readRDS("df_tested.rds")
dat_raw <- dat_raw[survey_version == 4,]


#' We look at some contingency tables of the data. Specifically, we
#' verify that only the people who answered "yes" to `other_exposed`
#' answered the question about household exposure.

xtabs(~ notclean_covid_19_ext_other_exposed +
        notclean_covid_19_ext_other_lives_in_household,
      data = dat_raw, addNA = TRUE)

xtabs(~ notclean_covid_19_ext_other_exposed +
        notclean_covid_19_ext_exposed_quarantine,
      data = dat_raw, addNA = TRUE)
xtabs(~ notclean_covid_19_ext_other_exposed +
        notclean_covid_19_ext_symptoms_in_household,
      data = dat_raw, addNA = TRUE)
xtabs(~ notclean_covid_19_ext_other_lives_in_household +
        notclean_covid_19_ext_symptoms_in_household,
      data = dat_raw, addNA = TRUE)
xtabs(~ notclean_covid_19_ext_other_exposed +
        format(as.Date(notclean_covid_19_ext_other_test_date,
                       format = "%Y-%m-%d", optional = TRUE), "%Y-%m"),
      data = dat_raw, addNA = TRUE)

xtabs(~ notclean_covid_19_ext_other_lives_in_household +
      household_members_count,
      data = dat_raw, addNA = TRUE)

#' # Data Munging

#' ## Recoding Variables

#' We recode the household exposure as a binary variable. It's 1 iff
#' a respondent answered yes to the household exposure question.
#' Otherwise it's 0.

dat_raw[, covid_19_in_household := 0]
dat_raw[notclean_covid_19_ext_other_lives_in_household == "yes",
        covid_19_in_household := 1]

#' We also recode whether they left the house so we can turn that into a
#' frequency.

table(dat_raw$combined_stayed_home)
dat_raw[combined_stayed_home == "True",
           stayed_home_numeric := 1]
dat_raw[combined_stayed_home == "False",
           stayed_home_numeric := 0]
dat_raw[combined_stayed_home == "missing",
           stayed_home_numeric := NA]
table(dat_raw$stayed_home_numeric, useNA = "always")

#' We also recode household size as a factor.

table(dat_raw$household_members_count, useNA = "ifany")
dat_raw[!is.na(household_members_count) &
           household_members_count <= 4,
           household_size := as.character(household_members_count)]
dat_raw[!is.na(household_members_count) &
           household_members_count == 5,
           household_size := "5+"]
dat_raw[is.na(household_members_count),
           household_size := "Not reported"]
table(dat_raw$household_size, useNA = "ifany")

#' ## Munging Symptoms

#' We merge the well and not well symptoms according th
#' `symptom_merge_lst`.

colnames(dat_raw)[startsWith(colnames(dat_raw), "symptoms")]
colnames(dat_raw)[startsWith(colnames(dat_raw), "not_well")]
symptom_merge_lst <- list(
  symptoms_cough = c("not_well_symptoms_dry_cough", "not_well_symptoms_wet_cough"),
  symptoms_chills_shaking = c("not_well_symptoms_chills_shaking", "symptoms_well_chills"),
  symptoms_loss_of_taste_and_or_smell = c("not_well_symptoms_loss_of_taste_and_or_smell",
                                          "symptoms_well_loss_of_taste_and_or_smell"),
  symptoms_diarrhea = c("not_well_symptoms_diarrhea"),
  symptoms_loss_of_appetite = c("not_well_symptoms_loss_of_appetite",
                                "symptoms_well_loss_of_appetite"),
  symptoms_tight_feeling_in_chest = c("not_well_symptoms_tight_feeling_in_chest",
                                      "symptoms_well_tight_feeling_in_chest"),
  symptoms_muscle_and_joint_pain = c("not_well_symptoms_muscle_and_joint_pain",
                                     "symptoms_well_muscle_and_joint_pain"),
  symptoms_nasal_congestion = c("not_well_symptoms_nasal_congestion",
                                "symptoms_well_nasal_congestion"),
  symptoms_shortness_of_breath = c("not_well_symptoms_shortness_of_breath"),
  symptoms_nausea_and_vomiting = c("not_well_symptoms_nausea_and_vomiting"),
  symptoms_runny_nose = c("symptoms_well_runny_nose", "not_well_symptoms_runny_nose"),
  symptoms_sore_throat = c("not_well_symptoms_sore_throat"),
  symptoms_fatigue = c("not_well_symptoms_fatigue", "symptoms_well_mild_fatigue"),
  symptoms_fever = c("symptoms_well_low_grade_fever", "not_well_symptoms_fever"),
  symptoms_facial_numbness = c("not_well_symptoms_facial_numbness"),
  symptoms_tingling_sensation = c("not_well_symptoms_tingling_sensation"))

munge_symptoms <- function(df, symptom_merge_lst, na_value = 0) {
  for (i in seq_along(symptom_merge_lst)) {
    new_symptom <- names(symptom_merge_lst)[i]
    old_symptoms <- symptom_merge_lst[[i]]
    subset_df <- df[, ..old_symptoms]
    col_classes <- sapply(subset_df, class)
    character_cols <- old_symptoms[col_classes == "character"]
    nonchar_cols_classes <- col_classes[col_classes != "character"]
    nonchar_cols <- old_symptoms[col_classes != "character"]
    stopifnot(all.equal(nonchar_cols_classes,
                        rep("logical", length(nonchar_cols)),
                        identical = TRUE, check.attributes = FALSE))

    n_char_cols <- length(character_cols)
    if (n_char_cols > 0) {
      converted_cols <- paste0(character_cols, "_conv")
      for (j in 1:n_char_cols) {
        cat(sprintf("This symptom is a character: %s\n",
                    character_cols[j]))
        print(table(df[[character_cols[j]]]))
        cat("\n")
        subset_df[get(character_cols[j]) == "True", (converted_cols[j]) := 1]
        subset_df[get(character_cols[j]) == "False", (converted_cols[j]) := 0]
        subset_df[get(character_cols[j]) == "not_asked",
                  (converted_cols[j]) := na_value]
      }
      cols_to_sum <- c(nonchar_cols, converted_cols)
    } else {
      cols_to_sum <- nonchar_cols
    }
    # Probably OK to sum because someone won't have well_symptom and
    # notwell_symptom on the same response. But we haven't checked this
    # and it's probably safer to take the max.
    new_symptom_summed <- rowSums(subset_df[, ..cols_to_sum])
    df[, (new_symptom) := new_symptom_summed]
  }
}

#' We then take the average number of responses in the time window
#' considered that each person reported each symptom. Note: a unique
#' session_id is considered a unique person.

munge_symptoms(dat_raw, symptom_merge_lst)
symptoms_collapsed <- dat_raw[, lapply(.SD, mean, na.rm = TRUE),
                              by = session_id,
                              .SDcols = names(symptom_merge_lst)]

#' ## Munge Sleep

#' For each category of sleep, we take the average number of responses
#' over the time window where each person reported that category.

table(dat_raw$sleep)
dat_raw[, sleep_fct := relevel(factor(sleep), ref = "7_to_8_hours")]
sleep_df <- data.table(model.matrix(~sleep_fct, data = dat_raw))
sleep_df$session_id <- dat_raw$session_id
sleep_df[, ("(Intercept)") := NULL]
summary(sleep_df)
sleep_collapsed <- sleep_df[, lapply(.SD, mean), by = session_id]

#' ## Munge Pre-existing conditions

#' We first verify that each person's pre-existing conditions don't
#' change with each response.

preexisting <- colnames(dat_raw)[startsWith(colnames(dat_raw), "preexisting_")]
dat_raw[, lapply(.SD, uniqueN), by = session_id,
        .SDcols = preexisting]
check <- dat_raw[, lapply(.SD, uniqueN, na.rm = TRUE),
                    by = session_id, .SDcols = preexisting]
check_vec <- check[, lapply(.SD, max), .SDcols = !"session_id"]
check_vec

#' `check_vec` stores the maximum number of unique responses per
#' individual for each pre-existing condition. We see there is one
#' person who reported differing pre-existing conditions. I'm not sure
#' what's going on with this person so we remove them from the analysis.

# manual check and remove offending person
dat_raw[session_id == check[preexisting_not_say == 2, session_id], ]
dat_raw <- dat_raw[session_id != check[preexisting_not_say == 2, session_id],]
check <- dat_raw[, lapply(.SD, uniqueN, na.rm = TRUE),
                    by = session_id, .SDcols = preexisting]
check_vec <- check[, lapply(.SD, max), .SDcols = !"session_id"]
stopifnot(as.integer(check_vec) == 1)

for (cond in preexisting) {
  cat(cond)
  cat("\n")
  print(table(dat_raw[, ..cond], useNA = "always"))
}

#' To collapse the pre-existing conditions, we take the unique
#' pre-existing condition for each person. Because some pre-existing
#' conditions are coded as booleans (TRUE/FALSE) and others are strings
#' ("True"/"False"), we deal with that below. If someone didn't report
#' a particular pre-exisiting condition, they were coded as NA. We
#' return 1 (TRUE) or 0 (FALSE) for each person for each pre-existing
#' condition.

collapse_preexisting <- function(preexisting_vec) {
  uniq_preexisting <- unique(preexisting_vec)
  na_idx <- is.na(uniq_preexisting)
  if (sum(na_idx) == length(uniq_preexisting)) {
    # If all NAs, turn to FALSE. This means they didn't report this
    # pre-existing condition.
    return(0L)
  } else {
    # otherwise, they reported pre-existing and later had NAs.
    uniq_preexisting_touse <- na.omit(uniq_preexisting)
    stopifnot(length(uniq_preexisting_touse) == 1)
    if (is.character(uniq_preexisting_touse)) {
      # if it's a character, return 1 if "True", 0 otherwise.
      return(ifelse(uniq_preexisting_touse == "True", 1L, 0L))
    }
    if (is.logical(uniq_preexisting_touse)) {
      # if logical, turn the logical into an integer.
      return(as.integer(uniq_preexisting_touse))
    }
    else {
      # Should not get here.
      stop("Error")
    }
  }
}
preexisting_collapsed <- dat_raw[, lapply(.SD, collapse_preexisting),
                                 by = session_id, .SDcols = preexisting]
preexisting_collapsed[, preexisting_none := NULL]


#' ## Collapsing by Session ID

#' Next, we collapse other variables by session_id (so each individual
#' has one response). Note: unique session_id's are treated as unique
#' individuals.

#' For these variables, we take the max over all responses.

vars1 <- c("covid_19_in_household",
           "household_members_count",
           "number_preexisting",
            "swab_test_any_pos")

max_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_integer_)
  } else {
    return(max(x, na.rm = TRUE))
  }
}
dat_collapsed1 <- dat_raw[, lapply(.SD, max_na),
                          by = session_id, .SDcols = vars1]
dat_collapsed1[number_preexisting >= 2, number_preexisting_cat := "2+"]
dat_collapsed1[number_preexisting == 1, number_preexisting_cat := "1"]
dat_collapsed1[number_preexisting == 0, number_preexisting_cat := "0"]
dat_collapsed1[, number_preexisting := NULL]

#' We also recode household members as a factor, since 5 actually means
#' 5+.
table(dat_collapsed1$household_members_count, useNA = "ifany")
dat_collapsed1[!is.na(household_members_count) &
               household_members_count <= 4,
             household_size := as.character(household_members_count)]
dat_collapsed1[!is.na(household_members_count) &
               household_members_count == 5,
               household_size := "5+"]
dat_collapsed1[is.na(household_members_count),
              household_size := "Not reported"]
dat_collapsed1 <- dat_collapsed1[household_members_count > 1, ]
table(dat_collapsed1$household_size, useNA = "ifany")

#' These variables should be the same across all responses for a
#' particular session_id. We verify that below and the collapse.

vars_simple <- c("race_cat",
                 "age_range_NEW",
                 "gender",
                 "profession_essential_cat",
                 "tested_predicted_indicator",
                 "positive",
                 "zipcode", "FIPS")
check <- dat_raw[, lapply(.SD, uniqueN, na.rm = TRUE),
                    by = session_id,
                    .SDcols = vars_simple]
check_vec <- check[, lapply(.SD, max), .SDcols = !"session_id"]
stopifnot(as.integer(check_vec) == 1)
dat_collapsed2 <- dat_raw[, lapply(.SD, unique), by = session_id,
                          .SDcols = vars_simple]

#' Next, we merge all the collapsed data together.

tmp1 <- dat_collapsed1[dat_collapsed2, on = "session_id", nomatch = NULL]
dim(tmp1)
tmp2 <- tmp1[sleep_collapsed, on = "session_id", nomatch = NULL]
dim(tmp2)
tmp3 <- tmp2[symptoms_collapsed, on = "session_id", nomatch = NULL]
dim(tmp3)
tmp4 <- tmp3[preexisting_collapsed, on = "session_id", nomatch = NULL]
dim(tmp4)

dat_collapsed <- tmp4

#' We modify the reference level of the factors.

dat_collapsed[,
  race_ethnicity := relevel(factor(race_cat), "white")]

table(dat_collapsed$profession_essential_cat)
dat_collapsed[,
  profession := relevel(factor(profession_essential_cat),
                        "nonessential")]
table(dat_collapsed$swab_test_any_pos, useNA = "ifany")

#' This shows that the final dataset we got only contains tested
#' individuals.

table(dat_collapsed$tested_predicted_indicator)

#' ## Merge in the population density and census income data.

#' We read the Census data and merge it with the ZIP codes, so we can
#' map ZIP codes to median income. We also read the Yu Group data and
#' compute the population density. The population density is by FIPS, so
#' we merge that with the FIPS in our data.

census_income = fread("data/census_income.csv", keepLeadingZeros = TRUE)
setnames(census_income,
         old = "MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS)",
         new = "median_income")
yugroup_dat = fread("data/Yu_Group_County_level_downloaded_04_14_2020_cleaned_demo.csv",
                    keepLeadingZeros = TRUE)
zip_to_zcta = fread("data/zip_to_zcta_2019.txt", keepLeadingZeros = TRUE)
setkey(census_income, "zip code tabulation area")
setkey(zip_to_zcta, "ZCTA")
census_zip <- census_income[zip_to_zcta[, .(ZCTA, ZIP_CODE)],
                            on = c("zip code tabulation area" = "ZCTA"),
                            nomatch = 0]
# Have to convert ZIP code and FIPS to 5 digits with leading zeros as
# necessary.
dat_collapsed[, `:=` (zipcode_str = sprintf("%05d", zipcode),
                      fips_str = sprintf("%05d", FIPS))]
dat_income_merged <- merge(dat_collapsed, census_zip, by.x = "zipcode_str",
                           by.y = "ZIP_CODE", all = FALSE, all.x = TRUE,
                           all.y = FALSE)

yugroup_dat[, pop_density := PopulationEstimate2018 / LandAreainSquareMiles2010]
popden_df <- yugroup_dat[, .(fips, pop_density)]
dat_income_pop <- merge(dat_income_merged, popden_df, by.x = "fips_str",
                        by.y = "fips", all = FALSE, all.x = TRUE,
                        all.y = FALSE)
stopifnot(nrow(dat_income_pop) == nrow(dat_collapsed))

#' We see there are some people where we were unable to find a
#' population density or median income. We get rid of the two people
#' where we can't find a population density. For the median income,
#' negative values of median income are NA, and we only have three of
#' them, so we'll remove those too.

summary(dat_income_pop[, .(pop_density, median_income)])
dat_income_pop[is.na(pop_density) | is.na(median_income) | median_income < 0, ]

dat_income_pop <- dat_income_pop[!is.na(pop_density) & !is.na(median_income) &
                                 median_income > 0, ]
summary(dat_income_pop[, .(pop_density, median_income)])

#' Binning population density and income

income_cuts <- c(0, 40000, 70000, 100000, Inf)
popden_cuts <- c(0, 150, 1000, 90000)
dat_income_pop[, `:=`
  (income_buckets = relevel(cut(median_income, income_cuts, right = FALSE),
                            "[1e+05,Inf)"),
   popden_buckets = relevel(cut(pop_density, popden_cuts, right = FALSE),
                            "[1e+03,9e+04)"))]
summary(dat_income_pop)


#' # Analysis

#' ## Village-Level Transmission

#' Check zip codes to verify that there isn't "village-level"
#' transmission. We see that among the 75 who are tested positive and
#' exposed in household, there are 68 unique zip codes.

dat_income_pop[, .(unique_zipcodes = uniqueN(zipcode)),
               by = .(covid_19_in_household, swab_test_any_pos)]

#' ## Contingency Table

xtabs(~ covid_19_in_household + swab_test_any_pos,
      data = dat_income_pop,
      addNA = TRUE)

dat_income_pop[, mean(swab_test_any_pos), by = covid_19_in_household]

#' ## Univariate Analysis

mod_uni <- glm(swab_test_any_pos ~ covid_19_in_household,
               data = dat_income_pop,
               family = binomial(link = "logit"))

tidy_uni <- tidy(mod_uni, conf.int = TRUE)
tidy_uni %>%
  mutate_if(is.numeric, list(~as.character(signif(., 3)))) %>%
  fwrite(file = "tidy_uni.csv")

tidy_uni %>%
  mutate_if(is.numeric, list(~as.character(signif(., 3)))) %>%
  kable(caption = "Regression coefficients from univariate model.")

tidy_uni %>%
  select(-std.error, -statistic, -p.value) %>%
  mutate_if(is.numeric, exp) %>%
  mutate_if(is.numeric, list(~as.character(signif(., 3)))) %>%
  kable(caption = "Exponentiated regression coefficients from univariate model.")

#' We also try Firth's correction because the data are imbalanced: we have very
#' few people who test positive.
firth_uni <- logistf(swab_test_any_pos ~ covid_19_in_household,
                     data = dat_income_pop, firth = TRUE)

tidy_firth_uni <- with(firth_uni,
    tibble(term = terms, estimate = coefficients,
           std.error = sqrt(diag(var)),
           statistic = coefficients^2 / diag(var),
           conf.low = ci.lower, conf.high = ci.upper)) %>%
  mutate(p.value = pchisq(statistic, 1, lower.tail = FALSE))

tidy_firth_uni %>%
  mutate_if(is.numeric, list(~as.character(signif(., 3)))) %>%
  kable(caption = "Regression coefficients from univariate model with Firth correction.")

tidy_firth_uni %>%
  mutate_if(is.numeric, list(~as.character(signif(., 3)))) %>%
  fwrite(file = "tidy_firth_uni.csv")

#' ## Multivariate Analysis

#' Here we define the columns we use for the analysis.

reg_cols <- c("covid_19_in_household", "swab_test_any_pos",
              "household_size", "age_range_NEW", "gender",
              "sleep_fct11_or_more_hours", "sleep_fct5_to_6_hours",
              "sleep_fct9_to_10_hours", "sleep_fctless_than_5",
              "symptoms_cough", "symptoms_chills_shaking",
              "symptoms_loss_of_taste_and_or_smell",
              "symptoms_diarrhea", "symptoms_loss_of_appetite",
              "symptoms_tight_feeling_in_chest",
              "symptoms_muscle_and_joint_pain",
              "symptoms_nasal_congestion",
              "symptoms_shortness_of_breath",
              "symptoms_nausea_and_vomiting", "symptoms_runny_nose",
              "symptoms_sore_throat", "symptoms_fatigue",
              "symptoms_fever", "symptoms_facial_numbness",
              "symptoms_tingling_sensation",
              "preexisting_chronic_lung_disease",
              "preexisting_allergies", "preexisting_asthma",
              "preexisting_autoimmune_disease", "preexisting_cancer",
              "preexisting_cardiovascular_disease",
              "preexisting_chronic_kidney_disease",
              "preexisting_diabetes", "preexisting_hypertension",
              "preexisting_immunodeficiency",
              "preexisting_liver_disease", "preexisting_not_say",
              "preexisting_pregnant", "race_ethnicity", "profession",
              "income_buckets", "popden_buckets")

mod_multi <- glm(swab_test_any_pos ~ .,
                 data = dat_income_pop[, ..reg_cols],
                 family = binomial(link = "logit"))
tidy_multi <- tidy(mod_multi, conf.int = TRUE)

tidy_multi %>%
  mutate_if(is.numeric, list(~as.character(signif(., 3)))) %>%
  fwrite(file = "tidy_multi.csv")

tidy_multi %>%
  mutate_if(is.numeric, list(~as.character(signif(., 3)))) %>%
  kable(caption = "Regression coefficients from multivariate model.")

tidy_multi %>%
  select(-std.error, -statistic, -p.value) %>%
  mutate_if(is.numeric, exp) %>%
  head()

#' We also try Firth regression here, too

firth_multi <- logistf(swab_test_any_pos ~ .,
                       data = dat_income_pop[, ..reg_cols])
firth_multi_sum <- summary(firth_multi)

tidy_firth_multi <- with(firth_multi_sum,
    tibble(term = terms, estimate = coefficients,
           std.error = sqrt(diag(var)),
           statistic = coefficients^2 / diag(var),
           conf.low = ci.lower, conf.high = ci.upper)) %>%
  mutate(p.value = pchisq(statistic, 1, lower.tail = FALSE))

tidy_firth_multi %>%
  mutate_if(is.numeric, list(~as.character(signif(., 3)))) %>%
  kable(caption = "Regression coefficients from multivariate model with Firth correction.")

tidy_firth_multi %>%
  mutate_if(is.numeric, list(~as.character(signif(., 3)))) %>%
  fwrite(file = "tidy_firth_multi.csv")

#' We also try an interaction between household size and
#' covid_19_in_household but we don't see anything significant.

reg_cols2 <- c(reg_cols[reg_cols != "household_size"], "household_fct")
dat_income_pop[household_size %in% c("2", "3"), household_fct := "3-"]
dat_income_pop[household_size %in% c("4", "5+"), household_fct := "4+"]
mod_interact <- glm(swab_test_any_pos ~ . +
                    covid_19_in_household:household_fct,
                    data = dat_income_pop[, ..reg_cols2],
                    family = binomial(link = "logit"))
summary(mod_interact)

sessionInfo()
