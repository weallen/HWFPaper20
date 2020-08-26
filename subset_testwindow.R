library(purrr)
library(data.table)

#' # Setup

#' The purpose of this code is to subset the HWF data to only include
#' responses within a certain time window of the test date.

#' For individuals who were tested, we restrict their responses to the
#' time two weeks before and after their first test date. For
#' individuals who ever tested positive, the first test date is the
#' first date positive test. Otherwise, the first test date is the
#' earliest test date.

#' For individuals who were not tested, we use the date one week
#' before their last response as the pseudo test date. We take data two
#' weeks before the pseudo test date and one week after the pseudo test
#' date.

date_touse <- "2020-05-15"

#' We read the cleaned data from `r date_touse`. We use the column `X`
#' as the row id. We read the predictions file that Han sent.

data_path <- sprintf("/ncf/xlin_covid/data/hwf_clean_%s.csv",
                     date_touse)
dat_raw <- fread(data_path)
stopifnot(length(unique(dat_raw$X)) == nrow(dat_raw))
setnames(dat_raw, old = "X", new = "rowid")
setkey(dat_raw, session_id, rowid)

preds_df <- fread("testpos-preds_2020-05-19.csv")[, -1]
setkey(preds_df, session_id)

#' We define a new version of `table` and `xtabs` that by default
#' display NA values.

table_na <- partial(table, useNA = "always")
xtabs_na <- partial(xtabs, addNA = TRUE)

#' # Data Wrangling

#' We look at the dates that people put in for testing. We see that some
#' people put in some nonsensical dates.

dat_raw[,
  covid_19_swab_test_date_NEW := as.Date(covid_19_swab_test_date,
                                         format = "%Y-%m-%d")
]

table_na(dat_raw$covid_19_swab_test_date_NEW)

#' We subset the responses to only include those who got tested after
#' Jan 31, 2020.

rows_tested <- dat_raw[covid_19_swab_test_tested == TRUE &
                      covid_19_swab_test_result != "dont_know" &
                      covid_19_swab_test_date_NEW > as.Date("2020-01-31"),]
table_na(rows_tested$covid_19_swab_test_date_NEW)
table_na(rows_tested$covid_19_swab_test_tested)
table_na(rows_tested$covid_19_swab_test_result)

both_ids <- intersect(unique(preds_df$session_id),
                      unique(rows_tested$session_id))
table(dat_raw[session_id %in% both_ids, .(covid_19_test_tested)])
table(dat_raw[session_id %in% both_ids, .(covid_19_swab_test_tested)])

#' ## Date of First Test

#' We need to find the date that each user got tested. For users who got
#' tested multiple times, we'll take the date of their first positive
#' test. If someone didn't test positive, we then take the date of their
#' first test.

#' First, we recode the test result as a numeric value and find users
#' who tested positive at least once.

rows_tested[covid_19_swab_test_result == "positive",
            covid_19_swab_test_result_bin := 1]
rows_tested[covid_19_swab_test_result == "negative",
            covid_19_swab_test_result_bin := 0]
table_na(rows_tested$covid_19_swab_test_result_bin)
rows_tested[,
    swab_test_any_pos := max(covid_19_swab_test_result_bin),
    by = session_id]

#' Next, we find users who report having both a positive test and a
#' negative test on the same day and exclude them from the
#' analysis---these are likely to be data entry errors.

# these users have both positive and negative tests.
neg_and_pos <- rows_tested[
  swab_test_any_pos == 1 & covid_19_swab_test_result == "negative",
  .(covid_19_swab_test_result, covid_19_swab_test_date_NEW, session_id, rowid)]
rows_neg_and_pos <- rows_tested[session_id %in% neg_and_pos$session_id,]

# Function to calculate if there are conflicting test results.
# If the sum of positive test results is equal to 0 or to the number of
# tests, then this person consistently tested negative or positive
# during a specific date.

conflicting_tests <- function(test_results) {
  sum_tests <- sum(test_results)
  n_tests <- length(test_results)
  return(!(sum_tests == 0 || sum_tests == n_tests))
}
conflicting <- rows_neg_and_pos[,
  .(conflicting = conflicting_tests(covid_19_swab_test_result_bin)),
  by = .(session_id, covid_19_swab_test_date_NEW)]

conflicting_rowids <- rows_neg_and_pos[
  session_id %in% conflicting$session_id &
  covid_19_swab_test_date_NEW %in% conflicting$covid_19_swab_test_date_NEW,
  rowid]

# remove those with conflicting test results
rows_tested <- rows_tested[!(rowid %in% conflicting_rowids),]

#' For those who tested positive at any time, we take the date of their
#' first positive test. For those who never tested positive, we take
#' their first test date.

# data tables that contain session ID and date of first test.
first_test_date_pos <- rows_tested[
  swab_test_any_pos == 1 &
  covid_19_swab_test_result_bin == 1,
  .(first_test_date = min(covid_19_swab_test_date_NEW)),
  by = session_id]
first_test_date_neg <- rows_tested[
  swab_test_any_pos == 0,
  .(first_test_date = min(covid_19_swab_test_date_NEW)),
  by = session_id]

# Test that there is no overlap between those who tested positive at
# least once and those who never tested positive.
stopifnot(length(base::intersect(first_test_date_pos$session_id,
                                 first_test_date_neg$session_id)) == 0)
first_test_date_df <- rbind(first_test_date_pos, first_test_date_neg)

# data frame of session ID and test result
# swab_test_any_pos is 1 if they ever tested positive on a swab test.
test_res_df <- rows_tested[,
  .(swab_test_any_pos = max(covid_19_swab_test_result_bin)),
  by = session_id]
stopifnot(nrow(first_test_date_df) == nrow(test_res_df))

# merge the two together
test_df <- merge(test_res_df, first_test_date_df, by = "session_id")
stopifnot(nrow(test_df) == nrow(first_test_date_df))

#' ## Merging and responses within two weeks of test

#' Next, we get all the responses within two weeks of each user's test.
#' For those who actually got tested, we use the user's first test date
#' calculated in the previous section. For users who didn't get tested,
#' we take the date that's 1 week before their last check-in as the test
#' date---this is because the prediction uses data two weeks before the
#' last check-in to predict the SARS-CoV-2 test result for those users.

dat_raw[, date_NEW := as.Date(date, format = "%Y-%m-%d")]

#' First, we do this with only the session_ids which ever got tested. We
#' merge the testing data with responses and take all responses two
#' weeks before the test date and two weeks after the test date. We
#' write this out.

#' Each of the tested, untested, and predicted data will have a
#' `days_since_test` column, a `tested_predicted_indicator` column, and
#' a `positive` column indicating days since test, a string for either
#' tested, untested, or predicted, and a binary indicator for whether
#' they were ever positive.

# this does a right join
rows_with_test <- dat_raw[test_df, on = "session_id"]
df_tested <- rows_with_test[first_test_date - 14 <= date_NEW &
                            date_NEW <= first_test_date + 14, ]
df_tested[, days_since_test := as.double(date_NEW - first_test_date,
                                         units = "days")]
df_tested[, tested_predicted_indicator := "Tested"]
df_tested[, positive := swab_test_any_pos]
uniqueN(df_tested$session_id)
saveRDS(df_tested, file = "df_tested.rds")
rm(df_tested, rows_with_test, rows_tested)

#' Next, we do the same to the untested session_ids. We use the date 7
#' days before last checkin as the pseudo test date and take two weeks
#' before the last checkin and 1 week after the last checkin. The reason
#' why we separate untested and predicted is that there are some people
#' who were tested that we did predictions on. They were people who were
#' tested in e.g. March before the V4 survey came out.

dat_untested <- dat_raw[!test_df, on = "session_id"]
dat_untested[, pseudo_test_date := max(date_NEW) - 7, by = session_id]
df_untested <- dat_untested[pseudo_test_date - 14 <= date_NEW &
                            date_NEW <= pseudo_test_date + 7, ]
df_untested[, days_since_test := as.double(date_NEW - pseudo_test_date,
                                         units = "days")]
df_untested[, tested_predicted_indicator := "Untested"]
df_untested[, positive := NA]
uniqueN(df_untested$session_id)
saveRDS(df_untested, file = "df_untested.rds")
rm(df_untested, dat_untested)


#' Finally, we do the same with predictions. We merge prediction data
#' with responses and take those responses 2 weeks before the pseudo
#' test date and 1 week after the pseudo test date. The test date was 1
#' week before the last checkin.

preds_df[, pseudo_test_date := as.Date(pseudo_test_date,
                                       format = "%Y-%m-%d")]
dat_with_preds <- dat_raw[preds_df, on = "session_id"]
df_predicted <- dat_with_preds[pseudo_test_date - 14 <= date_NEW &
                               date_NEW <= pseudo_test_date + 7, ]

df_predicted[, days_since_test := as.double(date_NEW - pseudo_test_date,
                                            units = "days")]
stopifnot(all(df_predicted[, uniqueN(prediction),
              by = session_id]$V1 == 1))

# see the number of unique session IDs
uniqueN(df_predicted$session_id)

df_tested[, tested_predicted_indicator := "Tested"]
df_tested[, positive := swab_test_any_pos]

#' Using this threshold set by Han, we take anyone with a predicted
#' positive probability above this threshold as a predicted positive.

threshold <- 0.23011175
df_predicted[, tested_predicted_indicator := "Predicted"]
df_predicted[, positive := as.integer(prediction > threshold)]
saveRDS(df_predicted, file = "df_predicted.rds")
rm(df_predicted, dat_with_preds)

sessionInfo()
