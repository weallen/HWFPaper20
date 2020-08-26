#' ---
#' title: 'How We Feel Analysis: Firth-Corrected Analysis'
#' author: "Andy Shi"
#' date: "`r Sys.Date()`"
#' output: html_document
#' ---

#' # Setup

#' We wanted to see if using Firth regression would change the results of our
#' analyses, since there were only a few number of people in our study who were
#' tested or who tested positive. Overall we found very little difference
#' between the outcome of vanilla logistic regression and Firth regression.


#' We use logistf R package to implement Firth regression.

library(dplyr)
library(data.table)
library(broom)
library(logistf)

#' We define a tidier for the logistf regression output and tidier for glm that
#' uses Wald CIs instead of profile likelihood CIs.

tidy.logistf <- function(fit) {
  ret <- with(fit,
    tibble(term = terms, estimate = coefficients,
           std.error = sqrt(diag(var)),
           statistic = coefficients^2 / diag(var),
           conf.low = ci.lower, conf.high = ci.upper)) %>%
    mutate(p.value = pchisq(statistic, 1, lower.tail = FALSE))
  return(ret)
}

tidy_logistic_naive <- function(fit, conf.level = 0.95) {
  zstar <- -qnorm((1 - conf.level)/2)
  ret <- with(fit,
    tibble(term = names(coefficients), estimate = coefficients,
           std.error = sqrt(diag(vcov(fit))))) %>%
    mutate(statistic = estimate / std.error,
           conf.low = estimate - zstar * std.error,
           conf.high = estimate + zstar * std.error,
           p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE))
    return(ret)
}

#' We define a function to plot the comparision of logit and Firth regression.
#' The inputs are two tidy dataframes with estimate, p.value, and std.error
#' columns.

plot_logit_firth <- function(tidy_logit, tidy_firth) {
  op <- par(mfrow = c(1, 3))

  plot(tidy_logit$estimate, tidy_firth$estimate,
       xlab = "Logit Regression Coefficient",
       ylab = "Firth Regression Coefficient",
       main = "Comparison of Coefficients")
  abline(a = 0, b = 1, lty = 2)

  plot(-log10(tidy_logit$p.value),
       -log10(tidy_firth$p.value),
       xlab = "-log10(Logit Reg. p-value)",
       ylab = "-log10(Firth Reg. p-value)",
       main = "Comparison of P-values")
  abline(a = 0, b = 1, lty = 2)

  plot(tidy_logit$std.error,
       tidy_firth$std.error,
       xlab = "Logit Regression SE",
       ylab = "Firth Regression SE",
       main = "Comparison of Std Errors")
  abline(a = 0, b = 1, lty = 2)

  par(op)
}

#' Here we read the featurized HWF data. This was generated using Han's code.

data_path <- "~/hwf_featurized.csv"
dat_feats <- fread(data_path)
setkey(dat_feats, session_id)

#' # Predicting Who Gets Tested

#' For this section, we used Wald confidence intervals instead of profile
#' likelihood confidence intervals because we had over 277,000 observations and
#' computing the profile likelihood confidence intervals would be really slow.

feats_tested <- c('West South Central', 'West North Central', 'South Atlantic',
                 'New England', 'Mountain', 'Middle Atlantic',
                 'East South Central', 'East North Central',
                 'sleep_11_or_more_hours',
                 'sleep_9_to_10_hours', 'sleep_5_to_6_hours',
                 'sleep_less_than_5', 'popden_1000+', 'popden_150-999',
                 'prof_other_essential', 'prof_healthcare',
                 'household_members_count_5', 'household_members_count_4',
                 'household_members_count_3', 'household_members_count_2',
                 'symptoms_tingling_sensation', 'symptoms_fever',
                 'symptoms_fatigue', 'symptoms_sore_throat',
                 'symptoms_runny_nose', 'symptoms_nausea_and_vomiting',
                 'symptoms_shortness_of_breath', 'symptoms_nasal_congestion',
                 'symptoms_muscle_and_joint_pain',
                 'symptoms_tight_feeling_in_chest', 'symptoms_loss_of_appetite',
                 'symptoms_diarrhea', 'symptoms_loss_of_taste_and_or_smell',
                 'symptoms_chills_shaking', 'symptoms_cough', 'used_to_smoke',
                 'preexisting_pregnant', 'preexisting_not_say',
                 'preexisting_liver_disease', 'preexisting_immunodeficiency',
                 'preexisting_hypertension', 'preexisting_diabetes',
                 'preexisting_chronic_kidney_disease',
                 'preexisting_chronic_lung_disease',
                 'preexisting_cardiovascular_disease', 'preexisting_cancer',
                 'preexisting_autoimmune_disease', 'preexisting_asthma',
                 'preexisting_allergies', 'unadj_100k+', 'unadj_70-100k',
                 'unadj_0-40k', 'race_ethnicity_unknown',
                 'race_ethnicity_other_expanded', 'race_ethnicity_multi',
                 'race_ethnicity_asian', 'race_ethnicity_african_american',
                 'race_ethnicity_hispanic_latino', 'female', '65-99', '55-64',
                 '45-54', '30-44')

cols_tested <- c("tested", feats_tested)

dat_testedreg <- dat_feats[, ..cols_tested]
dim(dat_testedreg)

tested_logit <- glm(tested ~ ., data = dat_testedreg,
                    family = binomial(link = "logit"))
tested_logit_df <- tidy_logistic_naive(tested_logit)

tested_firth <- logistf(tested ~ ., data = dat_testedreg, firth = TRUE,
                        pl = FALSE)
tested_firth_df <- tidy(tested_firth)

pdf("logit_firth_compare/tested.pdf", width = 8, height = 4)
plot_logit_firth(tested_logit_df, tested_firth_df)
dev.off()

fwrite(tested_logit_df, "logit_firth_compare/tested_logit.csv")
fwrite(tested_firth_df, "logit_firth_compare/tested_firth.csv")

#' # Predicting Who Tests Positive

feats_outcome <- c('30-44', '45-54', '55-64', '65-99', 'female',
                   'race_ethnicity_hispanic_latino',
                   'race_ethnicity_african_american', 'race_ethnicity_asian',
                   'race_ethnicity_multi', 'race_ethnicity_other_expanded',
                   'race_ethnicity_unknown', 'unadj_0-40k', 'unadj_70-100k',
                   'unadj_100k+', 'preexisting_allergies', 'preexisting_asthma',
                   'preexisting_autoimmune_disease', 'preexisting_cancer',
                   'preexisting_cardiovascular_disease',
                   'preexisting_chronic_lung_disease',
                   'preexisting_chronic_kidney_disease', 'preexisting_diabetes',
                   'preexisting_hypertension', 'preexisting_immunodeficiency',
                   'preexisting_liver_disease', 'preexisting_not_say',
                   'preexisting_pregnant', 'used_to_smoke', 'symptoms_cough',
                   'symptoms_chills_shaking',
                   'symptoms_loss_of_taste_and_or_smell', 'symptoms_diarrhea',
                   'symptoms_loss_of_appetite',
                   'symptoms_tight_feeling_in_chest',
                   'symptoms_muscle_and_joint_pain',
                   'symptoms_nasal_congestion', 'symptoms_shortness_of_breath',
                   'symptoms_nausea_and_vomiting', 'symptoms_runny_nose',
                   'symptoms_sore_throat', 'symptoms_fatigue', 'symptoms_fever',
                   'symptoms_tingling_sensation', 'household_members_count_2',
                   'household_members_count_3', 'household_members_count_4',
                   'household_members_count_5', 'prof_healthcare',
                   'prof_other_essential', 'popden_150-999', 'popden_1000+',
                   'covid_19_symptoms_in_household', 'exposed_yes_household',
                   'exposed_yes_not_household', 'exposed_most_likely',
                   'sleep_less_than_5', 'sleep_5_to_6_hours',
                   'sleep_9_to_10_hours', 'sleep_11_or_more_hours',
                   'test_rate_0-2', 'test_rate_2.5-3.7', 'test_rate_3.7+',
                   'East North Central', 'East South Central', 'Middle Atlantic',
                   'Mountain', 'New England', 'South Atlantic',
                   'West North Central', 'West South Central')
cols_outcome <- c("covid_19_swab_test_result_positive", feats_outcome)

dat_outcomereg <- dat_feats[covid_19_swab_test_result_positive == TRUE |
                            covid_19_swab_test_result_negative == TRUE,
                            ..cols_outcome]
dim(dat_outcomereg)

outcome_logit <- glm(covid_19_swab_test_result_positive ~ .,
                     data = dat_outcomereg,
                     family = binomial(link = "logit"))
outcome_logit_df <- tidy(outcome_logit, conf.int = TRUE, conf.level = 0.95)

outcome_firth <- logistf(covid_19_swab_test_result_positive ~ .,
                         data = dat_outcomereg, firth = TRUE, pl = TRUE)
outcome_firth_df <- tidy(outcome_firth)


pdf("logit_firth_compare/outcome.pdf", width = 8, height = 4)
plot_logit_firth(outcome_logit_df, outcome_firth_df)
dev.off()

fwrite(outcome_logit_df, "logit_firth_compare/outcome_logit.csv")
fwrite(outcome_firth_df, "logit_firth_compare/outcome_firth.csv")
