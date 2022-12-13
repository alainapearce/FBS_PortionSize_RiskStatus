# This script report was written by Alaina Pearce in January 2022 to 
# examine intake models by risk status for SSIB 2022 
# (data from Food and Brain Study).
#
#     Copyright (C) 2022 Alaina L Pearce
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.

############ Basic Data Load/Setup ############
# need to uncomment if running independently - not needed if compiling with 2022-01-27_PSU_CBBCsem.Rmd

# library(haven)
# library(feisr)
# library(lme4)
# library(lavaan)
# library(emmeans)
# library(gtsummary)
# theme_gtsummary_compact()
#
# source('functions.R')

# source('setup_data.R')

## demo table
intake_demo_data <- r01_intake[c(10, 12, 14:16, 22, 746:747)]
demo_tab <-
  tbl_summary(
    data = intake_demo_data,
    value = list(age_yr ~ "Age, yr", sex ~ "Sex", ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income", bmi_percentile ~ "BMI %tile", mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education"),
    label = list(age_yr ~ "Age, yr", sex ~ "Sex", ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income", bmi_percentile ~ "BMI %tile", mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education"),
    type = list(age_yr ~ "continuous", sex ~ "categorical", ethnicity ~ "categorical", race ~ "categorical", income ~ "categorical", bmi_percentile ~ "continuous", mom_ed ~ "categorical", dad_ed ~ "categorical"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1)

risk_intake_demo_data <- r01_intake[c(8, 10, 12, 14:16, 22, 746:747)]
demo_risk_mom_tab <-
  tbl_summary(
    data = risk_intake_demo_data,
    by = risk_status_mom,
    value = list(age_yr ~ "Age, yr", sex ~ "Sex", ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income", bmi_percentile ~ "BMI %tile", mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education"),
    label = list(age_yr ~ "Age, yr", sex ~ "Sex", ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income", bmi_percentile ~ "BMI %tile", mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education"),
    type = list(age_yr ~ "continuous", sex ~ "categorical", ethnicity ~ "categorical", race ~ "categorical", income ~ "categorical", bmi_percentile ~ "continuous", mom_ed ~ "categorical", dad_ed ~ "categorical"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1)


demo_merge_tab <-
  tbl_merge(
    tbls = list(demo_risk_mom_tab, demo_tab),
    tab_spanner = c("**Risk Groups**", "**Overall**")
  )

## intake tables - total
total_intake_data <- r01_intake[c(606:607, 748, 652:653, 749, 698:699, 750, 744:745, 751)]
total_intake_tab <-
  tbl_summary(
    data = total_intake_data,
    type = list(ps1_total_g ~ "continuous", ps1_total_kcal ~ "continuous",  ps1_avg_vas ~ "continuous", ps2_total_g ~ "continuous", ps2_total_kcal ~ "continuous", ps2_avg_vas ~ "continuous", ps3_total_g ~ "continuous", ps3_total_kcal ~ "continuous", ps3_avg_vas ~ "continuous", ps4_total_g ~ "continuous",  ps4_total_kcal ~ "continuous", ps4_avg_vas ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

total_intakerisk_data <- r01_intake[c(8, 606:607, 748, 652:653, 749, 698:699, 750, 744:745, 751)]
total_intakerisk_tab <-
  tbl_summary(
    data = total_intakerisk_data,
    by = risk_status_mom,
    type = list(ps1_total_g ~ "continuous", ps1_total_kcal ~ "continuous",  ps1_avg_vas ~ "continuous", ps2_total_g ~ "continuous", ps2_total_kcal ~ "continuous", ps2_avg_vas ~ "continuous", ps3_total_g ~ "continuous", ps3_total_kcal ~ "continuous", ps3_avg_vas ~ "continuous", ps4_total_g ~ "continuous",  ps4_total_kcal ~ "continuous", ps4_avg_vas ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

intake_merge_tab <-
  tbl_merge(
    tbls = list(total_intakerisk_tab, total_intake_tab),
    tab_spanner = c("**Risk Groups**", "**Overall**")
  )

## intake tables - by food
intake_data <- intake_long[c(9, 18:29)]
intake_tab <-
  tbl_summary(
    data = intake_data,
    by = PortionSize,
    type = list(chnug_grams ~ "continuous", chnug_kcal ~ "continuous",  mac_grams ~ "continuous", mac_kcal ~ "continuous", grape_grams ~ "continuous", grape_kcal ~ "continuous", broc_grams ~ "continuous",  broc_kcal ~ "continuous",  mac_vas ~ "continuous",  chnug_vas ~ "continuous",  broc_vas ~ "continuous",  grape_vas ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

hr_intake_data <- intake_long[intake_long$risk_status_mom == 'High Risk', c(9, 18:29)]
hr_intake_tab <-
  tbl_summary(
    data = hr_intake_data,
    by = PortionSize,
    type = list(chnug_grams ~ "continuous", chnug_kcal ~ "continuous",  mac_grams ~ "continuous", mac_kcal ~ "continuous", grape_grams ~ "continuous", grape_kcal ~ "continuous", broc_grams ~ "continuous",  broc_kcal ~ "continuous",  mac_vas ~ "continuous",  chnug_vas ~ "continuous",  broc_vas ~ "continuous",  grape_vas ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

lr_intake_data <- intake_long[intake_long$risk_status_mom == 'Low Risk', c(9, 18:29)]
lr_intake_tab <-
  tbl_summary(
    data = lr_intake_data,
    by = PortionSize,
    type = list(chnug_grams ~ "continuous", chnug_kcal ~ "continuous",  mac_grams ~ "continuous", mac_kcal ~ "continuous", grape_grams ~ "continuous", grape_kcal ~ "continuous", broc_grams ~ "continuous",  broc_kcal ~ "continuous",  mac_vas ~ "continuous",  chnug_vas ~ "continuous",  broc_vas ~ "continuous",  grape_vas ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

#liking

liking_data <- intake_long[c(9, 12, 26:29)]
liking_tab <-
  tbl_summary(
    data = liking_data,
    by = PortionSize,
    type = list(avg_vas ~ "continuous", chnug_vas ~ "continuous",  mac_vas ~ "continuous", grape_vas ~ "continuous", broc_vas ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)
