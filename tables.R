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

# race
r01_intake$race <- ifelse(r01_intake$race == 0, 'White/Caucasian', ifelse(r01_intake$race == 2, 'Asian', ifelse(r01_intake$race == 3, 'Black/AA', 'Other')))

# ethnicity
r01_intake$ethnicity <- ifelse(r01_intake$ethnicity == 0, 'Not Hispanic/Lantinx', 'Hispanic/Lantinx')

## demo table
intake_demo_data <- r01_intake[c(8, 10, 12, 14:16, 22, 746:747)]
demo_risk_mom_tab <-
  tbl_summary(
    data = intake_demo_data,
    by = risk_status_mom,
    value = list(age_yr ~ "Age, yr", sex ~ "Sex", ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income", bmi_percentile ~ "BMI %tile", mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education"),
    label = list(age_yr ~ "Age, yr", sex ~ "Sex", ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income", bmi_percentile ~ "BMI %tile", mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education"),
    statistic = all_continuous() ~ c("{mean} [{min} - {max}]"),
    missing = "ifany",
    digits = all_continuous() ~ 1)


## intake tables
intake_data <- intake_long[c(8, 17:28)]
intake_tab <-
  tbl_summary(
    data = intake_data,
    by = PortionSize,
    type = list(chnug_grams ~ "continuous", chnug_kcal ~ "continuous",  mac_grams ~ "continuous", mac_kcal ~ "continuous", grape_grams ~ "continuous", grape_kcal ~ "continuous", broc_grams ~ "continuous",  broc_kcal ~ "continuous",  mac_vas ~ "continuous",  chnug_vas ~ "continuous",  broc_vas ~ "continuous",  grape_vas ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

hr_intake_data <- intake_long[intake_long$risk_status_mom == 'High Risk', c(8, 17:28)]
hr_intake_tab <-
  tbl_summary(
    data = hr_intake_data,
    by = PortionSize,
    type = list(chnug_grams ~ "continuous", chnug_kcal ~ "continuous",  mac_grams ~ "continuous", mac_kcal ~ "continuous", grape_grams ~ "continuous", grape_kcal ~ "continuous", broc_grams ~ "continuous",  broc_kcal ~ "continuous",  mac_vas ~ "continuous",  chnug_vas ~ "continuous",  broc_vas ~ "continuous",  grape_vas ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

lr_intake_data <- intake_long[intake_long$risk_status_mom == 'Low Risk', c(8, 17:28)]
lr_intake_tab <-
  tbl_summary(
    data = lr_intake_data,
    by = PortionSize,
    type = list(chnug_grams ~ "continuous", chnug_kcal ~ "continuous",  mac_grams ~ "continuous", mac_kcal ~ "continuous", grape_grams ~ "continuous", grape_kcal ~ "continuous", broc_grams ~ "continuous",  broc_kcal ~ "continuous",  mac_vas ~ "continuous",  chnug_vas ~ "continuous",  broc_vas ~ "continuous",  grape_vas ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

intake_merge_tab <-
  tbl_merge(
    tbls = list(hr_intake_tab, lr_intake_tab),
    tab_spanner = c("**High Risk**", "**Low Risk**")
  )
