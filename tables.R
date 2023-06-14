# This script report was written by Alaina Pearce in 2022 paper examining 
# the portion size effect by risk status
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
intake_demo_data <- r01_intake[c(10, 12, 14:16, 22, 840:842, 801, 746:747)]
demo_tab <-
  tbl_summary(
    data = intake_demo_data,
    value = list(age_yr ~ "Age, yr", sex ~ "Sex", ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income", bmi_percentile ~ "BMI %tile", dxa_total_fat_mass ~ 'Fat Mass, g', dxa_total_body_perc_fat ~ 'Body Fat, %', fmi ~ 'Fat Mass Index (fat, kg/height, m2)', cebq_sr ~ 'Satiety Responsiveness', mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education"),
    label = list(age_yr ~ "Age, yr", sex ~ "Sex", ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income", bmi_percentile ~ "BMI %tile", dxa_total_fat_mass ~ 'Fat Mass, g', dxa_total_body_perc_fat ~ 'Body Fat, %', fmi ~ 'Fat Mass Index (fat, kg/height, m2)', cebq_sr ~ 'Satiety Responsiveness', mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education"),
    type = list(age_yr ~ "continuous", sex ~ "categorical", ethnicity ~ "categorical", race ~ "categorical", income ~ "categorical", bmi_percentile ~ "continuous", dxa_total_fat_mass ~ 'continuous', dxa_total_body_perc_fat ~ 'continuous', fmi ~ 'continuous', cebq_sr ~ 'continuous', mom_ed ~ "categorical", dad_ed ~ "categorical"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "ifany",
    digits = all_continuous() ~ 1)

risk_intake_demo_data <- r01_intake[c(8, 10, 12, 14:16, 22, 840:842, 801, 746:747)]

demo_risk_mom_tab <-
  tbl_summary(
    data = risk_intake_demo_data,
    by = risk_status_mom,
    value = list(age_yr ~ "Age, yr", sex ~ "Sex", ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income", bmi_percentile ~ "BMI %tile", dxa_total_fat_mass ~ 'Fat Mass, g', dxa_total_body_perc_fat ~ 'Body Fat, %', fmi ~ 'Fat Mass Index (fat, kg/height, m2)', cebq_sr ~ 'Satiety Responsiveness', mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education"),
    label = list(age_yr ~ "Age, yr", sex ~ "Sex", ethnicity ~ "Ethnicity", race ~ "Race", income ~ "Income", bmi_percentile ~ "BMI %tile", dxa_total_fat_mass ~ 'Fat Mass, g', dxa_total_body_perc_fat ~ 'Body Fat, %', fmi ~ 'Fat Mass Index (fat, kg/height, m2)', cebq_sr ~ 'Satiety Responsiveness', mom_ed ~ "Mother's Education", dad_ed ~ "Father's Education"),
    type = list(age_yr ~ "continuous", sex ~ "categorical", ethnicity ~ "categorical", race ~ "categorical", income ~ "categorical", bmi_percentile ~ "continuous",  dxa_total_fat_mass ~ 'continuous', dxa_total_body_perc_fat ~ 'continuous', fmi ~ 'continuous', cebq_sr ~ 'continuous', mom_ed ~ "categorical", dad_ed ~ "categorical"),
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

high_risk_liking_data <- intake_long[intake_long$risk_status_mom == 'High Risk', c(9, 12, 26:29)]

hr_liking_tab <-
  tbl_summary(
    data = high_risk_liking_data,
    by = PortionSize,
    type = list(avg_vas ~ "continuous", chnug_vas ~ "continuous",  mac_vas ~ "continuous", grape_vas ~ "continuous", broc_vas ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

low_risk_liking_data <- intake_long[intake_long$risk_status_mom == 'Low Risk', c(9, 12, 26:29)]

lr_liking_tab <-
  tbl_summary(
    data = low_risk_liking_data,
    by = PortionSize,
    type = list(avg_vas ~ "continuous", chnug_vas ~ "continuous",  mac_vas ~ "continuous", grape_vas ~ "continuous", broc_vas ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

liking_merge_tab <-
  tbl_stack(
    tbls = list(liking_tab, hr_liking_tab, lr_liking_tab),
    group_header = c("**Overall**", "**High-Risk**", "**Low-Risk**")
  )

#PSS survey
pss_data <- r01_intake[c(761:772)]
pss_tab <-
  tbl_summary(
    data = pss_data,
    value = list(pss_broccoli_eat ~ "Broccoli, eat", pss_broccoli_much ~ "Broccoli, amount",  pss_broccoli_like ~ "Broccoli, like", pss_chkn_nug_eat ~ "Chicken Nugget, eat", pss_chkn_nug_much ~ "Chicken Nugget, amount", pss_chkn_nug_like ~ "Chicken Nugget, like", pss_grapes_eat ~ "Grapes, eat", pss_grapes_much ~ "Grapes, amount", pss_grapes_like ~ "Grapes, like", pss_mac_cheese_eat ~ "Mac + Cheese, eat", pss_mac_cheese_much ~ "Mac + Cheese, amount", pss_mac_cheese_like ~ "Mac + Cheese, like"),
    label = list(pss_broccoli_eat ~ "Broccoli, eat", pss_broccoli_much ~ "Broccoli, amount",  pss_broccoli_like ~ "Broccoli, like", pss_chkn_nug_eat ~ "Chicken Nugget, eat", pss_chkn_nug_much ~ "Chicken Nugget, amount", pss_chkn_nug_like ~ "Chicken Nugget, like", pss_grapes_eat ~ "Grapes, eat", pss_grapes_much ~ "Grapes, amount", pss_grapes_like ~ "Grapes, like", pss_mac_cheese_eat ~ "Mac + Cheese, eat", pss_mac_cheese_much ~ "Mac + Cheese, amount", pss_mac_cheese_like ~ "Mac + Cheese, like")     ,   
    type = list(pss_broccoli_eat ~ "categorical", pss_broccoli_much ~ "continuous",  pss_broccoli_like ~ "continuous", pss_chkn_nug_eat ~ "categorical", pss_chkn_nug_much ~ "continuous", pss_chkn_nug_like ~ "continuous", pss_grapes_eat ~ "categorical", pss_grapes_much ~ "continuous", pss_grapes_like ~ "continuous", pss_mac_cheese_eat ~ "categorical", pss_mac_cheese_much ~ "continuous", pss_mac_cheese_like ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

risk_pss_data <- r01_intake[c(8, 761:772)]

risk_pss_tab <-
  tbl_summary(
    data = risk_pss_data,
    by = risk_status_mom,
    value = list(pss_broccoli_eat ~ "Broccoli, eat", pss_broccoli_much ~ "Broccoli, amount",  pss_broccoli_like ~ "Broccoli, like", pss_chkn_nug_eat ~ "Chicken Nugget, eat", pss_chkn_nug_much ~ "Chicken Nugget, amount", pss_chkn_nug_like ~ "Chicken Nugget, like", pss_grapes_eat ~ "Grapes, eat", pss_grapes_much ~ "Grapes, amount", pss_grapes_like ~ "Grapes, like", pss_mac_cheese_eat ~ "Mac + Cheese, eat", pss_mac_cheese_much ~ "Mac + Cheese, amount", pss_mac_cheese_like ~ "Mac + Cheese, like"),
    label = list(pss_broccoli_eat ~ "Broccoli, eat", pss_broccoli_much ~ "Broccoli, amount",  pss_broccoli_like ~ "Broccoli, like", pss_chkn_nug_eat ~ "Chicken Nugget, eat", pss_chkn_nug_much ~ "Chicken Nugget, amount", pss_chkn_nug_like ~ "Chicken Nugget, like", pss_grapes_eat ~ "Grapes, eat", pss_grapes_much ~ "Grapes, amount", pss_grapes_like ~ "Grapes, like", pss_mac_cheese_eat ~ "Mac + Cheese, eat", pss_mac_cheese_much ~ "Mac + Cheese, amount", pss_mac_cheese_like ~ "Mac + Cheese, like")     ,   
    type = list(pss_broccoli_eat ~ "categorical", pss_broccoli_much ~ "continuous",  pss_broccoli_like ~ "continuous", pss_chkn_nug_eat ~ "categorical", pss_chkn_nug_much ~ "continuous", pss_chkn_nug_like ~ "continuous", pss_grapes_eat ~ "categorical", pss_grapes_much ~ "continuous", pss_grapes_like ~ "continuous", pss_mac_cheese_eat ~ "categorical", pss_mac_cheese_much ~ "continuous", pss_mac_cheese_like ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

pss_merge_tab <-
  tbl_merge(
    tbls = list(risk_pss_tab, pss_tab),
    tab_spanner = c("**Risk Groups**", "**Overall**")
  )

#P-PSS survey
p_pss_data <- r01_intake[c(773:796)]
p_pss_tab <-
  tbl_summary(
    data = p_pss_data,
    value = list(p_pss_broccoli_eat ~ "Broccoli, eat", p_pss_broccoli_freq ~ "Broccoli, freq", p_pss_broccoli_much ~ "Broccoli, amount",  p_pss_broccoli_like ~ "Broccoli, like", p_pss_broccoli_portion ~ "Broccoli, portion", p_pss_broccoli_nutrition ~ "Broccoli, health", p_pss_chkn_nug_eat ~ "Chicken Nugget, eat", p_pss_chkn_nug_freq ~ "Chicken Nugget, freq", p_pss_chkn_nug_much ~ "Chicken Nugget, amount", p_pss_chkn_nug_like ~ "Chicken Nugget, like", p_pss_chkn_nug_portion ~ "Chicken Nugget, portion", p_pss_chkn_nug_nutrition ~ "Chicken Nugget, health", p_pss_grapes_eat ~ "Grapes, eat", p_pss_grapes_freq ~ "Grapes, freq", p_pss_grapes_much ~ "Grapes, amount", p_pss_grapes_like ~ "Grapes, like", p_pss_grapes_portion ~ "Grapes, poriton", p_pss_grapes_nutrition ~ "Grapes, health", p_pss_mac_cheese_eat ~ "Mac + Cheese, eat", p_pss_mac_cheese_freq ~ "Mac + Cheese, freq", p_pss_mac_cheese_much ~ "Mac + Cheese, amount", p_pss_mac_cheese_like ~ "Mac + Cheese, like", p_pss_mac_cheese_portion ~ "Mac + Cheese, poriton", p_pss_mac_cheese_nutrition ~ "Mac + Cheese, nutrition"),
    label = list(p_pss_broccoli_eat ~ "Broccoli, eat", p_pss_broccoli_freq ~ "Broccoli, freq", p_pss_broccoli_much ~ "Broccoli, amount",  p_pss_broccoli_like ~ "Broccoli, like", p_pss_broccoli_portion ~ "Broccoli, portion", p_pss_broccoli_nutrition ~ "Broccoli, health", p_pss_chkn_nug_eat ~ "Chicken Nugget, eat", p_pss_chkn_nug_freq ~ "Chicken Nugget, freq", p_pss_chkn_nug_much ~ "Chicken Nugget, amount", p_pss_chkn_nug_like ~ "Chicken Nugget, like", p_pss_chkn_nug_portion ~ "Chicken Nugget, portion", p_pss_chkn_nug_nutrition ~ "Chicken Nugget, health", p_pss_grapes_eat ~ "Grapes, eat", p_pss_grapes_freq ~ "Grapes, freq", p_pss_grapes_much ~ "Grapes, amount", p_pss_grapes_like ~ "Grapes, like", p_pss_grapes_portion ~ "Grapes, poriton", p_pss_grapes_nutrition ~ "Grapes, health", p_pss_mac_cheese_eat ~ "Mac + Cheese, eat", p_pss_mac_cheese_freq ~ "Mac + Cheese, freq", p_pss_mac_cheese_much ~ "Mac + Cheese, amount", p_pss_mac_cheese_like ~ "Mac + Cheese, like", p_pss_mac_cheese_portion ~ "Mac + Cheese, poriton", p_pss_mac_cheese_nutrition ~ "Mac + Cheese, nutrition"),   
    type = list(p_pss_broccoli_eat ~ "categorical", p_pss_broccoli_freq ~ "categorical", p_pss_broccoli_much ~ "continuous",  p_pss_broccoli_like ~ "continuous", p_pss_broccoli_portion ~ "continuous",  p_pss_broccoli_nutrition ~ "continuous", p_pss_chkn_nug_eat ~ "categorical", p_pss_chkn_nug_freq ~ "categorical", p_pss_chkn_nug_much ~ "continuous", p_pss_chkn_nug_like ~ "continuous", p_pss_chkn_nug_portion ~ "continuous", p_pss_chkn_nug_nutrition ~ "continuous", p_pss_grapes_eat ~ "categorical", p_pss_grapes_freq ~ "categorical", p_pss_grapes_much ~ "continuous", p_pss_grapes_like ~ "continuous", p_pss_grapes_portion ~ "continuous", p_pss_grapes_nutrition ~ "continuous", p_pss_mac_cheese_eat ~ "categorical", p_pss_mac_cheese_freq ~ "categorical", p_pss_mac_cheese_much ~ "continuous", p_pss_mac_cheese_like ~ "continuous", p_pss_mac_cheese_portion ~ "continuous", p_pss_mac_cheese_nutrition ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

risk_p_pss_data <- r01_intake[c(8, 773:796)]

risk_p_pss_tab <-
  tbl_summary(
    data = risk_p_pss_data,
    by = risk_status_mom,
    value = list(p_pss_broccoli_eat ~ "Broccoli, eat", p_pss_broccoli_freq ~ "Broccoli, freq", p_pss_broccoli_much ~ "Broccoli, amount",  p_pss_broccoli_like ~ "Broccoli, like", p_pss_broccoli_portion ~ "Broccoli, portion", p_pss_broccoli_nutrition ~ "Broccoli, health", p_pss_chkn_nug_eat ~ "Chicken Nugget, eat", p_pss_chkn_nug_freq ~ "Chicken Nugget, freq", p_pss_chkn_nug_much ~ "Chicken Nugget, amount", p_pss_chkn_nug_like ~ "Chicken Nugget, like", p_pss_chkn_nug_portion ~ "Chicken Nugget, portion", p_pss_chkn_nug_nutrition ~ "Chicken Nugget, health", p_pss_grapes_eat ~ "Grapes, eat", p_pss_grapes_freq ~ "Grapes, freq", p_pss_grapes_much ~ "Grapes, amount", p_pss_grapes_like ~ "Grapes, like", p_pss_grapes_portion ~ "Grapes, poriton", p_pss_grapes_nutrition ~ "Grapes, health", p_pss_mac_cheese_eat ~ "Mac + Cheese, eat", p_pss_mac_cheese_freq ~ "Mac + Cheese, freq", p_pss_mac_cheese_much ~ "Mac + Cheese, amount", p_pss_mac_cheese_like ~ "Mac + Cheese, like", p_pss_mac_cheese_portion ~ "Mac + Cheese, poriton", p_pss_mac_cheese_nutrition ~ "Mac + Cheese, nutrition"),
    label = list(p_pss_broccoli_eat ~ "Broccoli, eat", p_pss_broccoli_freq ~ "Broccoli, freq", p_pss_broccoli_much ~ "Broccoli, amount",  p_pss_broccoli_like ~ "Broccoli, like", p_pss_broccoli_portion ~ "Broccoli, portion", p_pss_broccoli_nutrition ~ "Broccoli, health", p_pss_chkn_nug_eat ~ "Chicken Nugget, eat", p_pss_chkn_nug_freq ~ "Chicken Nugget, freq", p_pss_chkn_nug_much ~ "Chicken Nugget, amount", p_pss_chkn_nug_like ~ "Chicken Nugget, like", p_pss_chkn_nug_portion ~ "Chicken Nugget, portion", p_pss_chkn_nug_nutrition ~ "Chicken Nugget, health", p_pss_grapes_eat ~ "Grapes, eat", p_pss_grapes_freq ~ "Grapes, freq", p_pss_grapes_much ~ "Grapes, amount", p_pss_grapes_like ~ "Grapes, like", p_pss_grapes_portion ~ "Grapes, poriton", p_pss_grapes_nutrition ~ "Grapes, health", p_pss_mac_cheese_eat ~ "Mac + Cheese, eat", p_pss_mac_cheese_freq ~ "Mac + Cheese, freq", p_pss_mac_cheese_much ~ "Mac + Cheese, amount", p_pss_mac_cheese_like ~ "Mac + Cheese, like", p_pss_mac_cheese_portion ~ "Mac + Cheese, poriton", p_pss_mac_cheese_nutrition ~ "Mac + Cheese, nutrition"),   
    type = list(p_pss_broccoli_eat ~ "categorical", p_pss_broccoli_freq ~ "categorical", p_pss_broccoli_much ~ "continuous",  p_pss_broccoli_like ~ "continuous", p_pss_broccoli_portion ~ "continuous",  p_pss_broccoli_nutrition ~ "continuous", p_pss_chkn_nug_eat ~ "categorical", p_pss_chkn_nug_freq ~ "categorical", p_pss_chkn_nug_much ~ "continuous", p_pss_chkn_nug_like ~ "continuous", p_pss_chkn_nug_portion ~ "continuous", p_pss_chkn_nug_nutrition ~ "continuous", p_pss_grapes_eat ~ "categorical", p_pss_grapes_freq ~ "categorical", p_pss_grapes_much ~ "continuous", p_pss_grapes_like ~ "continuous", p_pss_grapes_portion ~ "continuous", p_pss_grapes_nutrition ~ "continuous", p_pss_mac_cheese_eat ~ "categorical", p_pss_mac_cheese_freq ~ "categorical", p_pss_mac_cheese_much ~ "continuous", p_pss_mac_cheese_like ~ "continuous", p_pss_mac_cheese_portion ~ "continuous", p_pss_mac_cheese_nutrition ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

p_pss_merge_tab <-
  tbl_merge(
    tbls = list(risk_p_pss_tab, p_pss_tab),
    tab_spanner = c("**Risk Groups**", "**Overall**")
  )

pss_data <- r01_intake[c(761:772)]
pss_tab <-
  tbl_summary(
    data = pss_data,
    value = list(pss_broccoli_eat ~ "Broccoli, eat", pss_broccoli_much ~ "Broccoli, amount",  pss_broccoli_like ~ "Broccoli, like", pss_chkn_nug_eat ~ "Chicken Nugget, eat", pss_chkn_nug_much ~ "Chicken Nugget, amount", pss_chkn_nug_like ~ "Chicken Nugget, like", pss_grapes_eat ~ "Grapes, eat", pss_grapes_much ~ "Grapes, amount", pss_grapes_like ~ "Grapes, like", pss_mac_cheese_eat ~ "Mac + Cheese, eat", pss_mac_cheese_much ~ "Mac + Cheese, amount", pss_mac_cheese_like ~ "Mac + Cheese, like"),
    label = list(pss_broccoli_eat ~ "Broccoli, eat", pss_broccoli_much ~ "Broccoli, amount",  pss_broccoli_like ~ "Broccoli, like", pss_chkn_nug_eat ~ "Chicken Nugget, eat", pss_chkn_nug_much ~ "Chicken Nugget, amount", pss_chkn_nug_like ~ "Chicken Nugget, like", pss_grapes_eat ~ "Grapes, eat", pss_grapes_much ~ "Grapes, amount", pss_grapes_like ~ "Grapes, like", pss_mac_cheese_eat ~ "Mac + Cheese, eat", pss_mac_cheese_much ~ "Mac + Cheese, amount", pss_mac_cheese_like ~ "Mac + Cheese, like")     ,   
    type = list(pss_broccoli_eat ~ "categorical", pss_broccoli_much ~ "continuous",  pss_broccoli_like ~ "continuous", pss_chkn_nug_eat ~ "categorical", pss_chkn_nug_much ~ "continuous", pss_chkn_nug_like ~ "continuous", pss_grapes_eat ~ "categorical", pss_grapes_much ~ "continuous", pss_grapes_like ~ "continuous", pss_mac_cheese_eat ~ "categorical", pss_mac_cheese_much ~ "continuous", pss_mac_cheese_like ~ "continuous"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

## ranked data 
rank_data <- intake_long[c(9, 30:33)]
rank_tab <-
  tbl_summary(
    data = rank_data,
    by = PortionSize,
    type = list(chnug_rank ~ "categorical",  mac_rank ~ "categorical", grape_rank ~ "categorical", broc_rank ~ "categorical"),
    missing = "no",
    digits = all_continuous() ~ 1)

high_risk_rank_data <- intake_long[intake_long$risk_status_mom == 'High Risk', c(9, 30:33)]

hr_rank_tab <-
  tbl_summary(
    data = high_risk_rank_data,
    by = PortionSize,
    type = list(chnug_rank ~ "categorical",  mac_rank ~ "categorical", grape_rank ~ "categorical", broc_rank ~ "categorical"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

low_risk_rank_data <- intake_long[intake_long$risk_status_mom == 'Low Risk', c(9, 30:33)]

lr_rank_tab <-
  tbl_summary(
    data = low_risk_rank_data,
    by = PortionSize,
    type = list(chnug_rank ~ "categorical",  mac_rank ~ "categorical", grape_rank ~ "categorical", broc_rank ~ "categorical"),
    statistic = all_continuous() ~ c("{mean} ({sd})"),
    missing = "no",
    digits = all_continuous() ~ 1)

rank_merge_tab <-
  tbl_stack(
    tbls = list(rank_tab, hr_rank_tab, lr_rank_tab),
    group_header = c("**Overall**", "**High-Risk**", "**Low-Risk**")
  )

