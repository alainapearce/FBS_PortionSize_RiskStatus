# This script report was written by Alaina Pearce in January 2022 to 
# examine  intake models by risk status for SSIB 2022 
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
# library(lme4)
# library(lavaan)
# library(emmeans)
#
# source('functions.R')

# source('setup_data.R')

#### set up ####

# check plate cleaning
r01_intake$ps1_main4_total_g <- rowSums(r01_intake[c('ps1_consumed_mac_cheese_g', 'ps1_consumed_grapes_g', 'ps1_consumed_chkn_nug_g', 'ps1_consumed_broccoli_g')])
r01_intake$ps1_plate_clean <- ifelse(r01_intake$ps1_total_g >= 798, 1, 0)

r01_intake$ps2_main4_total_g <- rowSums(r01_intake[c('ps2_consumed_mac_cheese_g', 'ps2_consumed_grapes_g', 'ps2_consumed_chkn_nug_g', 'ps2_consumed_broccoli_g')])
r01_intake$ps2_plate_clean <- ifelse(r01_intake$ps2_main4_total_g >= 1037.4, 1, 0)

r01_intake$ps3_main4_total_g <- rowSums(r01_intake[c('ps3_consumed_mac_cheese_g', 'ps3_consumed_grapes_g', 'ps3_consumed_chkn_nug_g', 'ps3_consumed_broccoli_g')])
r01_intake$ps3_plate_clean <- ifelse(r01_intake$ps3_main4_total_g >= 1276.8, 1, 0)

r01_intake$ps4_main4_total_g <- rowSums(r01_intake[c('ps4_consumed_mac_cheese_g', 'ps4_consumed_grapes_g', 'ps4_consumed_chkn_nug_g', 'ps4_consumed_broccoli_g')])
r01_intake$ps4_plate_clean <- ifelse(r01_intake$ps4_main4_total_g >= 1516.2, 1, 0)

# sr alpha check
r01_intake$cebq3_rev <- ifelse(r01_intake$cebq3 == 1, 5, ifelse(r01_intake$cebq3 == 2, 4, ifelse(r01_intake$cebq3 == 4, 2, ifelse(r01_intake$cebq3 == 5, 1, as.numeric(r01_intake$cebq3)))))

sr_alpha <- alpha(r01_intake[c('cebq3_rev', 'cebq17', 'cebq21', 'cebq26', 'cebq30')])

# demographics check

age_ttest <- t.test(age_yr ~ risk_status_mom, data = r01_intake)
bmi_ttest <- t.test(bmi_percentile ~ risk_status_mom, data = r01_intake)
bmi_sd <- sd.function.na(r01_intake, r01_intake$bmi_percentile, r01_intake$risk_status_mom)
fmi_ttest <- t.test(fmi ~ risk_status_mom, data = r01_intake)
fmi_sd <- sd.function.na(r01_intake, r01_intake$fmi, r01_intake$risk_status_mom)
sex_chi <- chisq.test(x = r01_intake$sex, y = r01_intake$risk_status_mom)
income_chi <- chisq.test(x = r01_intake$income, y = r01_intake$risk_status_mom)
momed_fisher <- fisher.test(x = r01_intake$mom_ed, y = r01_intake$risk_status_mom)
cebq_sr_ttest <- t.test(cebq_sr ~ risk_status_mom, data = r01_intake)
cebq_sr_sd <- sd.function.na(r01_intake, r01_intake$cebq_sr, r01_intake$risk_status_mom)

# liking ####

liking_mod <- lmer(avg_vas ~ preFF + fmi + sex + cebq_sr + meal_order + risk_status_mom + ps_prop + (1|sub), data = intake_long)
liking_sum <- summary(liking_mod)

liking_means <- means.function.na(intake_long, intake_long$avg_vas, intake_long$PortionSize)

# chicken nuggets
liking_chnug_mod <- lmer(chnug_vas ~ preFF + fmi + sex + cebq_sr + meal_order + risk_status_mom + ps_prop + (1|sub), data = intake_long)
liking_chnug_sum <- summary(liking_chnug_mod)

liking_chnug_means <- means.function.na(intake_long, intake_long$chnug_vas, intake_long$PortionSize)

# mac and cheese
liking_mac_mod <- lmer(mac_vas ~ preFF + fmi + sex + cebq_sr + meal_order + risk_status_mom + ps_prop + (1|sub), data = intake_long)
liking_mac_sum <- summary(liking_mac_mod)

liking_mac_means <- means.function.na(intake_long, intake_long$mac_vas, intake_long$PortionSize)

# grapes
liking_grape_mod <- lmer(grape_vas ~ preFF + fmi + sex + cebq_sr + meal_order + risk_status_mom + ps_prop + (1|sub), data = intake_long)
liking_grape_sum <- summary(liking_grape_mod)

liking_grape_means <- means.function.na(intake_long, intake_long$grape_vas, intake_long$PortionSize)

# broc
liking_broc_mod <- lmer(broc_vas ~ preFF + fmi + sex + cebq_sr + meal_order + risk_status_mom + ps_prop + (1|sub), data = intake_long)
liking_broc_sum <- summary(liking_broc_mod)

liking_grape_means <- means.function.na(intake_long, intake_long$broc_vas, intake_long$PortionSize)

# ranking ####
ps1_chnug_mac_rank_ttest <- t.test(r01_intake$ps1_rank_chkn_nug, r01_intake$ps1_rank_mac_cheese, paired = TRUE)
ps1_chnug_grape_rank_ttest <- t.test(r01_intake$ps1_rank_chkn_nug, r01_intake$ps1_rank_grape, paired = TRUE)
ps1_chnug_broc_rank_ttest <- t.test(r01_intake$ps1_rank_chkn_nug, r01_intake$ps1_rank_broccoli, paired = TRUE)
ps1_mac_grape_rank_ttest <- t.test(r01_intake$ps1_rank_mac_cheese, r01_intake$ps1_rank_grape, paired = TRUE)
ps1_mac_broc_rank_ttest <- t.test(r01_intake$ps1_rank_mac_cheese, r01_intake$ps1_rank_broccoli, paired = TRUE)
ps1_grape_broc_rank_ttest <- t.test(r01_intake$ps1_rank_grape, r01_intake$ps1_rank_broccoli, paired = TRUE)

ps2_chnug_mac_rank_ttest <- t.test(r01_intake$ps2_rank_chkn_nug, r01_intake$ps2_rank_mac_cheese, paired = TRUE)
ps2_chnug_grape_rank_ttest <- t.test(r01_intake$ps2_rank_chkn_nug, r01_intake$ps2_rank_grape, paired = TRUE)
ps2_chnug_broc_rank_ttest <- t.test(r01_intake$ps2_rank_chkn_nug, r01_intake$ps2_rank_broccoli, paired = TRUE)
ps2_mac_grape_rank_ttest <- t.test(r01_intake$ps2_rank_mac_cheese, r01_intake$ps2_rank_grape, paired = TRUE)
ps2_mac_broc_rank_ttest <- t.test(r01_intake$ps2_rank_mac_cheese, r01_intake$ps2_rank_broccoli, paired = TRUE)
ps2_grape_broc_rank_ttest <- t.test(r01_intake$ps2_rank_grape, r01_intake$ps2_rank_broccoli, paired = TRUE)

ps3_chnug_mac_rank_ttest <- t.test(r01_intake$ps3_rank_chkn_nug, r01_intake$ps3_rank_mac_cheese, paired = TRUE)
ps3_chnug_grape_rank_ttest <- t.test(r01_intake$ps3_rank_chkn_nug, r01_intake$ps3_rank_grape, paired = TRUE)
ps3_chnug_broc_rank_ttest <- t.test(r01_intake$ps3_rank_chkn_nug, r01_intake$ps3_rank_broccoli, paired = TRUE)
ps3_mac_grape_rank_ttest <- t.test(r01_intake$ps3_rank_mac_cheese, r01_intake$ps3_rank_grape, paired = TRUE)
ps3_mac_broc_rank_ttest <- t.test(r01_intake$ps3_rank_mac_cheese, r01_intake$ps3_rank_broccoli, paired = TRUE)
ps3_grape_broc_rank_ttest <- t.test(r01_intake$ps3_rank_grape, r01_intake$ps3_rank_broccoli, paired = TRUE)

ps4_chnug_mac_rank_ttest <- t.test(r01_intake$ps4_rank_chkn_nug, r01_intake$ps4_rank_mac_cheese, paired = TRUE)
ps4_chnug_grape_rank_ttest <- t.test(r01_intake$ps4_rank_chkn_nug, r01_intake$ps4_rank_grape, paired = TRUE)
ps4_chnug_broc_rank_ttest <- t.test(r01_intake$ps4_rank_chkn_nug, r01_intake$ps4_rank_broccoli, paired = TRUE)
ps4_mac_grape_rank_ttest <- t.test(r01_intake$ps4_rank_mac_cheese, r01_intake$ps4_rank_grape, paired = TRUE)
ps4_mac_broc_rank_ttest <- t.test(r01_intake$ps4_rank_mac_cheese, r01_intake$ps4_rank_broccoli, paired = TRUE)
ps4_grape_broc_rank_ttest <- t.test(r01_intake$ps4_rank_grape, r01_intake$ps4_rank_broccoli, paired = TRUE)

# chicken nuggets
rank_chnug_mod <- lmer(chnug_rank ~ preFF + fmi + sex + cebq_sr + meal_order + risk_status_mom + ps_prop + (1|sub), data = intake_long)
rank_chnug_sum <- summary(rank_chnug_mod)

rank_chnug_means <- means.function.na(intake_long, intake_long$chnug_rank, intake_long$PortionSize)

# mac and cheese
rank_mac_mod <- lmer(mac_rank ~ preFF + fmi + sex + cebq_sr + meal_order + risk_status_mom + ps_prop + (1|sub), data = intake_long)
rank_mac_sum <- summary(rank_mac_mod)

rank_mac_means <- means.function.na(intake_long, intake_long$mac_rank, intake_long$PortionSize)

# grapes
rank_grape_mod <- lmer(grape_rank ~ preFF + fmi + sex + cebq_sr + meal_order + risk_status_mom + ps_prop + (1|sub), data = intake_long)
rank_grape_sum <- summary(rank_grape_mod)

rank_grape_means <- means.function.na(intake_long, intake_long$grape_rank, intake_long$PortionSize)

# broc
rank_broc_mod <- lmer(broc_rank ~ preFF + fmi + sex + cebq_sr + meal_order + risk_status_mom + ps_prop + (1|sub), data = intake_long)
rank_broc_sum <- summary(rank_broc_mod)

rank_broc_means <- means.function.na(intake_long, intake_long$broc_rank, intake_long$PortionSize)


## Portion Selection/Typical Intake ####

# child
broc_eat_chi <- chisq.test(xtabs(~pss_broccoli_eat + risk_status_mom, data = r01_intake))
grapes_eat_fisher <- fisher.test(xtabs(~pss_grapes_eat + risk_status_mom, data = r01_intake))
chknug_eat_fisher <- fisher.test(xtabs(~pss_chkn_nug_eat + risk_status_mom, data = r01_intake))
mac_eat_fisher <- fisher.test(xtabs(~pss_mac_cheese_eat + risk_status_mom, data = r01_intake))

# parent
p_broc_eat_chi <- chisq.test(xtabs(~p_pss_broccoli_eat + risk_status_mom, data = r01_intake))
p_grapes_eat_fisher <- fisher.test(xtabs(~p_pss_grapes_eat + risk_status_mom, data = r01_intake))
p_chknug_eat_fisher <- fisher.test(xtabs(~p_pss_chkn_nug_eat + risk_status_mom, data = r01_intake))
p_mac_eat_fisher <- fisher.test(xtabs(~p_pss_mac_cheese_eat + risk_status_mom, data = r01_intake))

p_broc_freq_fisher <- fisher.test(xtabs(~p_pss_broccoli_freq + risk_status_mom, data = r01_intake))
p_grapes_freq_fisher <- fisher.test(xtabs(~p_pss_grapes_freq + risk_status_mom, data = r01_intake))
p_chknug_freq_fisher <- fisher.test(xtabs(~p_pss_chkn_nug_freq + risk_status_mom, data = r01_intake))
p_mac_freq_fisher <- fisher.test(xtabs(~p_pss_mac_cheese_freq + risk_status_mom, data = r01_intake))

## 1) Base Portion Size ####

grams_ps_mod <- lmer(grams ~ preFF + fmi + sex + age_yr + cebq_sr + avg_vas + meal_order + ps_prop + (1|sub), data = intake_long)

kcal_ps_mod <- lmer(kcal ~ preFF + fmi + sex + age_yr + cebq_sr + avg_vas + meal_order + ps_prop + (1|sub), data = intake_long)


## test quadratic
intake_long$ps_prop2 <- intake_long$ps_prop*intake_long$ps_prop

grams_psquad_mod <- lmer(grams ~ preFF + fmi + sex + age_yr + cebq_sr + avg_vas + meal_order + ps_prop + ps_prop2 + (1|sub), data = intake_long)

kcal_psquad_mod <- lmer(kcal ~ preFF + fmi + sex + age_yr + cebq_sr + avg_vas + meal_order + ps_prop + ps_prop2 + (1|sub), data = intake_long)

anova(grams_ps_mod, grams_psquad_mod)
anova(kcal_ps_mod, kcal_psquad_mod)

## 2) Risk Status x Portion Size ####

### a) Intake ####

##grams 
grams_psxrisk_mod <- lmer(grams ~ preFF + fmi + sex + age_yr + cebq_sr + avg_vas + meal_order + risk_status_mom*ps_prop + (1|sub), data = intake_long)

anova(grams_ps_mod, grams_psxrisk_mod)


### adjusted intake
intake_long_model <- intake_long[!is.na(intake_long$preF) & !is.na(intake_long$avg_vas) & !is.na(intake_long$meal_order), ]

#need to remove 128-portion 4 bc mac and cheese was thrown out before measured post meal
intake_long_model <- intake_long_model[!is.na(intake_long_model$grams), ]

intake_long_model$grams_pred_rxps <- predict(grams_psxrisk_mod, type = 'response')

### post-hoc tests
grams_psxrisk_slopes <- emtrends(grams_psxrisk_mod, specs=pairwise~risk_status_mom, var="ps_prop")

grams_psxrisk_dat <- data.frame(factor(intake_long_model$ps_prop), intake_long_model$risk_status_mom)

grams_psxrisk_emmeans_tab <- means.function.na(intake_long_model, intake_long_model$grams_pred_rxps, grams_psxrisk_dat)

grams_ps1risk_ttest <- t.test(grams_pred_rxps ~ risk_status_mom, data = intake_long_model[intake_long_model$ps_prop == 0, ])
grams_ps2risk_ttest <- t.test(grams_pred_rxps ~ risk_status_mom, data = intake_long_model[intake_long_model$ps_prop == 0.33, ])
grams_ps3risk_ttest <- t.test(grams_pred_rxps ~ risk_status_mom, data = intake_long_model[intake_long_model$ps_prop == 0.66, ])
grams_ps4risk_ttest <- t.test(grams_pred_rxps ~ risk_status_mom, data = intake_long_model[intake_long_model$ps_prop == 0.99, ])

## kcal
kcal_psxrisk_mod <- lmer(kcal ~ preFF + fmi + sex + age_yr + cebq_sr + avg_vas + meal_order + risk_status_mom*ps_prop + (1|sub), data = intake_long)

anova(kcal_ps_mod, kcal_psxrisk_mod)

### adjusted intake
intake_long_model$kcal_pred_rxps <- predict(kcal_psxrisk_mod, type = 'response')

### post-hoc tests
kcal_psxrisk_slopes <- emtrends(kcal_psxrisk_mod, specs=pairwise~risk_status_mom,var="ps_prop")

kcal_psxrisk_dat <- data.frame(factor(intake_long_model$ps_prop), intake_long_model$risk_status_mom)

kcal_psxrisk_emmeans_tab <- means.function.na(intake_long_model, intake_long_model$kcal_pred_rxps, kcal_psxrisk_dat)

kcal_ps1risk_ttest <- t.test(kcal_pred_rxps ~ risk_status_mom, data = intake_long_model[intake_long_model$ps_prop == 0, ])
kcal_ps2risk_ttest <- t.test(kcal_pred_rxps ~ risk_status_mom, data = intake_long_model[intake_long_model$ps_prop == 0.33, ])
kcal_ps3risk_ttest <- t.test(kcal_pred_rxps ~ risk_status_mom, data = intake_long_model[intake_long_model$ps_prop == 0.66, ])
kcal_ps4risk_ttest <- t.test(kcal_pred_rxps ~ risk_status_mom, data = intake_long_model[intake_long_model$ps_prop == 0.99, ])

### g) Energy Density ####

intake_long$ED <- intake_long$kcal/intake_long$grams

ed_psxrisk_mod <- lmer(ED ~ preFF + fmi + sex + age_yr + cebq_sr + avg_vas + meal_order + risk_status_mom*ps_prop + (1|sub), data = intake_long)

ed_psxrisk_psquad_mod <- lmer(ED ~ preFF + fmi + sex + age_yr + cebq_sr + avg_vas + meal_order + risk_status_mom*ps_prop + ps_prop2 + (1|sub), data = intake_long)

anova(ed_psxrisk_mod, ed_psxrisk_psquad_mod)


## 3) test individual food patterns ####

## a) chicken nuggets ####
## a1 - Check quadratic ####
#grams
intake_long$chnug_grams <- as.numeric(intake_long$chnug_grams)
grams_chnug_ps_mod <- lmer(chnug_grams ~ preFF + fmi + sex + age_yr + cebq_sr + chnug_vas + meal_order + ps_prop + (1 | sub), data = intake_long)

grams_chnug_ps_psquad_mod <- lmer(chnug_grams ~ preFF + fmi + sex + age_yr + cebq_sr + chnug_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(grams_chnug_ps_mod, grams_chnug_ps_psquad_mod)

#kcal
intake_long$chnug_kcal <- as.numeric(intake_long$chnug_kcal)
kcal_chnug_ps_mod <- lmer(chnug_kcal ~ preFF + fmi + sex + age_yr + cebq_sr + chnug_vas + meal_order + ps_prop + (1 | sub), data = intake_long)

kcal_chnug_ps_psquad_mod <- lmer(chnug_kcal ~ preFF + fmi + sex + age_yr + cebq_sr + chnug_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(kcal_chnug_ps_mod, kcal_chnug_ps_psquad_mod)

## a2 - check interaction ####
#grams
grams_chnug_psxrisk_mod <- lmer(chnug_grams ~ preFF + fmi + sex + age_yr + cebq_sr + chnug_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

grams_chnug_psxrisk_slopes <- emtrends(grams_chnug_psxrisk_mod, specs=pairwise~risk_status_mom, var="ps_prop")

grams_chnug_ps_risk_mod <- lmer(chnug_grams ~ preFF + fmi + sex + age_yr + cebq_sr + chnug_vas + meal_order + ps_prop + risk_status_mom + (1 | sub), data = intake_long)

intakefoods_long_model <- intake_long[!is.na(intake_long$meal_order) & !is.na(intake_long$preFF), ]

intakefoods_long_model$chnug_grams_pred <- predict(grams_chnug_ps_risk_mod, type = 'response')

#kcal
kcal_chnug_psxrisk_mod <- lmer(chnug_kcal ~ preFF + fmi + sex + age_yr + cebq_sr + chnug_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

kcal_chnug_ps_risk_mod <- lmer(chnug_kcal ~ preFF + fmi + sex + age_yr + cebq_sr + chnug_vas + meal_order + ps_prop + risk_status_mom + (1 | sub), data = intake_long)

intakefoods_long_model$chnug_kcal_pred <- predict(kcal_chnug_ps_risk_mod, type = 'response')

## b) mac and cheese ####
## b1 - Check quadratic ####
#grams
intake_long$mac_grams <- as.numeric(intake_long$mac_grams)
grams_mac_ps_mod <- lmer(mac_grams ~ preFF + fmi + sex + age_yr + cebq_sr + mac_vas + meal_order + ps_prop + (1 | sub), data = intake_long)

grams_mac_ps_psquad_mod <- lmer(mac_grams ~ preFF + fmi + sex + age_yr + cebq_sr + mac_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(grams_mac_ps_mod, grams_mac_ps_psquad_mod)

#kcal
intake_long$mac_kcal <- as.numeric(intake_long$mac_kcal)
kcal_mac_ps_mod <- lmer(mac_kcal ~ preFF + fmi + sex + age_yr + mac_vas + cebq_sr + meal_order + ps_prop + (1 | sub), data = intake_long)

kcal_mac_ps_psquad_mod <- lmer(mac_kcal ~ preFF + fmi + sex + age_yr + cebq_sr + mac_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(kcal_mac_ps_mod, kcal_mac_ps_psquad_mod)

## b2 - check interaction ####
#grams
grams_mac_psxrisk_mod <- lmer(mac_grams ~ preFF + fmi + sex + age_yr + cebq_sr + mac_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

grams_mac_ps_risk_mod <- lmer(mac_grams ~ preFF + fmi + sex + age_yr + cebq_sr + mac_vas + meal_order + ps_prop + risk_status_mom + (1 | sub), data = intake_long)

intakefoods_mac_long_model <- intakefoods_long_model[!is.na(intakefoods_long_model$mac_grams), ]

intakefoods_mac_long_model$mac_grams_pred <- predict(grams_mac_ps_risk_mod, type = 'response')

#kcal
kcal_mac_psxrisk_mod <- lmer(mac_kcal ~ preFF + fmi + sex + age_yr + cebq_sr + mac_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

kcal_chnug_psxrisk_slopes <- emtrends(kcal_chnug_psxrisk_mod, specs=pairwise~risk_status_mom, var="ps_prop")

kcal_mac_ps_risk_mod <- lmer(mac_kcal ~ preFF + fmi + sex + age_yr + cebq_sr + mac_vas + meal_order + ps_prop +risk_status_mom + (1 | sub), data = intake_long)

intakefoods_mac_long_model$mac_kcal_pred <- predict(kcal_mac_ps_risk_mod, type = 'response')

## c) grapes ####
## c1 - Check quadratic ####
#grams
intake_long$grape_grams <- as.numeric(intake_long$grape_grams)
grams_grape_ps_mod <- lmer(grape_grams ~ preFF + fmi + sex + age_yr + cebq_sr + grape_vas + meal_order + ps_prop + (1 | sub), data = intake_long)

grams_grape_ps_psquad_mod <- lmer(grape_grams ~ preFF + fmi + sex + age_yr + cebq_sr + grape_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(grams_grape_ps_mod, grams_grape_ps_psquad_mod)

#kcal
intake_long$grape_kcal <- as.numeric(intake_long$grape_kcal)
kcal_grape_ps_mod <- lmer(grape_kcal ~ preFF + fmi + sex + age_yr + cebq_sr + grape_vas + meal_order + ps_prop + (1 | sub), data = intake_long)

kcal_grape_ps_psquad_mod <- lmer(grape_kcal ~ preFF + fmi + sex + age_yr + cebq_sr + grape_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(kcal_grape_ps_mod, kcal_grape_ps_psquad_mod)

## c2 - check interaction ####
#grams
grams_grape_psxrisk_mod <- lmer(grape_grams ~ preFF + fmi + sex + age_yr + cebq_sr + grape_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

grams_grape_ps_risk_mod <- lmer(grape_grams ~ preFF + fmi + sex + age_yr + cebq_sr + grape_vas + meal_order + ps_prop + risk_status_mom + (1 | sub), data = intake_long)

intakefoods_long_model$grape_grams_pred <- predict(grams_grape_ps_risk_mod, type = 'response')

#kcal
kcal_grape_psxrisk_mod <- lmer(grape_kcal ~ preFF + fmi + sex + age_yr + cebq_sr + grape_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

kcal_grape_ps_risk_mod <- lmer(grape_kcal ~ preFF + fmi + sex + age_yr + cebq_sr + grape_vas + meal_order + ps_prop + risk_status_mom + (1 | sub), data = intake_long)

intakefoods_long_model$grape_kcal_pred <- predict(kcal_grape_ps_risk_mod, type = 'response')

## c) brocoli ####
## c1 - Check quadratic ####
#grams
intake_long$broc_grams <- as.numeric(intake_long$broc_grams)
grams_broc_ps_mod <- lmer(broc_grams ~ preFF + fmi + sex + age_yr + cebq_sr + broc_vas + meal_order + ps_prop + (1 | sub), data = intake_long)

grams_broc_ps_psquad_mod <- lmer(broc_grams ~ preFF + fmi + sex + age_yr + cebq_sr + broc_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(grams_broc_ps_mod, grams_broc_ps_psquad_mod)

#kcal
intake_long$broc_kcal <- as.numeric(intake_long$broc_kcal)
kcal_broc_ps_mod <- lmer(broc_kcal ~ preFF + fmi + sex + cebq_sr + broc_vas + age_yr + meal_order + ps_prop + (1 | sub), data = intake_long)

kcal_broc_ps_psquad_mod <- lmer(broc_kcal ~ preFF + fmi + sex + age_yr + cebq_sr + broc_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(kcal_broc_ps_mod, kcal_broc_ps_psquad_mod)

## c2 - check interaction ####
#grams
grams_broc_psxrisk_mod <- lmer(broc_grams ~ preFF + fmi + sex + age_yr + cebq_sr + broc_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

grams_broc_psxrisk_slopes <- emtrends(grams_broc_psxrisk_mod, specs=pairwise~risk_status_mom, var="ps_prop")

intakefoods_long_model$broc_grams_pred <- predict(grams_broc_psxrisk_mod, type = 'response')

#kcal
kcal_broc_psxrisk_mod <- lmer(broc_kcal ~ preFF + fmi + sex + age_yr + cebq_sr + broc_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

kcal_broc_psxrisk_slopes <- emtrends(kcal_broc_psxrisk_mod, specs=pairwise~risk_status_mom, var="ps_prop")

intakefoods_long_model$broc_kcal_pred <- predict(kcal_broc_psxrisk_mod, type = 'response')

#merge for plot and make long
intakefoods_long_model <- merge(intakefoods_long_model, intakefoods_mac_long_model[c(1, 34, 86:87)], by = c('sub', 'ps_prop'))

intakefoods_long_graph <- melt(intakefoods_long_model[c(1, 2, 3, 84, 86, 88, 90)], id.vars = c('sub', 'risk_status_mom', 'ps_prop'))
intakefoods_long_graph$food <- ifelse(intakefoods_long_graph$variable == 'chnug_grams_pred', 'chicken nuggets', ifelse(intakefoods_long_graph$variable == 'grape_grams_pred', 'grapes', ifelse(intakefoods_long_graph$variable == 'broc_grams_pred', 'broccoli', 'mac and cheese')))
intakefoods_long_graph$grams <- intakefoods_long_graph$value

intakefoods_long_graph_kcal <- melt(intakefoods_long_model[c(1, 85, 87, 89, 91)], id.vars = 'sub')
names(intakefoods_long_graph_kcal)[3] <- 'kcal'

intakefoods_long_graph <- cbind.data.frame(intakefoods_long_graph[c(1:3, 6:7)], intakefoods_long_graph_kcal[3])

## 5) mediated moderation ####
intake_long$risk_status_mom_dummy <- ifelse(intake_long$risk_status_mom == 'Low Risk', 0, 1)
intake_long$psxrisk_int <- intake_long$ps_prop*intake_long$risk_status_mom_dummy

broc_med_mod_psgrams <- ' 
  # random intercept
  grams ~ sub

  # direct effect
  grams ~ preFF + fmi + sex + age_yr + cebq_sr + avg_vas + meal_order + risk_status_mom + ps_prop + c*psxrisk_int
  
  # mediator
  broc_grams ~ preFF + fmi + sex + age_yr + cebq_sr + broc_vas + meal_order + risk_status_mom + ps_prop + a*psxrisk_int
  
  grams ~ b*broc_grams
  
  # indirect effect (a*b)
  ab := a*b
           
  # total effect
  total := c + (a*b)
'

med_mod_psgrams_fit = sem(broc_med_mod_psgrams, data = intake_long, cluster = 'sub')
#summary(med_mod_psgrams_fit)

## kcal

broc_med_mod_pskcal <- ' 
  # random intercept
  kcal ~ sub

  # direct effect
  kcal ~ preFF + fmi + sex + age_yr + cebq_sr + avg_vas + meal_order + risk_status_mom + ps_prop + c*psxrisk_int
  
  # mediator
  broc_kcal ~ preFF + fmi + sex + age_yr + cebq_sr + broc_vas + meal_order + risk_status_mom + ps_prop + a*psxrisk_int
  
  kcal ~ b*broc_kcal
  
  # indirect effect (a*b)
  ab := a*b
           
  # total effect
  total := c + (a*b)
'

med_mod_pskcal_fit = sem(broc_med_mod_pskcal, data = intake_long, cluster = 'sub')
#summary(med_mod_pskcal_fit)

#### Remove 95% plate cleaners ####
intake_long_explateclean <- intake_long[intake_long$plate_cleaner == 0, ]

## Risk Status x Portion Size ####

### a) Intake ####

##grams 
grams_psxrisk_mod_noplateclean <- lmer(grams ~ preFF + fmi + sex + age_yr + cebq_sr + avg_vas + meal_order + risk_status_mom*ps_prop + (1|sub), data = intake_long_explateclean)

## kcal
kcal_psxrisk_mod_noplateclean <- lmer(kcal ~ preFF + fmi + sex + age_yr + cebq_sr + avg_vas + meal_order + risk_status_mom*ps_prop + (1|sub), data = intake_long_explateclean)

