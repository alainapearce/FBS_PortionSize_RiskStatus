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

## 1) Base Portion Size ####

grams_ps_mod <- lmer(grams ~ preFF + bmi + sex + avg_vas + meal_order + ps_prop + (1|sub), data = intake_long)

kcal_ps_mod <- lmer(kcal ~ preFF + bmi + sex+ avg_vas + meal_order + ps_prop + (1|sub), data = intake_long)


## test quadratic
intake_long$ps_prop2 <- intake_long$ps_prop*intake_long$ps_prop

grams_psquad_mod <- lmer(grams ~ preFF + bmi + sex + avg_vas + meal_order + ps_prop + ps_prop2 + (1|sub), data = intake_long)

kcal_psquad_mod <- lmer(kcal ~ preFF + bmi + sex + avg_vas + meal_order + ps_prop + ps_prop2 + (1|sub), data = intake_long)

anova(grams_ps_mod, grams_psquad_mod)
anova(kcal_ps_mod, kcal_psquad_mod)

## 2) Risk Status x Portion Size ####

### a) Intake ####

##grams 
grams_psxrisk_psquad_mod <- lmer(grams ~ preFF + bmi + sex + age_yr + avg_vas + meal_order + risk_status_mom*ps_prop + ps_prop2 + (1|sub), data = intake_long)

anova(grams_psquad_mod, grams_psxrisk_psquad_mod)

grams_psxrisk_psquadxrisk_mod <- lmer(grams ~ preFF + bmi + sex + age_yr + avg_vas + meal_order + risk_status_mom*ps_prop + risk_status_mom*ps_prop2 + (1|sub), data = intake_long)

### adjusted intake
intake_long_model <- intake_long[!is.na(intake_long$preF) & !is.na(intake_long$avg_vas) & !is.na(intake_long$meal_order), ]

#need to remove 128-portion 4 bc mac and cheese was thrown out before measured post meal
intake_long_model <- intake_long_model[!is.na(intake_long_model$grams), ]

intake_long_model$grams_pred_rxps <- predict(grams_psxrisk_psquad_mod, type = 'response')

### post-hoc tests
grams_psxrisk_psquad_slopes <- emtrends(grams_psxrisk_psquad_mod, specs=pairwise~risk_status_mom, var="ps_prop")

grams_psxrisk_dat <- data.frame(factor(intake_long_model$ps_prop), intake_long_model$risk_status_mom)

grams_psxrisk_emmeans_tab <- means.function.na(intake_long_model, intake_long_model$grams_pred_rxps, grams_psxrisk_dat)

grams_ps1risk_ttest <- t.test(grams_pred_rxps ~ risk_status_mom, data = intake_long_model[intake_long_model$ps_prop == 0, ])
grams_ps2risk_ttest <- t.test(grams_pred_rxps ~ risk_status_mom, data = intake_long_model[intake_long_model$ps_prop == 0.33, ])
grams_ps3risk_ttest <- t.test(grams_pred_rxps ~ risk_status_mom, data = intake_long_model[intake_long_model$ps_prop == 0.66, ])
grams_ps4risk_ttest <- t.test(grams_pred_rxps ~ risk_status_mom, data = intake_long_model[intake_long_model$ps_prop == 0.99, ])

## test interaction with quadratic
grams_psxrisk_psquadxrisk_mod <- lmer(grams ~ preFF + bmi + sex + age_yr + avg_vas + meal_order + ps_prop*ps_prop2 + risk_status_mom*ps_prop2 + (1|sub), data = intake_long)

anova(grams_psxrisk_psquad_mod, grams_psxrisk_psquadxrisk_mod)

## kcal
kcal_psxrisk_mod <- lmer(kcal ~ preFF + bmi + sex + age_yr + avg_vas + meal_order + risk_status_mom*ps_prop + (1|sub), data = intake_long)

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

### g) Energy Denisty ####

intake_long$ED <- intake_long$kcal/intake_long$grams

ed_psxrisk_mod <- lmer(ED ~ preFF + bmi + sex + age_yr + avg_vas + meal_order + risk_status_mom*ps_prop + (1|sub), data = intake_long)

ed_psxrisk_psquad_mod <- lmer(ED ~ preFF + bmi + sex + age_yr + avg_vas + meal_order + risk_status_mom*ps_prop + ps_prop2 + (1|sub), data = intake_long)

anova(ed_psxrisk_mod, ed_psxrisk_psquad_mod)


## 3) check BMI x Risk Status Group x Portion Size ####

bmi_mod <- lm(bmi ~ age_yr + sex + risk_status_mom, data = r01_intake)

## grams
grams_psxrisk_psxbmi_psquad_mod <- lmer(grams ~ preFF + bmi + sex + age_yr + avg_vas + meal_order + risk_status_mom*ps_prop + bmi*ps_prop + ps_prop2 + (1|sub), data = intake_long)

anova(grams_psxrisk_psquad_mod, grams_psxrisk_psxbmi_psquad_mod)

## kcal
kcal_psxrisk_psxbmi_mod <- lmer(kcal ~ preFF + bmi + sex + age_yr + avg_vas + meal_order + risk_status_mom*ps_prop + bmi*ps_prop + ps_prop2 + (1|sub), data = intake_long)

anova(kcal_psxrisk_mod, kcal_psxrisk_psxbmi_mod)


## 4) test individual food patterns ####

## a) chicken nuggets ####
## a1 - Check quadratic ####
#grams
intake_long$chnug_grams <- as.numeric(intake_long$chnug_grams)
grams_chnug_ps_mod <- lmer(chnug_grams ~ preFF + bmi + sex + chnug_vas + meal_order + ps_prop + (1 | sub), data = intake_long)

grams_chnug_ps_psquad_mod <- lmer(chnug_grams ~ preFF + bmi + sex + chnug_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(grams_chnug_ps_mod, grams_chnug_ps_psquad_mod)

#kcal
intake_long$chnug_kcal <- as.numeric(intake_long$chnug_kcal)
kcal_chnug_ps_mod <- lmer(chnug_kcal ~ preFF + bmi + sex + chnug_vas + meal_order + ps_prop + (1 | sub), data = intake_long)

kcal_chnug_ps_psquad_mod <- lmer(chnug_kcal ~ preFF + bmi + sex + chnug_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(kcal_chnug_ps_mod, kcal_chnug_ps_psquad_mod)

## a2 - check interaction ####
#grams
grams_chnug_psxrisk_mod <- lmer(chnug_grams ~ preFF + bmi + sex + chnug_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

grams_chnug_psxrisk_slopes <- emtrends(grams_chnug_psxrisk_mod, specs=pairwise~risk_status_mom, var="ps_prop")

grams_chnug_ps_risk_mod <- lmer(chnug_grams ~ preFF + bmi + sex + chnug_vas + meal_order + ps_prop + risk_status_mom + (1 | sub), data = intake_long)

intakefoods_long_model <- intake_long[!is.na(intake_long$meal_order) & !is.na(intake_long$preFF), ]

intakefoods_long_model$chnug_grams_pred <- predict(grams_chnug_ps_risk_mod, type = 'response')

#kcal
kcal_chnug_psxrisk_mod <- lmer(chnug_kcal ~ preFF + bmi + sex + chnug_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

kcal_chnug_ps_risk_mod <- lmer(chnug_kcal ~ preFF + bmi + sex + chnug_vas + meal_order + ps_prop + risk_status_mom + (1 | sub), data = intake_long)

## b) mac and cheese ####
## b1 - Check quadratic ####
#grams
intake_long$mac_grams <- as.numeric(intake_long$mac_grams)
grams_mac_ps_mod <- lmer(mac_grams ~ preFF + bmi + sex + mac_vas + meal_order + ps_prop + (1 | sub), data = intake_long)

grams_mac_ps_psquad_mod <- lmer(mac_grams ~ preFF + bmi + sex + mac_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(grams_mac_ps_mod, grams_mac_ps_psquad_mod)

#kcal
intake_long$mac_kcal <- as.numeric(intake_long$mac_kcal)
kcal_mac_ps_mod <- lmer(mac_kcal ~ preFF + bmi + sex + mac_vas + meal_order + ps_prop + (1 | sub), data = intake_long)

kcal_mac_ps_psquad_mod <- lmer(mac_kcal ~ preFF + bmi + sex + mac_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(kcal_mac_ps_mod, kcal_mac_ps_psquad_mod)

## b2 - check interaction ####
#grams
grams_mac_psxrisk_mod <- lmer(mac_grams ~ preFF + bmi + sex + mac_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

grams_mac_ps_risk_mod <- lmer(mac_grams ~ preFF + bmi + sex + mac_vas + meal_order + ps_prop + risk_status_mom + (1 | sub), data = intake_long)

intakefoods_mac_long_model <- intakefoods_long_model[!is.na(intakefoods_long_model$mac_grams), ]

intakefoods_mac_long_model$mac_grams_pred <- predict(grams_mac_ps_risk_mod, type = 'response')

#kcal
kcal_mac_psxrisk_mod <- lmer(mac_kcal ~ preFF + bmi + sex + mac_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

kcal_chnug_psxrisk_slopes <- emtrends(kcal_chnug_psxrisk_mod, specs=pairwise~risk_status_mom, var="ps_prop")

kcal_mac_ps_risk_mod <- lmer(mac_kcal ~ preFF + bmi + sex + mac_vas + meal_order + ps_prop +risk_status_mom + (1 | sub), data = intake_long)

## c) grapes ####
## c1 - Check quadratic ####
#grams
intake_long$grape_grams <- as.numeric(intake_long$grape_grams)
grams_grape_ps_mod <- lmer(grape_grams ~ preFF + bmi + sex + grape_vas + meal_order + ps_prop + (1 | sub), data = intake_long)

grams_grape_ps_psquad_mod <- lmer(grape_grams ~ preFF + bmi + sex + grape_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(grams_grape_ps_mod, grams_grape_ps_psquad_mod)

#kcal
intake_long$grape_kcal <- as.numeric(intake_long$grape_kcal)
kcal_grape_ps_mod <- lmer(grape_kcal ~ preFF + bmi + sex + grape_vas + meal_order + ps_prop + (1 | sub), data = intake_long)

kcal_grape_ps_psquad_mod <- lmer(grape_kcal ~ preFF + bmi + sex + grape_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(kcal_grape_ps_mod, kcal_grape_ps_psquad_mod)

## c2 - check interaction ####
#grams
grams_grape_psxrisk_mod <- lmer(grape_grams ~ preFF + bmi + sex + grape_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

grams_grape_ps_risk_mod <- lmer(grape_grams ~ preFF + bmi + sex + grape_vas + meal_order + ps_prop + risk_status_mom + (1 | sub), data = intake_long)

intakefoods_long_model$grape_grams_pred <- predict(grams_grape_ps_risk_mod, type = 'response')

#kcal
kcal_grape_psxrisk_mod <- lmer(grape_kcal ~ preFF + bmi + sex + grape_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

kcal_grape_ps_risk_mod <- lmer(grape_kcal ~ preFF + bmi + sex + grape_vas + meal_order + ps_prop + risk_status_mom + (1 | sub), data = intake_long)

## c) brocoli ####
## c1 - Check quadratic ####
#grams
intake_long$broc_grams <- as.numeric(intake_long$broc_grams)
grams_broc_ps_mod <- lmer(broc_grams ~ preFF + bmi + sex + broc_vas + meal_order + ps_prop + (1 | sub), data = intake_long)

grams_broc_ps_psquad_mod <- lmer(broc_grams ~ preFF + bmi + sex + broc_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(grams_broc_ps_mod, grams_broc_ps_psquad_mod)

#kcal
intake_long$broc_kcal <- as.numeric(intake_long$broc_kcal)
kcal_broc_ps_mod <- lmer(broc_kcal ~ preFF + bmi + sex + broc_vas + meal_order + ps_prop + (1 | sub), data = intake_long)

kcal_broc_ps_psquad_mod <- lmer(broc_kcal ~ preFF + bmi + sex + broc_vas + meal_order + ps_prop + ps_prop2 + (1 | sub), data = intake_long)

anova(kcal_broc_ps_mod, kcal_broc_ps_psquad_mod)

## c2 - check interaction ####
#grams
grams_broc_psxrisk_mod <- lmer(broc_grams ~ preFF + bmi + sex + broc_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

grams_broc_psxrisk_slopes <- emtrends(grams_broc_psxrisk_mod, specs=pairwise~risk_status_mom, var="ps_prop")

intakefoods_long_model$broc_grams_pred <- predict(grams_broc_psxrisk_mod, type = 'response')

#kcal
kcal_broc_psxrisk_mod <- lmer(broc_kcal ~ preFF + bmi + sex + broc_vas + meal_order + ps_prop*risk_status_mom + (1 | sub), data = intake_long)

kcal_broc_psxrisk_slopes <- emtrends(kcal_broc_psxrisk_mod, specs=pairwise~risk_status_mom, var="ps_prop")

#merge for plot and make long
intakefoods_long_model <- merge(intakefoods_long_model, intakefoods_mac_long_model[c(1, 33)], by = 'sub')

intakefoods_long_model <- melt(intakefoods_long_model[c(1, 2, 29, 32:35)], id.vars = c('sub', 'risk_status_mom', 'ps_prop'))
intakefoods_long_model$food <- ifelse(intakefoods_long_model$variable == 'chnug_grams_pred', 'chicken nuggets', ifelse(intakefoods_long_model$variable == 'grape_grams_pred', 'grapes', ifelse(intakefoods_long_model$variable == 'broc_grams_pred', 'broccoli', 'mac and cheese')))
intakefoods_long_model$grams <- intakefoods_long_model$value

intakefoods_long_model <- intakefoods_long_model[c(1:3, 6:7)]

## 5) mediated moderation ####
intake_long$risk_status_mom_dummy <- ifelse(intake_long$risk_status_mom == 'Low Risk', 0, 1)
intake_long$psxrisk_int <- intake_long$ps_prop*intake_long$risk_status_mom_dummy

broc_med_mod_psgrams <- ' 
  # random intercept
  grams ~ sub

  # direct effect
  grams ~ preFF + bmi + sex + age_yr + avg_vas + meal_order + risk_status_mom + ps_prop + c*psxrisk_int + ps_prop2
  
  # mediator
  broc_grams ~ preFF + bmi + sex + age_yr + broc_vas + meal_order + risk_status_mom + ps_prop + a*psxrisk_int
  
  grams ~ b*broc_grams
  
  # indirect effect (a*b)
  ab := a*b
           
  # total effect
  total := c + (a*b)
'

med_mod_psgrams_fit = sem(broc_med_mod_psgrams, data = intake_long, cluster = 'sub')
summary(med_mod_psgrams_fit)

## kcal

broc_med_mod_pskcal <- ' 
  # random intercept
  kcal ~ sub

  # direct effect
  kcal ~ preFF + bmi + sex + age_yr + avg_vas + meal_order + risk_status_mom + ps_prop + c*psxrisk_int
  
  # mediator
  broc_kcal ~ preFF + bmi + sex + age_yr + broc_vas + meal_order + risk_status_mom + ps_prop + a*psxrisk_int
  
  kcal ~ b*broc_kcal
  
  # indirect effect (a*b)
  ab := a*b
           
  # total effect
  total := c + (a*b)
'

med_mod_pskcal_fit = sem(broc_med_mod_pskcal, data = intake_long, cluster = 'sub')
summary(med_mod_pskcal_fit)