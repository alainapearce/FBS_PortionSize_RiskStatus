# This script was written by Alaina Pearce in August 2022
# to set up data for the paper examining 
# the portion size effect by risk status
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
# need to uncomment if running indipendently - not needed if compiling with FBS_Datablitz_2022-01-25.Rmd

# library(haven)
# library(gtsummary)
# theme_gtsummary_compact()
#
# source('functions.R')

#### set up ####
## Intake Data ####
## a) Load Data ####
r01_intake <- as.data.frame(read_spss(("data/intake_data.sav")))
names(r01_intake)[1] <- 'sub'

r01_intake_labels <- lapply(r01_intake, function(x) attributes(x)$label)

#remove 2 that were removed for ADHD
r01_intake <- r01_intake[r01_intake$sub != 31 & r01_intake$sub != 34, ]

# make numeric
r01_intake[c(606, 652, 698, 744, 607, 653, 699, 745, 115, 477, 116, 478, 166, 528, 167, 529)] <- sapply(r01_intake[c(606, 652, 698, 744, 607, 653, 699, 745, 115, 477, 116, 478, 166, 528, 167, 529)], FUN = as.numeric)

## b) Get Variable Labels and Re-Level ####

# risk status
r01_intake$risk_status_mom <- droplevels(as_factor(r01_intake$risk_status_mom))
r01_intake$risk_status_both <- droplevels(as_factor(r01_intake$risk_status_both))
r01_intake$sex <- as_factor(r01_intake$sex)

# race
r01_intake$race <- factor(r01_intake$race)

# ethnicity
r01_intake$ethnicity <- ifelse(r01_intake$ethnicity == 0, 'Not Hispanic/Lantinx', 'Hispanic/Lantinx')
r01_intake$ethnicity <- factor(r01_intake$ethnicity)

# income
r01_intake$income <- ifelse(is.na(r01_intake$income), NA, ifelse(r01_intake$income < 3, '< $51,000', ifelse(r01_intake$income < 5, "$51,000 - $100,000", '>$100,000')))
r01_intake$income <- factor(r01_intake$income)

# parental ed
r01_intake$mom_ed <- ifelse(r01_intake$measured_parent == 0, ifelse(r01_intake$parent_ed == 0, 'High School/GED', ifelse(r01_intake$parent_ed < 3, 'AA/Technical Degree', ifelse(r01_intake$parent_ed == 3, 'Bachelor Degree', ifelse(r01_intake$parent_ed < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_intake$partner_ed == 0, 'High School/GED', ifelse(r01_intake$partner_ed < 3, 'AA/Technical Degree', ifelse(r01_intake$partner_ed == 3, 'Bachelor Degree', ifelse(r01_intake$partner_ed < 8, '> Bachelor Degree', 'Other/NA')))))
r01_intake$mom_ed <- factor(r01_intake$mom_ed)

r01_intake$dad_ed <- ifelse(r01_intake$measured_parent == 1, ifelse(r01_intake$parent_ed == 0, 'High School/GED', ifelse(r01_intake$parent_ed < 3, 'AA/Technical Degree', ifelse(r01_intake$parent_ed == 3, 'Bachelor Degree', ifelse(r01_intake$parent_ed < 8, '> Bachelor Degree', 'Other/NA')))), ifelse(r01_intake$partner_ed == 0, 'High School/GED', ifelse(r01_intake$partner_ed < 3, 'AA/Technical Degree', ifelse(r01_intake$partner_ed == 3, 'Bachelor Degree', ifelse(r01_intake$partner_ed < 8, '> Bachelor Degree', 'Other/NA')))))
r01_intake$dad_ed <- factor(r01_intake$dad_ed)

## reduce to those with complete meal data
r01_intake <- r01_intake[!is.na(r01_intake$ps1_total_g) & !is.na(r01_intake$ps2_total_g) & !is.na(r01_intake$ps3_total_g) & !is.na(r01_intake$ps4_total_g), ]

## average VAS
r01_intake[c("ps1_vas_mac_cheese","ps1_vas_chkn_nug", "ps1_vas_broccoli","ps1_vas_grape", "ps2_vas_mac_cheese","ps2_vas_chkn_nug", "ps2_vas_broccoli","ps2_vas_grape", "ps3_vas_mac_cheese","ps3_vas_chkn_nug", "ps3_vas_broccoli","ps3_vas_grape", "ps4_vas_mac_cheese","ps4_vas_chkn_nug", "ps4_vas_broccoli","ps4_vas_grape")] <- sapply(r01_intake[c("ps1_vas_mac_cheese","ps1_vas_chkn_nug", "ps1_vas_broccoli","ps1_vas_grape", "ps2_vas_mac_cheese","ps2_vas_chkn_nug", "ps2_vas_broccoli","ps2_vas_grape", "ps3_vas_mac_cheese","ps3_vas_chkn_nug", "ps3_vas_broccoli","ps3_vas_grape", "ps4_vas_mac_cheese","ps4_vas_chkn_nug", "ps4_vas_broccoli","ps4_vas_grape")], FUN = as.numeric)

r01_intake[['ps1_avg_vas']] <- rowMeans(r01_intake[c("ps1_vas_mac_cheese","ps1_vas_chkn_nug", "ps1_vas_broccoli","ps1_vas_grape")])
r01_intake[['ps2_avg_vas']] <- rowMeans(r01_intake[c("ps2_vas_mac_cheese","ps2_vas_chkn_nug", "ps2_vas_broccoli","ps2_vas_grape")])
r01_intake[['ps3_avg_vas']] <- rowMeans(r01_intake[c("ps3_vas_mac_cheese","ps3_vas_chkn_nug", "ps3_vas_broccoli","ps3_vas_grape")])
r01_intake[['ps4_avg_vas']] <- rowMeans(r01_intake[c("ps4_vas_mac_cheese","ps4_vas_chkn_nug", "ps4_vas_broccoli","ps4_vas_grape")])

#get portion order
r01_intake[['ps1_visit']] <- ifelse(is.na(r01_intake[['v2_meal_ps']]), NA, ifelse(r01_intake[['v2_meal_ps']] == 0, 1, ifelse(r01_intake[['v3_meal_ps']] == 0, 2, ifelse(r01_intake[['v4_meal_ps']] == 0, 3, 4))))

r01_intake[['ps2_visit']] <- ifelse(is.na(r01_intake[['v2_meal_ps']]), NA, ifelse(r01_intake[['v2_meal_ps']] == 1, 1, ifelse(r01_intake[['v3_meal_ps']] == 1, 2, ifelse(r01_intake[['v4_meal_ps']] == 1, 3, 4))))

r01_intake[['ps3_visit']] <- ifelse(is.na(r01_intake[['v2_meal_ps']]), NA, ifelse(r01_intake[['v2_meal_ps']] == 2, 1, ifelse(r01_intake[['v3_meal_ps']] == 2, 2, ifelse(r01_intake[['v4_meal_ps']] == 2, 3, 4))))

r01_intake[['ps4_visit']] <- ifelse(is.na(r01_intake[['v2_meal_ps']]), NA, ifelse(r01_intake[['v2_meal_ps']] == 3, 1, ifelse(r01_intake[['v3_meal_ps']] == 3, 2, ifelse(r01_intake[['v4_meal_ps']] == 3, 3, 4))))

## plate cleaners
r01_intake$ps1_plate_cleaner <- ifelse(r01_intake$ps1_total_g >= 769*.95, 1, 0)
#number 17 and 110
r01_intake$ps2_plate_cleaner <- ifelse(r01_intake$ps2_total_g >= 1011*.95, 1, 0)
r01_intake$ps3_plate_cleaner <- ifelse(r01_intake$ps3_total_g >= 1255*.95, 1, 0)
r01_intake$ps4_plate_cleaner <- ifelse(r01_intake$ps4_total_g >= 1499*.95, 1, 0)

r01_intake$plate_cleaner <- rowSums(r01_intake[756:759])

## c) Make Data Long ####
intake_long <- melt(r01_intake[c(1, 8:12, 21, 606, 652, 698, 744, 760)], id.vars = names(r01_intake)[c(1, 8:12, 21, 760)])
names(intake_long)[9:10] <- c('PortionSize', 'grams')
intake_long$PortionSize <- ifelse(intake_long$PortionSize == 'ps4_total_g', 'PS-4', ifelse(intake_long$PortionSize == 'ps3_total_g', 'PS-3', ifelse(intake_long$PortionSize == 'ps2_total_g', 'PS-2', 'PS-1')))
intake_long$grams <- as.numeric(intake_long$grams)

intake_kcal_long <- melt(r01_intake[c(1, 607, 653, 699, 745)], id.vars = 'sub')
intake_long$kcal <- intake_kcal_long$value

intake_vas_long <- melt(r01_intake[c(1, 748:751)], id.vars = 'sub')
intake_long$avg_vas <- intake_vas_long$value

intake_preFF_long <- melt(r01_intake[c(1, 563, 609, 655, 701)], id.vars = 'sub')
intake_long$preFF <- as.numeric(intake_preFF_long$value)

intake_postFF_long <- melt(r01_intake[c(1, 564, 610, 656, 702)], id.vars = 'sub')
intake_long$postFF <- as.numeric(intake_preFF_long$value)

intake_date_long <- melt(r01_intake[c(1, 562, 608, 654, 700)], id.vars = 'sub')
intake_long$date <- intake_date_long$value

intake_order_long <- melt(r01_intake[c(1, 752:755)], id.vars = 'sub')
intake_long$meal_order <- intake_order_long$value

intake_meal_dur_long <- melt(r01_intake[c(1, 576, 622, 668, 714)], id.vars = 'sub')
intake_long$meal_dur <- intake_meal_dur_long$value

#chicken nuggets
intake_chnug_grams_long <- melt(r01_intake[c(1, 580, 626, 672, 718)], id.vars = 'sub')
intake_long$chnug_grams <- intake_chnug_grams_long$value

intake_chnug_kcal_long <- melt(r01_intake[c(1, 581, 627, 673, 719)], id.vars = 'sub')
intake_long$chnug_kcal <- intake_chnug_kcal_long$value

#mac and cheese
intake_mac_grams_long <- melt(r01_intake[c(1, 585, 631, 677, 723)], id.vars = 'sub')
intake_long$mac_grams <- intake_mac_grams_long$value

intake_mac_kcal_long <- melt(r01_intake[c(1, 586, 632, 678, 724)], id.vars = 'sub')
intake_long$mac_kcal <- intake_mac_kcal_long$value

#graps
intake_grape_grams_long <- melt(r01_intake[c(1, 590, 636, 682, 728)], id.vars = 'sub')
intake_long$grape_grams <- intake_grape_grams_long$value

intake_grape_kcal_long <- melt(r01_intake[c(1, 591, 637, 683, 729)], id.vars = 'sub')
intake_long$grape_kcal <- intake_grape_kcal_long$value

#brocoli
intake_broc_grams_long <- melt(r01_intake[c(1, 595, 641, 687, 733)], id.vars = 'sub')
intake_long$broc_grams <- intake_broc_grams_long$value

intake_broc_kcal_long <- melt(r01_intake[c(1, 596, 642, 688, 734)], id.vars = 'sub')
intake_long$broc_kcal <- intake_broc_kcal_long$value

#individual vas
intake_mac_vas_long <- melt(r01_intake[c(1, 565, 611, 657, 703)], id.vars = 'sub')
intake_long$mac_vas <- intake_mac_vas_long$value

intake_chnug_vas_long <- melt(r01_intake[c(1, 566, 612, 658, 704)], id.vars = 'sub')
intake_long$chnug_vas <- intake_chnug_vas_long$value

intake_broc_vas_long <- melt(r01_intake[c(1, 567, 613, 659, 705)], id.vars = 'sub')
intake_long$broc_vas <- intake_broc_vas_long$value

intake_grape_vas_long <- melt(r01_intake[c(1, 568, 614, 660, 706)], id.vars = 'sub')
intake_long$grape_vas <- intake_grape_vas_long$value

#individual ranks
intake_mac_rank_long <- melt(r01_intake[c(1, 570, 616, 662, 708)], id.vars = 'sub')
intake_long$mac_rank <- intake_mac_rank_long$value

intake_chnug_rank_long <- melt(r01_intake[c(1, 571, 617, 663, 709)], id.vars = 'sub')
intake_long$chnug_rank <- intake_chnug_rank_long$value

intake_grape_rank_long <- melt(r01_intake[c(1, 573, 619, 665, 711)], id.vars = 'sub')
intake_long$grape_rank <- intake_grape_rank_long$value

intake_broc_rank_long <- melt(r01_intake[c(1, 572, 618, 664, 710)], id.vars = 'sub')
intake_long$broc_rank <- intake_broc_rank_long$value



#continuous approach:
intake_long$ps_prop <- ifelse(intake_long[['PortionSize']] == 'PS-1', 0, ifelse(intake_long[['PortionSize']] == 'PS-2', 0.33, ifelse(intake_long[['PortionSize']] == 'PS-3', 0.66, 0.99)))

