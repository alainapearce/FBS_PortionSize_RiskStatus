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

## long by food, for each portion - for rank data only
ps1_rank <- intake_long[intake_long$PortionSize == 'PS-1', ]
ps1_rank <- ps1_rank[!is.na(ps1_rank$mac_rank) & !is.na(ps1_rank$chnug_rank) & !is.na(ps1_rank$grape_rank) & !is.na(ps1_rank$broc_rank), ]
ps1_rank_long <- melt(ps1_rank[c(1:2, 4, 6, 9, 16, 30:33)], id.vars = names(ps1_rank[c(1:2, 4, 6, 9, 16)]))
names(ps1_rank_long)[7] <- 'food'
names(ps1_rank_long)[8] <- 'rank'
ps1_rank_long <- ps1_rank_long[order(ps1_rank_long$sub), ]

ps2_rank <- intake_long[intake_long$PortionSize == 'PS-2', ]
ps2_rank <- ps2_rank[!is.na(ps2_rank$mac_rank) & !is.na(ps2_rank$chnug_rank) & !is.na(ps2_rank$grape_rank) & !is.na(ps2_rank$broc_rank), ]
ps2_rank_long <- melt(ps2_rank[c(1:2, 4, 6, 9, 16, 30:33)], id.vars = names(ps2_rank[c(1:2, 4, 6, 9, 16)]))
names(ps2_rank_long)[7] <- 'food'
names(ps2_rank_long)[8] <- 'rank'
ps2_rank_long <- ps2_rank_long[order(ps2_rank_long$sub), ]

ps3_rank <- intake_long[intake_long$PortionSize == 'PS-3', ]
ps3_rank <- ps3_rank[!is.na(ps3_rank$mac_rank) & !is.na(ps3_rank$chnug_rank) & !is.na(ps3_rank$grape_rank) & !is.na(ps3_rank$broc_rank), ]
ps3_rank_long <- melt(ps3_rank[c(1:2, 4, 6, 9, 16, 30:33)], id.vars = names(ps3_rank[c(1:2, 4, 6, 9, 16)]))
names(ps3_rank_long)[7] <- 'food'
names(ps3_rank_long)[8] <- 'rank'
ps3_rank_long <- ps3_rank_long[order(ps3_rank_long$sub), ]

ps4_rank <- intake_long[intake_long$PortionSize == 'PS-4', ]
ps4_rank <- ps4_rank[!is.na(ps4_rank$mac_rank) & !is.na(ps4_rank$chnug_rank) & !is.na(ps4_rank$grape_rank) & !is.na(ps4_rank$broc_rank), ]
ps4_rank_long <- melt(ps4_rank[c(1:2, 4, 6, 9, 16, 30:33)], id.vars = names(ps4_rank[c(1:2, 4, 6, 9, 16)]))
names(ps4_rank_long)[7] <- 'food'
names(ps4_rank_long)[8] <- 'rank'
ps4_rank_long <- ps4_rank_long[order(ps4_rank_long$sub), ]

#continuous approach:
#intake_long$ps_prop <- ifelse(intake_long[['PortionSize']] == 'PS-1', 0, ifelse(intake_long[['PortionSize']] == 'PS-2', 0.33, ifelse(intake_long[['PortionSize']] == 'PS-3', 0.66, 0.99)))

intake_long$g_served <- ifelse(intake_long[['PortionSize']] == 'PS-1', 0, ifelse(intake_long[['PortionSize']] == 'PS-2', 242.4, ifelse(intake_long[['PortionSize']] == 'PS-3', 485.8, 730.2)))

intake_long$kcal_served <- ifelse(intake_long[['PortionSize']] == 'PS-1', 0, ifelse(intake_long[['PortionSize']] == 'PS-2', 328.8, ifelse(intake_long[['PortionSize']] == 'PS-3', 658.6, 989.4)))

## Load CEBQ and PSS data ####
r01_eat_qs <- as.data.frame(read_spss(("data/qs_eatbeh_bodyimage.sav")))
names(r01_eat_qs)[1] <- 'sub'

r01_eat_qs_labels <- lapply(r01_eat_qs, function(x) attributes(x)$label)

#fix factors
r01_eat_qs[c(121:123, 139:141, 151:153, 157:159, 198:203, 234:239, 258:263, 270:275)] <- sapply(r01_eat_qs[c(121:123, 139:141, 151:153, 157:159, 198:203, 234:239, 258:263, 270:275)], as.numeric)

r01_eat_qs$pss_broccoli_eat <- ifelse(r01_eat_qs$pss_broccoli_eat == 1, 'Y', ifelse(r01_eat_qs$pss_broccoli_eat == 0, 'N', as.character(r01_eat_qs$pss_broccoli_eat)))
r01_eat_qs$pss_broccoli_eat <- factor(r01_eat_qs$pss_broccoli_eat)

r01_eat_qs$pss_grapes_eat <- ifelse(r01_eat_qs$pss_grapes_eat == 1, 'Y', ifelse(r01_eat_qs$pss_grapes_eat == 0, 'N', as.character(r01_eat_qs$pss_grapes_eat)))
r01_eat_qs$pss_grapes_eat <- factor(r01_eat_qs$pss_grapes_eat)

r01_eat_qs$pss_chkn_nug_eat <- ifelse(r01_eat_qs$pss_chkn_nug_eat == 1, 'Y', ifelse(r01_eat_qs$pss_chkn_nug_eat == 0, 'N', as.character(r01_eat_qs$pss_chkn_nug_eat)))
r01_eat_qs$pss_chkn_nug_eat <- factor(r01_eat_qs$pss_chkn_nug_eat)

r01_eat_qs$pss_mac_cheese_eat <- ifelse(r01_eat_qs$pss_mac_cheese_eat == 1, 'Y', ifelse(r01_eat_qs$pss_mac_cheese_eat == 0, 'N', as.character(r01_eat_qs$pss_mac_cheese_eat)))
r01_eat_qs$pss_mac_cheese_eat <- factor(r01_eat_qs$pss_mac_cheese_eat)

r01_eat_qs$p_pss_broccoli_eat <- ifelse(r01_eat_qs$p_pss_broccoli_eat == 1, 'Y', ifelse(r01_eat_qs$p_pss_broccoli_eat == 0, 'N', as.character(r01_eat_qs$p_pss_broccoli_eat)))
r01_eat_qs$p_pss_broccoli_eat <- factor(r01_eat_qs$p_pss_broccoli_eat)

r01_eat_qs$p_pss_grapes_eat <- ifelse(r01_eat_qs$p_pss_grapes_eat == 1, 'Y', ifelse(r01_eat_qs$p_pss_grapes_eat == 0, 'N', as.character(r01_eat_qs$p_pss_grapes_eat)))
r01_eat_qs$p_pss_grapes_eat <- factor(r01_eat_qs$p_pss_grapes_eat)

r01_eat_qs$p_pss_chkn_nug_eat <- ifelse(r01_eat_qs$p_pss_chkn_nug_eat == 1, 'Y', ifelse(r01_eat_qs$p_pss_chkn_nug_eat == 0, 'N', as.character(r01_eat_qs$p_pss_chkn_nug_eat)))
r01_eat_qs$p_pss_chkn_nug_eat <- factor(r01_eat_qs$p_pss_chkn_nug_eat)

r01_eat_qs$p_pss_mac_cheese_eat <- ifelse(r01_eat_qs$p_pss_mac_cheese_eat == 1, 'Y', ifelse(r01_eat_qs$p_pss_mac_cheese_eat == 0, 'N', as.character(r01_eat_qs$p_pss_mac_cheese_eat)))
r01_eat_qs$p_pss_mac_cheese_eat <- factor(r01_eat_qs$p_pss_mac_cheese_eat)

r01_eat_qs$p_pss_broccoli_freq <- ifelse(r01_eat_qs$p_pss_broccoli_freq == 0, 'Never - 1/month', ifelse(r01_eat_qs$p_pss_broccoli_freq == 1, '2-3/month', ifelse(r01_eat_qs$p_pss_broccoli_freq == 2, '1-2/week', ifelse(r01_eat_qs$p_pss_broccoli_freq == 3, '3-4/week', ifelse(r01_eat_qs$p_pss_broccoli_freq == 4, '5-6/week', ifelse(r01_eat_qs$p_pss_broccoli_freq == 5, '1/day', ifelse(r01_eat_qs$p_pss_broccoli_freq == 6, '2+/day', as.character(r01_eat_qs$p_pss_broccoli_freq))))))))
r01_eat_qs$p_pss_broccoli_freq <- factor(r01_eat_qs$p_pss_broccoli_freq, levels = c('Never - 1/month', '2-3/month', '1-2/week', '3-4/week', '5-6/week', '1/day', '2+/day'))
r01_eat_qs$p_pss_broccoli_freq <- droplevels(r01_eat_qs$p_pss_broccoli_freq)


r01_eat_qs$p_pss_grapes_freq <- ifelse(r01_eat_qs$p_pss_grapes_freq == 0, 'Never - 1/month', ifelse(r01_eat_qs$p_pss_grapes_freq == 1, '2-3/month', ifelse(r01_eat_qs$p_pss_grapes_freq == 2, '1-2/week', ifelse(r01_eat_qs$p_pss_grapes_freq == 3, '3-4/week', ifelse(r01_eat_qs$p_pss_grapes_freq == 4, '5-6/week', ifelse(r01_eat_qs$p_pss_grapes_freq == 5, '1/day', ifelse(r01_eat_qs$p_pss_grapes_freq == 6, '2+/day', as.character(r01_eat_qs$p_pss_grapes_freq))))))))
r01_eat_qs$p_pss_grapes_freq <- factor(r01_eat_qs$p_pss_grapes_freq, levels = c('Never - 1/month', '2-3/month', '1-2/week', '3-4/week', '5-6/week', '1/day', '2+/day'))
r01_eat_qs$p_pss_grapes_freq <- droplevels(r01_eat_qs$p_pss_grapes_freq)

r01_eat_qs$p_pss_chkn_nug_freq <- ifelse(r01_eat_qs$p_pss_chkn_nug_freq == 0, 'Never - 1/month', ifelse(r01_eat_qs$p_pss_chkn_nug_freq == 1, '2-3/month', ifelse(r01_eat_qs$p_pss_chkn_nug_freq == 2, '1-2/week', ifelse(r01_eat_qs$p_pss_chkn_nug_freq == 3, '3-4/week', ifelse(r01_eat_qs$p_pss_chkn_nug_freq == 4, '5-6/week', ifelse(r01_eat_qs$p_pss_chkn_nug_freq == 5, '1/day', ifelse(r01_eat_qs$p_pss_chkn_nug_freq == 6, '2+/day', as.character(r01_eat_qs$p_pss_chkn_nug_freq))))))))
r01_eat_qs$p_pss_chkn_nug_freq <- factor(r01_eat_qs$p_pss_chkn_nug_freq, levels = c('Never - 1/month', '2-3/month', '1-2/week', '3-4/week', '5-6/week', '1/day', '2+/day'))
r01_eat_qs$p_pss_chkn_nug_freq <- droplevels(r01_eat_qs$p_pss_chkn_nug_freq)

r01_eat_qs$p_pss_mac_cheese_freq <- ifelse(r01_eat_qs$p_pss_mac_cheese_freq == 0, 'Never - 1/month', ifelse(r01_eat_qs$p_pss_mac_cheese_freq == 1, '2-3/month', ifelse(r01_eat_qs$p_pss_mac_cheese_freq == 2, '1-2/week', ifelse(r01_eat_qs$p_pss_mac_cheese_freq == 3, '3-4/week', ifelse(r01_eat_qs$p_pss_mac_cheese_freq == 4, '5-6/week', ifelse(r01_eat_qs$p_pss_mac_cheese_freq == 5, '1/day', ifelse(r01_eat_qs$p_pss_mac_cheese_freq == 6, '2+/day', as.character(r01_eat_qs$p_pss_mac_cheese_freq))))))))
r01_eat_qs$p_pss_mac_cheese_freq <- factor(r01_eat_qs$p_pss_mac_cheese_freq, levels = c('Never - 1/month', '2-3/month', '1-2/week', '3-4/week', '5-6/week', '1/day', '2+/day'))
r01_eat_qs$p_pss_mac_cheese_freq <- droplevels(r01_eat_qs$p_pss_mac_cheese_freq)

# merge
intake_long <- merge(intake_long, r01_eat_qs[c(1, 121:123, 139:141, 151:153, 157:159, 198:203, 234:239, 258:263, 270:275, 479:486)], by = 'sub', all.x = TRUE)
r01_intake <- merge(r01_intake, r01_eat_qs[c(1, 121:123, 139:141, 151:153, 157:159, 198:203, 234:239, 258:263, 270:275, 479:486, 444:478)], by = 'sub', all.x = TRUE)

## Load anthro data ####
r01_anthro_data <- as.data.frame(read_spss(("data/anthro_data.sav")))
names(r01_anthro_data)[1] <- 'sub'

r01_anthro_data_labels <- lapply(r01_anthro_data, function(x) attributes(x)$label)

# fat mass index
r01_anthro_data$dxa_total_fat_mass <- as.numeric(r01_anthro_data$dxa_total_fat_mass)
r01_anthro_data$dxa_total_body_perc_fat <- as.numeric(r01_anthro_data$dxa_total_body_perc_fat)

r01_anthro_data[['fmi']] <- (r01_anthro_data$dxa_total_fat_mass/1000)/(r01_anthro_data$height_avg/100)^2

# merge
intake_long <- merge(intake_long, r01_anthro_data[c(1, 106, 119, 570)], by = 'sub', all.x = TRUE)
r01_intake <- merge(r01_intake, r01_anthro_data[c(1, 106, 119, 570)], by = 'sub', all.x = TRUE)
