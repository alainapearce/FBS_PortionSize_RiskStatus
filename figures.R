# This script report was written by Alaina Pearce paper examining 
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

# library(ggplot2)
#
# source('functions.R')

# source('setup_data.R')

## Portion x Risk ####
psxr_g <- ggplot(aes(x = g_served, y = grams, color = risk_status_mom), data = intake_long) + 
  labs(y = "Total Intake, Grams - Adjusted", x = "Weight Served (centered at reference portion)") +
  geom_line(aes(group = sub), size = 1, alpha = 0.5, color = 'grey') +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
  geom_segment(aes(y=769, yend=1011.4, x=0, xend=0.33), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=1011.4, yend=1254.8, x=0.33, xend=0.66), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=1254.8, yend=1499.2, x=0.66, xend=0.99), colour = "black", size=1, show.legend=F) +
  scale_color_manual(values = c('purple4', 'darkorange1')) +
  scale_x_continuous(breaks=c(0.0, 242.4, 485.8, 730.2)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90))

psxr_g_noline <- ggplot(aes(x = g_served, y = grams, color = risk_status_mom), data = intake_long) + 
  labs(y = "Total Intake, Grams - Adjusted", x = "Weight Served (centered at reference portion)") +
  geom_line(aes(group = sub), size = 1, alpha = 0.5, color = 'grey') +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
  scale_color_manual(values = c('purple4', 'darkorange1')) +
  scale_x_continuous(breaks=c(0.0, 242.4, 485.8, 730.2)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90))

psxr_kcal <- ggplot(aes(x = kcal_served, y = kcal, color = risk_status_mom), data = intake_long) + 
  labs(y = "Total Intake, kcal - Adjusted", x = "Energy Served (centered at reference portion)") +
  geom_line(aes(group = sub), size = 1, alpha = 0.5, color = 'grey') +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE) +
  geom_segment(aes(y=1029, yend=1408.8, x=0, xend=328.8), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=1408.8, yend=1680.6, x=328.8, xend=658.6), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=1680.6, yend=2006.4, x=658.6, xend=989.4), colour = "black", size=1, show.legend=F) +
  scale_color_manual(values = c('purple4', 'darkorange1')) +
  scale_x_continuous(breaks=c(0.0, 328.8, 658.6, 989.4)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90))

psxr_kcal_noline <- ggplot(aes(x = kcal_served, y = kcal, color = risk_status_mom), data = intake_long) + 
  labs(y = "Total Intake, kcal - Adjusted", x = "Energy Served (centered at reference portion)") +
  geom_line(aes(group = sub), size = 1, alpha = 0.5, color = 'grey') +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE) +
  scale_color_manual(values = c('purple4', 'darkorange1')) +
  scale_x_continuous(breaks=c(0.0, 328.8, 658.6, 989.4)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90))

## Portion x Risk by Food
psxr_eachfood_g <- ggplot(aes(x = g_served, y = grams, group = interaction(food, risk_status_mom), color = food, linetype = risk_status_mom), data = intakefoods_long_graph) + 
  labs(y = "Total Intake, Grams - Adjusted", x = "Weight Served (centered at reference portion)") +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE) +
  scale_color_manual(values = c('mediumseagreen', 'orange3', 'purple4', 'goldenrod1')) +
  scale_x_continuous(breaks=c(0.0, 242.4, 485.8, 730.2)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90))

psxr_eachfood_kcal <- ggplot(aes(x = kcal_served, y = kcal, group = interaction(food, risk_status_mom), color = food, linetype = risk_status_mom), data = intakefoods_long_graph) + 
  labs(y = "Total Intake, kcal - Adjusted", x = "Energy Served (centered at reference portion)") +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE) +
  scale_color_manual(values = c('mediumseagreen', 'orange3', 'purple4', 'goldenrod1')) +
  scale_x_continuous(breaks=c(0.0, 328.8, 658.6, 989.4)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90))

## Chicken Nuggets
psxr_chnug_g <- ggplot(aes(x = g_served, y = chnug_grams, group = risk_status_mom, color = risk_status_mom), data = intake_long) + 
  labs(y = "Chicken Nugget Intake, Grams - Raw", x = "Weight Served (centered at reference portion)") +
  geom_line(aes(group = sub), size = 1, alpha = 0.5, color = 'grey') +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE) +
  geom_segment(aes(y=115, yend=149, x=0, xend=242.4), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=149, yend=184, x=242.4, xend=485.8), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=184, yend=222, x=485.8, xend=730.2), colour = "black", size=1, show.legend=F) +
  scale_color_manual(values = c('purple4', 'darkorange1')) +
  scale_x_continuous(breaks=c(0.0, 242.4, 485.8, 730.2)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90))

psxr_chnug_kcal <- ggplot(aes(x = kcal_served, y = chnug_kcal, group = risk_status_mom, color = risk_status_mom), data = intake_long) + 
  labs(y = "Chicken Nugget Intake, kcal - Raw", x = "Energy Served (centered at reference portion)") +
  geom_line(aes(group = sub), size = 1, alpha = 0.5, color = 'grey') +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_segment(aes(y=287.5, yend=372.5, x=0, xend=328.8), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=372.5, yend=460, x=328.8, xend=658.6), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=460, yend=555, x=658.6, xend=989.4), colour = "black", size=1, show.legend=F) +
  scale_color_manual(values = c('purple4', 'darkorange1')) +
  scale_x_continuous(breaks=c(0.0, 328.8, 658.6, 989.4)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90))

## Mac and Cheese
psxr_mac_g <- ggplot(aes(x = g_served, y = mac_grams, group = risk_status_mom, color = risk_status_mom), data = intake_long) + 
  labs(y = "Mac and Cheese Intake, Grams - Raw", x = "Weight Served (centered at reference portion)") +
  geom_line(aes(group = sub), size = 1, alpha = 0.5, color = 'grey') +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE) +
  geom_segment(aes(y=270, yend=351, x=0, xend=242.4), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=351, yend=432, x=242.4, xend=485.8), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=432, yend=513, x=485.8, xend=730.2), colour = "black", size=1, show.legend=F) +
  scale_color_manual(values = c('purple4', 'darkorange1')) +
  scale_x_continuous(breaks=c(0.0, 242.4, 485.8, 730.2)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90))

psxr_mac_kcal <- ggplot(aes(x = kcal_served, y = mac_kcal, group = risk_status_mom, color = risk_status_mom), data = intake_long) + 
  labs(y = "Mac and Cheese Intake, kcal - Raw", x = "Energy Served (centered at reference portion)") +
  geom_line(aes(group = sub), size = 1, alpha = 0.5, color = 'grey') +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_segment(aes(y=459, yend=596.7, x=0, xend=328.8), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=596.7, yend=734.4, x=328.8, xend=658.6), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=734.4, yend=872.1, x=658.6, xend=989.4), colour = "black", size=1, show.legend=F) +
  scale_color_manual(values = c('purple4', 'darkorange1')) +
  scale_x_continuous(breaks=c(0.0, 328.8, 658.6, 989.4)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90))

## Grapes
psxr_grape_g <- ggplot(aes(x = g_served, y = grape_grams, group = risk_status_mom, color = risk_status_mom), data = intake_long) + 
  labs(y = "Grapes Intake, Grams - Raw", x = "Weight Served (centered at reference portion)") +
  geom_line(aes(group = sub), size = 1, alpha = 0.5, color = 'grey') +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE) +
  geom_segment(aes(y=200, yend=266, x=0, xend=242.4), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=266, yend=332, x=242.4, xend=485.8), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=332, yend=398, x=485.8, xend=730.2), colour = "black", size=1, show.legend=F) +
  scale_color_manual(values = c('purple4', 'darkorange1')) +
  scale_x_continuous(breaks=c(0.0, 242.4, 485.8, 730.2)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90))

psxr_grape_kcal <- ggplot(aes(x = kcal_served, y = grape_kcal, group = risk_status_mom, color = risk_status_mom), data = intake_long) + 
  labs(y = "Grapes Intake, kcal - Raw", x = "Energy Served (centered at reference portion)") +
  geom_line(aes(group = sub), size = 1, alpha = 0.5, color = 'grey') +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_segment(aes(y=140, yend=186.2, x=0, xend=328.8), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=186.2, yend=232.4, x=328.8, xend=658.6), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=232.4, yend=278.6, x=658.6, xend=989.4), colour = "black", size=1, show.legend=F) +
  scale_color_manual(values = c('purple4', 'darkorange1')) +
  scale_x_continuous(breaks=c(0.0, 328.8, 658.6, 989.4)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90))

## Broccoli
psxr_broc_g <- ggplot(aes(x = g_served, y = broc_grams, group = risk_status_mom, color = risk_status_mom), data = intake_long) + 
  labs(y = "Broccoli Intake, Grams - Raw", x = "Weight Served (centered at reference portion)") +
  geom_line(aes(group = sub), size = 1, alpha = 0.5, color = 'grey') +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE) +
  geom_segment(aes(y=199, yend=261.4, x=0, xend=242.4), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=261.4, yend=324.8, x=242.4, xend=485.8), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=324.8, yend=389.2, x=485.8, xend=730.2), colour = "black", size=1, show.legend=F) +
  scale_color_manual(values = c('purple4', 'darkorange1')) +
  scale_x_continuous(breaks=c(0.0, 242.4, 485.8, 730.2)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90))

psxr_broc_kcal <- ggplot(aes(x = kcal_served, y = broc_kcal, group = risk_status_mom, color = risk_status_mom), data = intake_long) + 
  labs(y = "Broccoli Intake, kcal - Raw", x = "Energy Served (centered at reference portion)") +
  geom_line(aes(group = sub), size = 1, alpha = 0.5, color = 'grey') +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE) +
  geom_segment(aes(y=199, yend=261.4, x=0, xend=328.8), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=261.4, yend=324.8, x=328.8, xend=658.6), colour = "black", size=1, show.legend=F) +
  geom_segment(aes(y=324.8, yend=389.2, x=658.6, xend=989.4), colour = "black", size=1, show.legend=F) +
  scale_color_manual(values = c('purple4', 'darkorange1')) +
  scale_x_continuous(breaks=c(0.0, 328.8, 658.6, 989.4)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_blank(), axis.text.x = element_text(angle = 90))
