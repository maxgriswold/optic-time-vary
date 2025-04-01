rm(list = ls())

library(optic)
library(plyr)
library(data.table)
library(ggplot2)

data(overdoses)

# For whatever reason, crude.rate is missing observations for North Dakota.
# So recompute this vector from the data:
df <- setDT(overdoses)

df$crude.rate <- 1e5*(df$deaths/df$population)

write.csv(df, './data/overdoses.csv', row.names = F)

# Construct a linear increase and decrease in the effect size through time,
# building towards or away from a 10% effect over a 5-year span:

full_effect <- 0.1*mean(df$crude.rate, na.rm = T)
effect_sd   <- sd(df$crude.rate, na.rm = T)

# Construct scenarios with time-varying patterns but same 
# average effect post-treatment.

linear_ramp_up <- seq(0, 1, 0.2)*full_effect
linear_sunset <- rev(seq(0, 1, 0.2))*full_effect

instant_plateau  <- c(0, 0.3, 0.7, 1, 0.8, 0.2)*full_effect
slow_plateau     <- c(0, 0.5, 0.6, 0.8, 0.5, 0.6)*full_effect

time_varying_scenarios <- list(linear_ramp_up, linear_sunset, instant_plateau, slow_plateau)
scenario_names <- c("I", "II", "III", "IV")

scenarios <- list()

for (i in 1:4){
  scene <- time_varying_scenarios[[i]]
  scenarios[[i]] <- data.table("ttt" = seq(0, length(scene) - 1),
                                       "smd" = scene/effect_sd,
                                       "true_effect" = scene,
                                       "scenario_name" = scenario_names[[i]])
}

scenarios <- rbindlist(scenarios)

long_names <- c("Ramp Up", "Ramp Down", "Temporary", "Inconsistent")
scenarios[, scenario_name_label := mapvalues(scenario_name, 
                                             c("I", "II", "III", "IV"),
                                             long_names)]

scenarios[, scenario_name_label := factor(scenario_name_label,
                                          levels = long_names)]

write.csv(scenarios, "./data/scenarios.csv")
