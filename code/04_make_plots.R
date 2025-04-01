rm(list = ls())

library(tidyr)
library(ggplot2)
library(gridExtra)
library(plyr)
library(data.table)

# Load simulations from the article, along
# with sensitivity simulations for the appendix. 

df_main <- fread("./data/sim_results.csv")
df_tunits <- fread("./data/sim_results_varying_treated_units.csv")
df_synth  <- fread("./data/sim_results_synthetic_states.csv")

# Load synthetic state data
synth_data <- fread("./data/df_synthetic_states.csv")

# Load scenarios
scenarios <- fread("./data/scenarios.csv")

# Plot of policy scenario:
p <- ggplot(scenarios, aes(x = ttt, y = smd)) +
        geom_point(size = 2.5) +
        geom_line(linewidth = 0.8) +
        facet_wrap(~scenario_name_label) +
        labs(x = "Time since treatment",
             y = "Standardized \nMean Difference") +
        scale_y_continuous(breaks = seq(0., 0.2, 0.05),
                           limits = c(0, 0.2)) +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text = element_text(family = 'sans', size = 14),
              axis.text = element_text(family = 'sans', size = 10),
              axis.title.x = element_text(family = 'sans', size = 12),
              axis.title.y = element_text(family = 'sans', size = 12, angle = 0, vjust = 0.5))

ggsave(filename = "./plots/fig_1_policy_scenarios.pdf", plot = p, height = 6, width = 8, units = "in")
  
# Produce summary statistics used in the plots.

prep_summary <- function(df_sim){
  
  df_sim[, scenario := paste(effect_magnitude1, effect_magnitude2, effect_magnitude3, effect_magnitude4, effect_magnitude5, effect_magnitude6)]
  
  # Reshape effect columns long; this should probably occur during the simulation procedure
  id_cols <- c("outcome", "n_implementation_periods", "n_units", "policy_speed", 
               "effect_direction", "model_formula", "model_name", "model_call", "scenario", "iter")
  
  sum_cols <- c("n_units", "scenario", "model_name", "ttt")
  
  df_sim <- melt(df_sim, id.vars = id_cols, 
                 measure = patterns(estimate = "^estimate_ttt==", 
                                    se = "^se_ttt==", 
                                    variance = "^variance_ttt==", 
                                    t_stat = "^t_stat_ttt==", 
                                    p_value = "^p_value_ttt==",  
                                    true_effect = "^effect_magnitude"), variable.name = "ttt")
  
  # Index time to treat correctly:
  df_sim[, ttt := as.numeric(ttt)]
  df_sim[, ttt := ttt - 1]
  
  # Remove rows where errors occurred.
  # This affects 68 autoregressive models out of 84k sims and 1 CSA run.
  df_sim <- df_sim[!is.na(estimate) & !is.na(se),]
  
  # Calculate GoF statistics
  
  #Median sim error & 2.5th/97.5th percentiles
  df_sim[, sim_error := abs(true_effect - estimate)]
  df_sim[, sim_error_std := abs((true_effect - estimate)/sd(overdoses$crude.rate, na.rm = T))]
  
  df_sim[, sim_error_50 := quantile(.SD$sim_error, 0.5), by = sum_cols]
  df_sim[, sim_error_025 := quantile(.SD$sim_error, 0.025), by = sum_cols]
  df_sim[, sim_error_975 := quantile(.SD$sim_error, 0.975), by = sum_cols]
  
  df_sim[, sim_error_50_std := quantile(.SD$sim_error_std, 0.5), by = sum_cols]
  df_sim[, sim_error_025_std := quantile(.SD$sim_error_std, 0.025), by = sum_cols]
  df_sim[, sim_error_975_std := quantile(.SD$sim_error_std, 0.975), by = sum_cols]
  
  #Median sim variance & 2.5th/97.5th percentiles
  df_sim[, sim_variance := var(.SD$estimate), by = sum_cols]
  
  #RMSE
  df_sim[, sim_rmse := sum(.SD$sim_error^2)/.N, by = sum_cols]
  
  #Model coverage
  df_sim[, covered := ifelse((estimate + 1.96*se >= true_effect) & (estimate - 1.96*se <= true_effect), 1, 0)]
  df_sim[, coverage := sum(.SD$covered)/.N, by = sum_cols]
  
  # Average model effect and variance
  df_sim[, model_est_mean := mean(.SD$estimate), by = sum_cols]
  df_sim[, model_est_025 := mean(.SD$estimate) - 1.96*(sd(.SD$estimate)), by = sum_cols]
  df_sim[, model_est_975 := mean(.SD$estimate) + 1.96*(sd(.SD$estimate)), by = sum_cols]
  
  df_sim[, model_se_50  := quantile(.SD$se, 0.5), by = sum_cols]
  df_sim[, model_se_025 := quantile(.SD$se, 0.025), by = sum_cols]
  df_sim[, model_se_975 := quantile(.SD$se, 0.975), by = sum_cols]
  
  # Change variable coding to be a little more informative on scenarios, model names,
  # and time to treat
  
  # Get list of effects which correspond to each scenario. Use this to merge with
  # the OPTIC output which details scenarios. So split the vector into list items
  # (6 at a time), then convert to a string matching the OPTIC format.
  time_varying_scenarios <- split(scenarios$true_effect, ceiling(seq_along(scenarios$true_effect)/6))
  time_varying_scenarios <- sapply(time_varying_scenarios, paste, collapse = " ")
  
  scenario_names <- data.frame("scenario_name" = c("Ramp Up", "Ramp Down",
                                                   "Temporary", "Inconsistent"),
                               "scenario" = time_varying_scenarios)
  
  df_summary <- merge(df_sim, scenario_names, by = "scenario")
  df_summary[, scenario_name_label := factor(scenario_name, levels = scenario_names$scenario_name)]
  
  model_names_remap <- data.frame("new_model_name" = c("DID-ES", "AR-DB",  "DID-HT", "DID-SA", 
                                                       "DID-2S", "DID-IMP", "ASCM"),
                                  "model_name" = c("twfe", "autoregressive", "sa", "csa_did", "gardner", "bjs", "augsynth"))
  
  df_summary <- merge(df_summary, model_names_remap, by = "model_name")
  df_summary[, new_model_name := factor(new_model_name, levels = model_names_remap$new_model_name)]
  
  df_summary[, n_units_name := paste0(n_units, " treated units")]
  df_summary[, n_units_name := factor(n_units_name, levels = unique(df_summary$n_units_name))]
  
  df_summary[, scenario := NULL]
  df_summary[, model_name := NULL]
  setnames(df_summary, "new_model_name", "model_name")
  
  # Collapse to summaries:
  df_summary <- unique(df_summary[, .(n_units, n_units_name, scenario_name_label, model_name, ttt,
                                      sim_error_50, sim_error_025,
                                      sim_error_975, sim_error_50_std, sim_error_025_std, sim_error_975_std,
                                      model_est_mean, model_est_025, model_est_975, model_se_50, model_se_025, model_se_975,
                                      coverage, sim_rmse, sim_variance)])
  
  # Add on true effects to calculate percentages for plots:
  df_summary <- join(df_summary, scenarios, by = c("ttt", "scenario_name_label"), type = "left")
  
  return(df_summary)
  
}
  
df_summary        <- prep_summary(df_main)
df_summary_tunits <- prep_summary(df_tunits)
df_summary_synth  <- prep_summary(df_synth)

# Hold onto results for 25 units in df_summary; separate out units
# in appendix df:
df_summary <- df_summary[n_units_name == "25 treated units",]

df_summary_tunits_5  <- df_summary_tunits[n_units_name == "5 treated units",]
df_summary_tunits_45 <- df_summary_tunits[n_units_name == "45 treated units",]

plot_colors <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f', '#cab2d6')


#############
# GOF Plots #
#############

#####################
# Sim error v. se  #
#####################

error_se <- function(dd, scene){

  dd <- dd[n_units == 25 & scenario_name_label == scene,]

  plot_scatter <- ggplot(dd, aes(x = model_se_50, y= sim_error_50, color = model_name)) +
    facet_wrap(~ttt, nrow = 2) +
    geom_point(size = 3, shape = 19, alpha = 0.9) +
    labs(title = sprintf("Median absolute error and standard errors\n by time-since-treatment: %s scenario", scene),
         y = "Error",
         x = "Standard errors",
         color = "Estimator") +
    theme_bw() +
    scale_x_continuous(limits = c(0, 2.5),
                       breaks = seq(0, 2.5, 0.5)) +
    scale_y_continuous(limits = c(0, 1.75),
                       breaks = seq(0, 1.75, 0.25)) +
    scale_color_manual(values = plot_colors) +
    theme(plot.title = element_text(hjust = 0.5, family = 'sans', size = 16),
          strip.background = element_blank(),
          strip.text = element_text(family = "sans", size = 10),
          legend.position = "bottom",
          legend.text = element_text(family = 'sans', size = 10),
          axis.ticks = element_line(linewidth = 1),
          axis.ticks.length = unit(5.6, "points"),
          axis.title = element_text(size = 12, family = 'sans'),
          axis.title.y = element_text(size = 12, family = 'sans', angle = 0),
          axis.text = element_text(size = 10, family = 'sans'),
          axis.text.x = element_text(size = 10, family = 'sans',
                                     margin = margin(t = 5, r = 0, b = 10, l = 0)),
          legend.title = element_text(family = 'sans', size = 14))

  return(plot_scatter)

}

args <- c("Ramp Up", "Ramp Down", "Temporary", "Inconsistent")

bias_v_variance_plots <- lapply(args, error_se, dd = df_summary)

ggsave(filename = "./plots/appendix/fig_s1_model_error_v_se.pdf", 
       plot = marrangeGrob(bias_v_variance_plots, nrow=1, ncol=1, top=NULL),
       height = 8.27, width = 11.69, units = "in")

############
# Sim bias #
############

sim_bias <- function(dd){
  ggplot(dd, aes(x = ttt, y = sim_error_50_std, color = model_name)) +
               geom_point(size = 2, shape = 19) +
               geom_line(size = 0.5, linetype = 2) +
               facet_wrap(~scenario_name_label) +
               theme_bw() +
               labs(
                    y = "Bias  ",
                    x = "Years since treatment",
                    color = "Estimator") +
               scale_color_manual(values = plot_colors) +
               guides(color = guide_legend(nrow = 2, byrow = T)) +
               theme(plot.title = element_text(hjust = 0.5, family = 'sans', size = 18),
                     strip.background = element_blank(),
                     strip.text = element_text(family = "sans", size = 14),
                     legend.position = "bottom",
                     legend.text = element_text(family = 'sans', size = 12),
                     axis.ticks = element_line(linewidth = 1),
                     axis.ticks.length = unit(5.6, "points"),
                     axis.title = element_text(size = 14, family = 'sans'),
                     axis.title.y = element_text(size = 14, family = 'sans', angle = 0),
                     axis.text = element_text(size = 10, family = 'sans'),
                     axis.text.x = element_text(size = 10, family = 'sans',
                                                margin = margin(t = 5, r = 0, b = 10, l = 0)),
                     legend.title = element_text(family = 'sans', size = 12))
}

fig_2 <- sim_bias(df_summary) +
        scale_y_continuous(breaks = seq(0, 0.3, 0.1),
                   limits = c(0, 0.3)) 
   
ggsave(filename = "./plots/fig_2_bias.pdf", 
        plot = fig_2, height = 8.27, width = 11.69, units = "in")

fig_s2_5 <- sim_bias(df_summary_tunits_5) +
            scale_y_continuous(breaks = seq(0, 0.5, 0.1),
                               limits = c(0, 0.5)) 
          
ggsave(filename = "./plots/appendix/fig_2_bias_5_units.pdf", 
       plot = fig_s2_5, height = 8.27, width = 11.69, units = "in")

fig_s2_45 <- sim_bias(df_summary_tunits_45) +
              scale_y_continuous(breaks = seq(0, 0.4, 0.1),
                                 limits = c(0, 0.4)) 

ggsave(filename = "./plots/appendix/fig_2_bias_45_units.pdf", 
       plot = fig_s2_45, height = 8.27, width = 11.69, units = "in")

fig_s2_synth <- sim_bias(df_summary_synth) +
                scale_y_continuous(breaks = seq(0, 0.3, 0.1),
                                   limits = c(0, 0.3)) 
              
ggsave(filename = "./plots/appendix/fig_2_bias_synthetic.pdf", 
       plot = fig_s2_synth, height = 8.27, width = 11.69, units = "in")

# Summary statistics for paper:
overall <- df_summary[, mean(.SD$sim_error_50_std), by = "model_name"]
setorder(overall, V1)

overall_scenario <- df_summary[, mean(.SD$sim_error_50_std), by = "scenario_name_label"]

scenario <- df_summary[, mean(.SD$sim_error_50_std), by = c("model_name", "scenario_name_label")]
setorder(scenario, scenario_name_label, V1)
 
################
# Sim variance #
################

sim_se <- function(dd){
  ggplot(dd, aes(x = ttt, y = model_se_50, color = model_name)) +
                geom_point(size = 2, shape = 19) +
                geom_line(size = 0.5, linetype = 2) +
                facet_wrap(~scenario_name_label) +
                theme_bw() +
                labs(y = "Standard \nError",
                     x = "Years since treatment",
                     color = "Estimator") +
                scale_color_manual(values = plot_colors) +
                theme(plot.title = element_text(hjust = 0.5, family = 'sans', size = 16),
                      strip.background = element_blank(),
                      strip.text = element_text(family = "sans", size = 10),
                      legend.position = "bottom",
                      legend.text = element_text(family = 'sans', size = 10),
                      axis.ticks = element_line(linewidth = 1),
                      axis.ticks.length = unit(5.6, "points"),
                      axis.title = element_text(size = 12, family = 'sans'),
                      axis.title.y = element_text(size = 12, family = 'sans', angle = 0),
                      axis.text = element_text(size = 10, family = 'sans'),
                      axis.text.x = element_text(size = 10, family = 'sans',
                                                 margin = margin(t = 5, r = 0, b = 10, l = 0)),
                      legend.title = element_text(family = 'sans', size = 14))
}

fig_3 <- sim_se(df_summary)  +
          scale_y_continuous(breaks = seq(0, 2.5, 0.5),
                             limits = c(0, 2.5)) 

ggsave(filename = "./plots/fig_3_model_se.pdf", plot = fig_3, 
       height = 8.27, width = 11.69, units = "in")

fig_s3_5 <- sim_se(df_summary_tunits_5)  +
              scale_y_continuous(breaks = seq(0, 3.5, 0.5),
                                 limits = c(0, 3.5)) 

ggsave(filename = "./plots/appendix/fig_3_model_se_5_units.pdf", plot = fig_s3_5, 
       height = 8.27, width = 11.69, units = "in")

fig_s3_45 <- sim_se(df_summary_tunits_45)  +
              scale_y_continuous(breaks = seq(0, 2.5, 0.5),
                                 limits = c(0, 2.5)) 

ggsave(filename = "./plots/appendix/fig_3_model_se_45_units.pdf", plot = fig_s3_45, 
       height = 8.27, width = 11.69, units = "in")

fig_s3_synth <- sim_se(df_summary_synth)  +
                scale_y_continuous(breaks = seq(0, 2.5, 0.5),
                                   limits = c(0, 2.5)) 

ggsave(filename = "./plots/appendix/fig_3_model_se_synthetic.pdf", plot = fig_s3_synth, 
       height = 8.27, width = 11.69, units = "in")

overall <- df_summary[, mean(.SD$model_se_50), by = "model_name"]
setorder(overall, V1)

overall_scenario <- df_summary[, mean(.SD$model_se_50), by = "scenario_name_label"]

scenario <- df_summary[, mean(.SD$model_se_50), by = c("model_name", "scenario_name_label")]
setorder(scenario, scenario_name_label, V1)

############
# Coverage #
############

coverage <- function(dd){
  ggplot(dd, aes(x = ttt, y = coverage, color = model_name)) +
            geom_line(size = 0.5, linetype = 2) +
            geom_point(size = 2, shape = 19) +
            geom_hline(size = 0.5, linetype = 3, yintercept = 0.95, alpha = 0.7) +
            facet_wrap(~scenario_name_label) +
            theme_bw() +
            labs(y = "Coverage",
                 x = "Years since treatment",
                 color = "Estimator") +
            scale_color_manual(values = plot_colors) +
            scale_y_continuous(labels = scales::percent) +
            theme(plot.title = element_text(hjust = 0.5, family = 'sans', size = 16),
                  strip.background = element_blank(),
                  strip.text = element_text(family = "sans", size = 10),
                  legend.position = "bottom",
                  legend.text = element_text(family = 'sans', size = 10),
                  axis.ticks = element_line(linewidth = 1),
                  axis.ticks.length = unit(5.6, "points"),
                  axis.title = element_text(size = 12, family = 'sans'),
                  axis.title.y = element_text(size = 12, family = 'sans', angle = 0),
                  axis.text = element_text(size = 10, family = 'sans'),
                  axis.text.x = element_text(size = 10, family = 'sans',
                                             margin = margin(t = 5, r = 0, b = 10, l = 0)),
                  legend.title = element_text(family = 'sans', size = 14))
}

ggsave(filename = "./plots/fig_4_coverage.pdf", 
       plot = coverage(df_summary), 
       height = 8.27, width = 11.69, units = "in")

ggsave(filename = "./plots/appendix/fig_4_coverage_5_units.pdf", 
       plot = coverage(df_summary_tunits_5), 
       height = 8.27, width = 11.69, units = "in")

ggsave(filename = "./plots/appendix/fig_4_coverage_45_units.pdf", 
       plot = coverage(df_summary_tunits_45), 
       height = 8.27, width = 11.69, units = "in")

ggsave(filename = "./plots/appendix/fig_4_coverage_synthetic.pdf", 
       plot = coverage(df_summary_synth), 
       height = 8.27, width = 11.69, units = "in")

overall <- df_summary[, mean(.SD$coverage), by = "model_name"]
setorder(overall, V1)

overall_scenario <- df_summary[, mean(.SD$coverage), by = "scenario_name_label"]

scenario <- df_summary[, mean(.SD$coverage), by = c("model_name", "scenario_name_label")]
setorder(scenario, scenario_name_label, V1)

########
# RMSE #
########

rmse <- function(dd){
  ggplot(dd, aes(x = ttt, y = sim_rmse, color = model_name)) +
          geom_point(size = 2, shape = 19) +
          geom_line(size = 0.5, linetype = 2) +
          facet_wrap(~scenario_name_label) +
          theme_bw() +
          labs(y = "RMSE",
               x = "Years since treatment",
               color = "Estimator") +
          scale_color_manual(values = plot_colors) +
          theme(plot.title = element_text(hjust = 0.5, family = 'sans', size = 16),
                strip.background = element_blank(),
                strip.text = element_text(family = "sans", size = 10),
                legend.position = "bottom",
                legend.text = element_text(family = 'sans', size = 10),
                axis.ticks = element_line(linewidth = 1),
                axis.ticks.length = unit(5.6, "points"),
                axis.title = element_text(size = 12, family = 'sans'),
                axis.title.y = element_text(size = 12, family = 'sans', angle = 0),
                axis.text = element_text(size = 10, family = 'sans'),
                axis.text.x = element_text(size = 10, family = 'sans',
                                           margin = margin(t = 5, r = 0, b = 10, l = 0)),
                legend.title = element_text(family = 'sans', size = 14))
}

fig_5 <- rmse(df_summary)  +
        scale_y_continuous(breaks = seq(0, 8, 2),
                           limits = c(0, 8)) 

ggsave(filename = "./plots/fig_5_rmse.pdf", plot = fig_5, 
       height = 8.27, width = 11.69, units = "in")

fig_s5_5 <- rmse(df_summary_tunits_5) +
            scale_y_continuous(breaks = seq(0, 50, 10),
                               limits = c(0, 50)) 

ggsave(filename = "./plots/appendix/fig_5_rmse_5_units.pdf", plot = fig_s5_5, 
       height = 8.27, width = 11.69, units = "in")

fig_s5_45 <- rmse(df_summary_tunits_45) +
              scale_y_continuous(breaks = seq(0, 10, 2),
                                 limits = c(0, 10)) 
            
ggsave(filename = "./plots/appendix/fig_5_rmse_45_units.pdf", plot = fig_s5_45, 
       height = 8.27, width = 11.69, units = "in")

fig_s5_synth <- rmse(df_summary_synth)  +
                  scale_y_continuous(breaks = seq(0, 8, 2),
                                     limits = c(0, 8))

ggsave(filename = "./plots/appendix/fig_5_rmse_synthetic.pdf", plot = fig_s5_synth, 
       height = 8.27, width = 11.69, units = "in")

# Create appendix plots concerning synthetic state data

cruderate <- ggplot(synth_data, aes(x = crude.rate, color = observed_data)) + 
  geom_density(size = 1) + 
  facet_wrap(~year) + 
  theme_bw() +
  labs(x = "Crude Mortality Rate", y = "Density",
       color = "Observed data") +
  theme(strip.background = element_blank(),
        legend.position = "bottom")

unemploymentrate <- ggplot(synth_data, aes(x = unemploymentrate, color = observed_data)) + 
  geom_density(size = 1) + 
  facet_wrap(~year) + 
  theme_bw() +
  labs(x = "Unemployment Rate", y = "Density",
       color = "Observed data") +
  theme(strip.background = element_blank(),
        legend.position = "bottom")

ggsave(filename = "./plots/appendix/density_unemploymentrate.pdf", 
       plot = unemploymentrate,
       height = 8.27, width = 11.69, units = "in")

ggsave(filename = "./plots/appendix/density_cruderate.pdf", 
       plot = cruderate,
       height = 8.27, width = 11.69, units = "in")  
