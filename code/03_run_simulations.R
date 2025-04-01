rm(list = ls())

library(optic)
library(data.table)

df <- fread("./data/overdoses.csv")

time_varying_scenarios <- fread("./data/scenarios.csv")

df_synth <- fread("./data/df_synthetic_states.csv")

# Specify  models to simulate treatment effects:

m_es <- optic_model(
  name = "twfe",
  type = "eventstudy",
  call = "feols",
  formula = crude.rate ~ unemploymentrate + i(time_to_treat, ref = c(-1, Inf)) | state + year,
  model_args = list(cluster = "state"),
  se_adjust = "none",
)

m_sa <- optic_model(
  name = "sa",
  type = "eventstudy",
  call = "feols",
  formula = crude.rate ~ unemploymentrate + sunab(treatment_date, time_to_treat, ref.p = c(-1, .F)) | state + year,
  model_args = list(cluster = "state"),
  se_adjust = "none"
)

m_csa <- optic_model(
  name = "csa_did",
  type = "did",
  call = "att_gt",
  formula = crude.rate ~ unemploymentrate + treatment_level,
  model_args = list(yname = "crude.rate", tname = 'year', idname = 'state',
                    gname = 'treatment_date', xformla = formula(~ unemploymentrate)),
  se_adjust = "none"
  
)

m_aug <- optic_model(
  name = "augsynth",
  type = "multisynth",
  call = "multisynth",
  formula = crude.rate ~ treatment_level | unemploymentrate,
  model_args = list(unit = as.name("state"),
                    time = as.name("year"),
                    lambda = 0.1),
  se_adjust = "none"
  
)

m_bjs <- optic_model(
  name = "bjs",
  type = "did_imputation",
  call = "did_imputation",
  formula = crude.rate ~ unemploymentrate + treatment_level,
  model_args = list(yname = "crude.rate",
                    gname = "treatment_date",
                    tname = "year",
                    idname = "state"),
  se_adjust = "none"
)

m_g <- optic_model(
  name = "gardner",
  type = "did2s",
  call = "did2s",
  formula = crude.rate ~ unemploymentrate + treatment_level,
  model_args = list(yname = "crude.rate", treatment = "treatment",
                    first_stage = ~ 0 | state + year,
                    second_stage = ~ i(time_to_treat, ref = c(-1, Inf)),
                    cluster_var = "state"),
  se_adjust = "none"
)

m_ar_db <- optic_model(
  
  name = "autoregressive",
  type = "autoreg_debiased",
  call = "autoreg_debiased",
  formula = crude.rate ~ unemploymentrate + i(time_to_treat, ref = c(-1)) | year,
  model_args = list(outcome_name = "crude.rate", date_name = 'year', unit_name = 'state',
                    cov_names = "unemploymentrate", lags = 6),
  se_adjust = "none"
  
)

sim_models <- list(m_es, m_ar_db, m_sa, m_csa, m_g, m_bjs, m_aug)

sim_config <- optic_simulation(
  
  x                        = df,
  models                   = sim_models,
  iters                    = 1000,
  method                   = "time_varying",
  unit_var                 = "state",
  treat_var                = "state",
  time_var                 = "year",
  effect_magnitude         = time_varying_scenarios,
  n_units                  = c(5, 25, 45),
  effect_direction         = c("pos"),
  policy_speed             = c("instant"),
  n_implementation_periods = c(6)
  
)

sim_results <- dispatch_simulations(
  
  sim_config,
  seed = 9780,
  verbose = 0,
  use_future = T,
  future.packages = c("didimputation", "did2s", "optic","augsynth", "fixest", "tidyr")
  
)

sim_final <- rbindlist(sim_results)

# Separate out results into two sets - one for main paper,
# then varying number of units for appendix:
sim_vary_units <- sim_final[n_units != 25,]

write.csv(sim_final,  "./data/sim_results.csv", row.names = F)
write.csv(sim_vary_units,  "./data/sim_results_varying_treated_units.csv", row.names = F)

# Run models using synthetic states:

sim_config_synth <- optic_simulation(
  
  x                        = df_synth,
  models                   = sim_models,
  iters                    = 1000,
  method                   = "time_varying",
  unit_var                 = "state",
  treat_var                = "state",
  time_var                 = "year",
  effect_magnitude         = time_varying_scenarios,
  n_units                  = c(25),
  effect_direction         = c("pos"),
  policy_speed             = c("instant"),
  n_implementation_periods = c(6)
  
)

sim_results_synth <- dispatch_simulations(
  
  sim_config_synth,
  seed = 9780,
  verbose = 0,
  use_future = T,
  future.packages = c("didimputation", "did2s", "optic","augsynth", "fixest", "tidyr")
  
)

sim_final_synth <- rbindlist(sim_results_synth)

write.csv(sim_final_synth,  "./data/sim_results_synthetic_states.csv", row.names = F)

write.csv(sim_final,  "./data/sim_results.csv", row.names = F)
