rm(list = ls())

library(data.table)
library(scales)
library(synthpop)
library(plyr)

df <- fread("./data/overdoses.csv")

# Number of replicates:
i <- 10

# Generate an additional 510 synthetic states based off observed data:
pred_mat <- matrix(c(0, 0, 0, 0,
                     0, 0, 0, 0,
                     1, 1, 1, 0,
                     1, 1, 1, 0), 
                   byrow = T, nrow = 4)

df_true <- df[, .(state, year, unemploymentrate, crude.rate)]

df_sim <- syn(df_true, m = i, proper = F, method = "rf", seed = 191,
              visit.sequence = c(3, 4), predictor.matrix = pred_mat,
              smoothing = "spline")

# For each synthetic dataset, swap state names with a sequential number of
# fake names
fake_states <- fread("./data/fake_states.csv", fill = T)

hold <- list()

for (j in 1:i){
  old_states <- unique(df_sim$syn[[j]]$state)
  new_states <- fake_states[((j - 1)*51 + 1):(j*51)]$new_name
  
  swap <- as.data.table(df_sim$syn[[i]])
  swap[, state := mapvalues(state, old_states, new_states)]
  
  hold[[j]] <- swap
  
}

df_sim <- rbindlist(hold)

# Add on indicator for fake states:
df_true[, observed_data := T]
df_sim[, observed_data := F]

df_sim <- rbind(df_true, df_sim)

write.csv(df_sim, "./data/df_synthetic_states.csv", row.names = F)

