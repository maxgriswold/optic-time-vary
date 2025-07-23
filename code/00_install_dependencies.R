rm(list = ls())

install.packages(c("devtools", "renv"))

renv::restore()

# Build the OPTIC package
devtools::install("./")
