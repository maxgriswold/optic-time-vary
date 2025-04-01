rm(list = ls())

install.packages(c("devtools", "renv"))

# Build the OPTIC package
devtools::install("./")

renv::restore()
