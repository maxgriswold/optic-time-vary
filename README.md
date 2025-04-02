# Assessing Bias and Precision in State Policy Evaluations

## Introduction

This repository contains code for the article [Assessing Bias and Precision in State Policy Evaluations: A Comparative Analysis of Time-Varying Estimators Using Policy Simulations](https://www.arxiv.org/abs/2503.20882), replicating the paper's simulations, sensitivity tests, and plots. The repository also contains a copy of the [OPTIC package](https://cran.r-project.org/web/packages/optic/index.html), using the version available at the time of the paper's initial publication.

## Overview

In *Assessing Bias and Precision in State Policy Evaluations*, we used simulated time-varying treatment effects in state-level opioid overdose mortality data to compare the performance of commonly-used policy evaluation methods: two-way fixed effects event study, debiased autoregressive model, augmented synthetic control, difference-in-differences with staggered adoption, an event study with heterogeneous treatment, two-stage differences-in-differences, and differences-in-differences imputation. 

This code reproduces the simulations, methods, and plots used in the paper. 
## Dependencies

The project was conducted using R 4.2.2 and Rstudio 2024.12.1. All package dependencies are available by using `renv::restore()` and installing the OPTIC package using `devtools::install()` . To run this code, load the optic.Rproj project and use the scripts in the folder 'code'.

## Repository Structure

- **/code/** contains all scripts used in the article:
  + **00_install_dependencies** installs all packages needed to run subsequent scripts.
  + **01_create_scenarios** creates the policy scenarios used in the simulations
  + **02_synthetic_states** makes a dataset of synthetic states used in a set of simulation runs.
  + **03_run_simulations** sets up each method used in the analysis, simulates policy effects across scenarios, and estimates effects using each method.
  + **04_make_plots** produces figures in the paper.
- **/plots/** contains pdfs of all figures in the article.
- **/data/** contains the raw data file (`overdoses.rd`), a synthetic state dataset, scenario data, and simulations results.
- **/R/** and `optic_1.0.1.tar` contains the version of the `OPTIC` package used within `code`.

## Contact

For questions, please feel free to reach out to griswold[at]rand.org.

## References

```tex
@misc{griswold2025assessingbiasprecisionstate,
      title={Assessing Bias and Precision in State Policy Evaluations: A Comparative Analysis of Time-Varying Estimators Using Policy Simulations}, 
      author={Max Griswold and Beth Ann Griffin and Max Rubinstein and Mincen Liu and Megan Schuler and Elizabeth Stone and Pedro Nascimento de Lima and Bradley D. Stein and Elizabeth A. Stuart},
      year={2025},
      eprint={2503.20882},
      archivePrefix={arXiv},
      primaryClass={stat.ME},
      url={https://arxiv.org/abs/2503.20882}, 
}
```

## License

This repository is released as open-source software under a GPL-3.0 license. See the LICENSE file for more details.
