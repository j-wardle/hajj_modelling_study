# Workflow for project

library(tidyverse)
library(orderly2)
library(hipercow)

# Set up cluster interaction
hipercow_init(driver = "windows")
hipercow_configuration()

# Specify packages (i.e. orderly2) in pkgdepends.txt in hipercow root
hipercow_provision(method = "pkgdepends")


# Ensure the latest development version of multipatchr is installed
# remove.packages('multipatchr')
devtools::install_github("sangeetabhatia03/multipatchr", build = TRUE)

# Note that the following task and subsequent analysis tasks require access to
# IATA datasets - purchased from https://www.iata.org/en/services/data/
orderly2::orderly_run("get_flight_data",
                      parameters = list(folder = "C:/Users/jw2519/Documents/iata_analysis/data/",
                                        analysis_years_start = "2012",
                                        analysis_years_end = "2019"))

# Generate estimates of pilgrim numbers by origin (three methods)
orderly2::orderly_run("moh_pilgrim_numbers")
orderly2::orderly_run("iata_pilgrim_estimates")
orderly2::orderly_run("iata_pilgrim_estimates_method2")

# Compare pilgrim numbers and generate Figure 4
orderly2::orderly_run("compare_pilgrim_inputs")

# Compute probabilities of travelling (used in model code)
orderly2::orderly_run("estimate_flight_probs", list(analysis_years_start = 2012,
                                                    analysis_years_end = 2021))

# Process age-sex distributions for later  estimation of hospitalisations and deaths
orderly2::orderly_run("pilgrim_age_distribution", list(
  age_data_source = "azarpazhooh_et_al"
))

# SIMULATION STUDY
# We ran our simulation study using the Imperial DIDE local cluster, which we
# interact with using the hipercow package.

# Set-up for cluster runs:

# Define scenario parameters ----
sars_cov_parameters <- data.frame(
  pathogen = "sars-cov",
  p = 0.595, # proportion symptomatic
  delta = 1/4.6, # rate of leaving E
  gamma_p = 1/1.7, # rate of I_p -> I_s
  gamma_s = 1/2.1, # rate of I_s -> R
  gamma_a = 1/2.1, # rate of I_a -> R
  theta_a = 0.58, # relative infectivity of I_a (c.f. I_s)
  theta_p = 1, # relative infectivity of I_p (c.f. I_s)
  beta = 0.966
)

flu_parameters <- data.frame(
  pathogen = "flu",
  p = 0.669, # proportion symptomatic
  delta = 1/1.1, # rate of leaving E
  gamma_p = 1/1, # rate of I_p -> I_s
  gamma_s = 1/1.5, # rate of I_s -> R
  gamma_a = 1/2.5, # rate of I_a -> R
  theta_a = 0.58, # relative infectivity of I_a (c.f. I_s))
  theta_p = 1, # relative infectivity of I_p (c.f. I_s)
  beta = 0.761
)

parameters <- bind_rows(sars_cov_parameters, flu_parameters)

parameters <- expand_grid(parameters, movement_input = c("moh", "iata", "iata_method2"))
parameters$sensitivity <-  0.768
parameters$specificity <-  0.9968
parameters$isolation_period <-  10

# NB. testing_rate 9999 denotes the scenario where we use a symptomatic testing strategy
parameters <- expand_grid(parameters, testing_rate = c(0, 0.80, 0.99, 9999))
parameters <- expand_grid(parameters, seed_country = c("Indonesia", "United Kingdom", "global"))

parameters <- parameters %>%
  mutate_if(is.numeric, round, digits = 4)

# Define helper function for submitting orderly task to cluster
cluster_submit_long_run <- function(i, sim_parameters, resources) {
  
  hipercow::task_create_explicit(
    expr = quote({
      orderly2::orderly_run(
        "run_symptoms_model_parallel",
        parameters = list(beta = sim_parameters$beta[[i]],
                          theta_a = sim_parameters$theta_a[[i]],
                          theta_p = sim_parameters$theta_p[[i]],
                          p = sim_parameters$p[[i]],
                          delta = sim_parameters$delta[[i]],
                          gamma_p = sim_parameters$gamma_p[[i]],
                          gamma_s = sim_parameters$gamma_s[[i]],
                          gamma_a = sim_parameters$gamma_a[[i]],
                          tau = sim_parameters$testing_rate[[i]], # testing rate
                          sensitivity = sim_parameters$sensitivity[[i]],
                          specificity = sim_parameters$specificity[[i]],
                          isolation_period = sim_parameters$isolation_period[[i]],
                          simulations = 1000,
                          sim_range_start = 1,
                          movement_input = sim_parameters$movement_input[[i]],
                          seed_country = sim_parameters$seed_country[[i]],
                          long_run = TRUE))
    }),
    export = c("sim_parameters", "i"),
    parallel = hipercow_parallel("future"),
    resources = resources)
  
}

# Set the cluster resources ----
resources <- hipercow_resources(cores = 32)

# Submit simulations to cluster ----
task_ids <- vector("character", nrow(parameters))
for (i in 1:nrow(parameters)) {
  task_ids[i] <- cluster_submit_long_run(i, parameters, resources)
}

# Processing of simulation outputs ----
orderly2::orderly_run("visualize_sim_results",
                      parameters = list(nsim = 1000,
                                        long_run = TRUE))

orderly2::orderly_run("pairwise_sim_comparisons",
                      parameters = list(nsim = 1000,
                                        long_run = TRUE))

# Generation of figures for main text and supplementary material
orderly2::orderly_run("pairwise_comparison_figs",
                      parameters = list(nsim = 1000,
                                        long_run = TRUE))