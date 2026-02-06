orderly2::orderly_strict_mode()

orderly2::orderly_parameters(beta = NULL,
                             theta_a = NULL,
                             theta_p = NULL,
                             p = NULL,
                             delta = NULL,
                             gamma_p = NULL,
                             gamma_s = NULL,
                             gamma_a = NULL,
                             tau = NULL, # testing rate
                             sensitivity = NULL,
                             specificity = NULL,
                             isolation_period = NULL,
                             simulations = NULL,
                             # sim_number = NULL,
                             sim_range_start = NULL,
                             movement_input = NULL,
                             seed_country = NULL,
                             long_run = FALSE)

orderly2::orderly_dependency(
  "estimate_flight_probs",
  "latest",
  # c(pops_for_sim.rds = "pops_for_sim.rds",
  #   probs_for_sim.rds = "probs_for_sim.rds"))
  "pops_for_sim.rds")

if (movement_input == "moh") {
  orderly2::orderly_dependency(
    "moh_pilgrim_numbers",
    "latest",
    c(arrival_numbers_for_sim.rds = "arrival_numbers_for_sim.rds",
      departure_numbers_for_sim.rds = "departure_numbers_for_sim.rds"))
}

if (movement_input == "iata") {
  orderly2::orderly_dependency(
    "iata_pilgrim_estimates",
    "latest",
    c(arrival_numbers_for_sim.rds = "arrival_numbers_for_sim.rds",
      departure_numbers_for_sim.rds = "departure_numbers_for_sim.rds"))
}

if (movement_input == "iata_method2") {
  orderly2::orderly_dependency(
    "iata_pilgrim_estimates_method2",
    "latest",
    c(arrival_numbers_for_sim.rds = "arrival_numbers_for_sim.rds",
      departure_numbers_for_sim.rds = "departure_numbers_for_sim.rds"))
}

orderly2::orderly_artefact("Model output",
                           # c("scenario_output.rds", "output_summary.rds")
                           "output_summary.rds"
)

# Note: check correct version of multipatchr is installed
# See hajj_workflow.R

# Load packages

library(multipatchr)
library(tidyverse)
library(furrr)

# Load various functions used in model running
orderly2::orderly_resource("R/support_functions.R")
source("R/support_functions.R")

# Read in movement and population data
arrival_numbers_for_sim <- readRDS("arrival_numbers_for_sim.rds")
departure_numbers_for_sim <- readRDS("departure_numbers_for_sim.rds")
pops_for_sim <- readRDS("pops_for_sim.rds")

if(!"KSA_AtRisk" %in% colnames(arrival_numbers_for_sim)) warning("Movement data does not include KSA_AtRisk patch")

# Prepare the populations data
populations <- filter(pops_for_sim,
                      year == 2021 &
                        (country_name %in% rownames(arrival_numbers_for_sim) |
                           country_name == "Saudi Arabia")) %>%
  mutate(country_name = ifelse(country_name == "Saudi Arabia", "KSA", country_name))

domestic_pilgrims <- 634379 # Number taken from 2019 domestic pilgrims statistic (https://www.stats.gov.sa/sites/default/files/haj_40_en.pdf)
# This number is used in the hajj_quotas task.

ksa_nonpilgrims_at_risk <- 1411599 + 2385509
# Population of Medina + Population of Mecca
# Sources:
# 1) https://www.citypopulation.de/en/saudiarabia/madina/0301__al_mad%C4%ABnah_al_munuwarah/
# 2) https://www.citypopulation.de/en/saudiarabia/makkah/0201__makkah_al_mukarramah/

# Amend the KSA population so that the ksa_nonpilgrims at risk are not double counted
populations$population[populations$country_name == "KSA"] <- populations$population[populations$country_name == "KSA"] -
  ksa_nonpilgrims_at_risk

populations[nrow(populations) + 1,] <- list("KSA_AtRisk", NA, ksa_nonpilgrims_at_risk)

# Get the index for nonpilgrims at risk
atrisk_index <- grep("KSA_AtRisk", rownames(arrival_numbers_for_sim))

# Get the indices for all the sub-patches within KSA
ksa_index <- grep("^KSA_", rownames(arrival_numbers_for_sim))
ksa_index <- ksa_index[ksa_index != atrisk_index]

# Create df of the patches included in the model
patches_in_model <- left_join(tibble(country_name = rownames(arrival_numbers_for_sim)),
                              populations,
                              by = "country_name") %>%
  select(-year) %>%
  mutate(population = ifelse(is.na(population), 0, population)) 

empty_patches <- patches_in_model %>%  
  filter(!(str_starts(country_name, "KSA_")) & population == 0) %>% 
  pull(country_name)

empty_patches <- c(empty_patches, paste0("KSA_", empty_patches))

# Remove the empty patches from patches_in_model and movement matrices
patches_in_model <- patches_in_model %>% 
  filter(!(country_name %in% empty_patches))

arrival_numbers_for_sim <- arrival_numbers_for_sim[
  !rownames(arrival_numbers_for_sim) %in% empty_patches,
  !colnames(arrival_numbers_for_sim) %in% empty_patches]

departure_numbers_for_sim <- departure_numbers_for_sim[
  !rownames(departure_numbers_for_sim) %in% empty_patches,
  !colnames(departure_numbers_for_sim) %in% empty_patches]

# *****
# Here we set up some of the parameters for the model
# *****

if (seed_country != "global") {
  # Choose the seed location of initial infections
  seed_location <- seed_country
  
  # Get index of the seed country so that we can apply init_inf to the correct country
  seed_index <- which(patches_in_model[,"country_name"] == seed_location)
} else {
  # Set the seed locations for initial infections
  ## Select the three most populated countries in each WHO region
  seed_location <- c(
    "Nigeria", "Ethiopia", "Democratic Republic of the Congo",  # African Region
    "United States", "Brazil", "Mexico",                       # Region of the Americas
    "Egypt", "Pakistan", "Iran",                               # Eastern Mediterranean Region
    "Russian Federation", "Germany", "Turkey",                 # European Region
    "India", "Indonesia", "Bangladesh",                        # South-East Asia Region
    "China", "Philippines", "Vietnam"                          # Western Pacific Region
  )
  
  # Get index of the seed country so that we can apply init_inf to the correct country
  seed_index <- which(patches_in_model$country_name %in% seed_location)
}

# Set the number of initial infections in the seed location
init_inf <- 1000


# Create a list of movement matrices for the different phases of the model run
# The three phases are:
# 1) Importation period (where pilgrims travel to KSA)
# 2) Period with no movement between countries (I assume this is the Hajj)
# 3) Exportation period (pilgrims return to their home country)
no_movement_phase <- matrix(0,
                            nrow = nrow(arrival_numbers_for_sim),
                            ncol = ncol(arrival_numbers_for_sim))
rownames(no_movement_phase) <- rownames(arrival_numbers_for_sim)
colnames(no_movement_phase) <- colnames(arrival_numbers_for_sim)

if (long_run == FALSE) {
  movement_phases <- list(arrival_numbers_for_sim,
                          no_movement_phase,
                          departure_numbers_for_sim)
  
  # Define the length of the different model periods
  lengths_of_phases <- c(30, 5, 30)
} else {
  movement_phases <- list(arrival_numbers_for_sim,
                          no_movement_phase,
                          departure_numbers_for_sim,
                          no_movement_phase)
  
  # Define the length of the different model periods
  lengths_of_phases <- c(30, 5, 30, 35)
}

# Set up a base initial state ----
# This uses the parameters specified in the orderly_run function call

base_model_parameters <- list(birth_rates = 0,
                              death_rates = 0,
                              transmission_rates = beta,
                              transmission_rates_pilgrims = beta * 1.3,
                              transmission_rates_pilgrims_and_atrisk = beta,
                              asymptomatic_infectiousness = theta_a,
                              presymptomatic_infectiousness = theta_p,
                              prop_symptomatic = p,
                              infection_rates = delta,
                              symptom_rates = gamma_p,
                              recovery_rates_asym = gamma_a,
                              recovery_rates_sym = gamma_s,
                              testing_rate = tau,
                              test_sensitivity = sensitivity,
                              test_specificity = specificity,
                              isolation_period = isolation_period)

if (seed_country != "global") {
  initial_susceptible <- patches_in_model$population
  initial_exposed <- c(rep(0, times = seed_index - 1),
                       init_inf,
                       rep(0, times = nrow(patches_in_model) - seed_index))
} else {
  
  prevalence <- 0.00001 # 0.001%
  populations_in_seed <- pull(patches_in_model[seed_index, "population"])
  infections_in_seed <- round(prevalence * populations_in_seed)
  
  initial_exposed <- rep(0, times = nrow(patches_in_model))
  initial_exposed[seed_index] <- infections_in_seed
  
  initial_susceptible <- patches_in_model$population - initial_exposed
}

base_initial_states <- list(
  s_patches = initial_susceptible,
  e_patches = initial_exposed,
  i_a_patches = 0,
  i_p_patches = 0,
  i_s_patches = 0,
  r_patches = 0,
  diag_patches = 0,
  s_false_patches = 0,
  r_false_patches = 0)

base_starting_state <- c(base_initial_states, base_model_parameters)

# Pre-compute probabilities of ending isolation in each disease state ----
# There is one set of probabilities per possible starting state

isolation_probs_initial_state <- list(s_patches = 0,
                                      e_patches = 0,
                                      i_a_patches = 0,
                                      i_p_patches = 0,
                                      i_s_patches = 0,
                                      r_patches = 0,
                                      diag_patches = 0,
                                      s_false_patches = 0,
                                      r_false_patches = 0)

dummy_movement_matrix <- list(movement_rate = matrix(1))

probs_starting_state <- c(isolation_probs_initial_state,
                          base_model_parameters,
                          dummy_movement_matrix)

end_of_isolation_probabilities <- multipatchr:::get_end_of_isolation_probabilities(
  probs_starting_state, isolation_period, sample_size = 1000000
)

## Run simulation scenario ----

# simulation_scenario <- run_patch_model_screening_vary_movement2(movement_phases,
#                                                               lengths_of_phases,
#                                                               sims = simulations,
#                                                               base_starting_state)

# Test the new functions for symptom-based screening
set.seed(42)

if (tau == 9999) {
  # Run the testing strategy where we screen 99% of symptomatic ppl, and 1% of all others
  simulation_scenario <- 
    furrr::future_map(1:simulations, function(x) {
      
      run_patch_model_symptom_screening_vary_movement(
        movement_phases,
        lengths_of_phases,
        sims = 1,
        base_starting_state)
    },
    .options = furrr_options(seed = TRUE))
  
  code_check <- data.frame("tau" = 9999, "check" = "yes")
  saveRDS(code_check, "code_check.rds")
} else {
  # Run the testing strategy where we screen all pilgrims with rate tau
  simulation_scenario <- 
    furrr::future_map(1:simulations, function(x) {
      
      # runif(1)
      run_patch_model_screening_vary_movement2(
        movement_phases,
        lengths_of_phases,
        sims = 1,
        base_starting_state)
    },
    .options = furrr_options(seed = TRUE))
}

#Temp save the simulation_scenario for any bug fixing
# saveRDS(simulation_scenario, "furrr_output.rds")

# scenario_output <- format_model_output(simulation_scenario) #original command

# scenario_output <- purrr::map_dfr(simulation_scenario, function(s) {
#   format_model_output(s) %>%
#     select(-sim)
# }, .id = "sim")

# scenario_output <- list_rbind(simulation_scenario, names_to = "sim")

# 
# #### Not saving sim output for now as file outputs are getting v large...
# saveRDS(scenario_output, "scenario_output.rds")
# 
# # Apply summary functions to each simulation output ----
# 
# # Create a data frame with the epidemic sizes for different cohorts

# results <- tibble(sim = NA, foreign_pilgrim_epidemic_size = NA,
#                   domestic_pilgrim_epidemic_size = NA, ksa_nonpilgrim_epidemic_size = NA)

results <- data.frame(
  sim = integer(simulations),
  foreign_pilgrim_epidemic_size = numeric(simulations),
  domestic_pilgrim_epidemic_size = numeric(simulations),
  ksa_nonpilgrim_epidemic_size = numeric(simulations)
)

results <- imap_dfr(simulation_scenario, function(sim_result, sim_number) {
  
  true_positives <- get_true_positive_results(sim_result)
  false_positives <- get_false_positive_results(sim_result)
  true_negatives <- get_true_negative_results(sim_result)
  false_negatives <- get_false_negative_results(sim_result)
  total_imported_infected <- get_imported_infections(sim_result)
  
  tibble(
    sim = sim_number,
    foreign_pilgrim_epidemic_size = get_foreign_pilgrim_epidemic_size(sim_result),
    domestic_pilgrim_epidemic_size = get_domestic_pilgrim_epidemic_size(sim_result),
    ksa_nonpilgrim_epidemic_size = get_ksa_nonpilgrim_epidemic_size(sim_result),
    true_positives = true_positives,
    false_positives = false_positives,
    true_negatives = true_negatives,
    false_negatives = false_negatives,
    total_tests = true_positives + false_positives + true_negatives + false_negatives,
    imported_infections = total_imported_infected,
    untested_importations = total_imported_infected - (true_positives + false_negatives)
  )
})

# results <- tibble(sim = NA, foreign_pilgrim_epidemic_size = NA,
#                   domestic_pilgrim_epidemic_size = NA, ksa_nonpilgrim_epidemic_size = NA)
# 
# for (i in 1:simulations) {
#   
#   sim_result <- filter(scenario_output, sim == i)
#   
#   results[i, "sim"] <- i
#   results[i, "foreign_pilgrim_epidemic_size"] <- get_foreign_pilgrim_epidemic_size(sim_result)
#   results[i, "domestic_pilgrim_epidemic_size"] <- get_domestic_pilgrim_epidemic_size(sim_result)
#   results[i, "ksa_nonpilgrim_epidemic_size"] <- get_ksa_nonpilgrim_epidemic_size(sim_result)
#   
# }

# results <- tibble(
#   sim = sim_number,
#   foreign_pilgrim_epidemic_size = get_foreign_pilgrim_epidemic_size(scenario_output),
#   domestic_pilgrim_epidemic_size = get_domestic_pilgrim_epidemic_size(scenario_output),
#   ksa_nonpilgrim_epidemic_size = get_ksa_nonpilgrim_epidemic_size(scenario_output)
# )
# 

# Summarise the attack rate for KSA citizens
# (combination of pilgrims and nonpilgrims)
model_compartments <- c("susceptible", "exposed", "infected_asymptomatic",
                        "infected_presymptomatic", "infected_symptomatic",
                        "recovered")

# ksa_population <- scenario_output %>%
#   filter(sim == 1 & time == max(time) & patch %in% c("KSA", "KSA_AtRisk") &
#            variable %in% model_compartments) %>%
#   summarize(sum_value = sum(value, na.rm = TRUE)) %>%
#   pull(sum_value)

ksa_population <- simulation_scenario[[1]] %>%
  filter(time == max(time) & patch %in% c("KSA", "KSA_AtRisk") &
           variable %in% model_compartments) %>%
  summarize(sum_value = sum(value, na.rm = TRUE)) %>%
  pull(sum_value)


results$ksa_resident_attack_rate <- (results$domestic_pilgrim_epidemic_size +
                                       results$ksa_nonpilgrim_epidemic_size) /
  ksa_population

# Foreign pilgrim attack rate
# Population is all daily pilgrims arriving from outside KSA, multiplied by the arrival period (30 days)
foreign_pilgrim_population <- sum(arrival_numbers_for_sim[rownames(arrival_numbers_for_sim)!="KSA",])*30

results$foreign_pilgrim_attack_rate <- results$foreign_pilgrim_epidemic_size /
  foreign_pilgrim_population

# # Add testing outputs to the results summaries
# 
# for (i in 1:simulations) {
#   
#   sim_result <- filter(scenario_output, sim == i)
#   
#   results[i, "true_positives"] <- get_true_positive_results(sim_result)
#   results[i, "false_positives"] <- get_false_positive_results(sim_result)
#   results[i, "true_negatives"] <- get_true_negative_results(sim_result)
#   results[i, "false_negatives"] <- get_false_negative_results(sim_result)
#   results[i, "total_tests"] <- results$true_positives[i] + results$false_positives[i] +
#     results$true_negatives[i] + results$false_negatives[i]
#   
#   # Total tests of infected pilgrims
#   
#   total_tests_infected <- results$true_positives[i] + results$false_negatives[i]
#   total_imported_infected <- get_imported_infections(sim_result)
#   
#   results[i, "imported_infections"] <- total_imported_infected
#   results[i, "untested_importations"] <- total_imported_infected -
#     total_tests_infected
#   
# }


# results$true_positives <- get_true_positive_results(scenario_output)
# results$false_positives <- get_false_positive_results(scenario_output)
# results$true_negatives <- get_true_negative_results(scenario_output)
# results$false_negatives <- get_false_negative_results(scenario_output)
# results$total_tests <- results$true_positives + results$false_positives +
#   results$true_negatives + results$false_negatives
# 
# 
# # Total tests of infected pilgrims
# 
# total_tests_infected <- results$true_positives + results$false_negatives
# total_imported_infected <- get_imported_infections(scenario_output)
# 
# results$imported_infections <- total_imported_infected
# results$untested_importations <- total_imported_infected -
#   total_tests_infected

saveRDS(results, "output_summary.rds")