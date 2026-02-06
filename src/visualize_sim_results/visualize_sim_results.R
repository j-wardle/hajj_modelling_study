orderly2::orderly_strict_mode()

orderly2::orderly_parameters(nsim = NULL,
                             long_run = FALSE)

# Load packages
library(tidyverse)
library(scales)

# For now, we define dependencies manually here based on the parameters in hajj_workflow.R

# Define scenario parameters
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

movement_inputs <- c("moh", "iata")
parameters <- expand_grid(parameters, movement_input = c("moh", "iata", "iata_method2")) 
parameters$sensitivity <-  0.768
parameters$specificity <-  0.9968
parameters$isolation_period <-  10

parameters <- expand_grid(parameters, testing_rate = c(0, 0.8, 0.99, 9999))
parameters <- expand_grid(parameters, seed_country = c("Indonesia", "United Kingdom", "global"))

parameters <- parameters %>% 
  mutate_if(is.numeric, round, digits = 4)

parameters_select <- parameters %>%
  select(pathogen, beta, movement_input, testing_rate, seed_country)

for (row in 1:nrow(parameters_select)) {
  
  testing_rate <- parameters_select$testing_rate[row]
  movement_input <- parameters_select$movement_input[row]
  seed_country <- parameters_select$seed_country[row]
  beta <- parameters_select$beta[row] # beta determines the pathogen scenario, we add pathogen label later
    
  orderly2::orderly_dependency(
    "run_symptoms_model_parallel",
    quote(latest(
      latest(
        parameter:tau == environment:testing_rate &&
          parameter:movement_input  == environment:movement_input &&
          parameter:seed_country  == environment:seed_country &&
          parameter:simulations == this:nsim &&
          parameter:beta == environment:beta &&
          parameter:long_run == this:long_run # TEMP FOR LONG_RUN SIMS
        # parameter:tau == environment:parameters_select$testing_rate[row] &&
        #   parameter:movement_input  == environment:parameters_select$movement_input[row] &&
        #   parameter:pathogen  == environment:parameters_select$pathogen[row] &&
        #   parameter:seed_country  == environment:parameters_select$seed_country[row]
      ))),
    # c("data/${s}.rds" = "scenario_output.rds"))
    # c("data/${row}.rds" = "all_sims_summary.rds"))
    c("data/${row}.rds" = "output_summary.rds"))
}


# orderly2::orderly_artefact("Collated outputs", c("all_sim_outputs.rds",
#                                                  "all_sims_summary.rds"))



scenario_summaries <- purrr::map_dfr(1:nrow(parameters_select), function(s) {
  data <- readRDS(paste0("data/", s, ".rds"))
  
  data$pathogen <- parameters_select$pathogen[s]
  data$testing_rate <- parameters_select$testing_rate[s]
  data$movement_input <- parameters_select$movement_input[s]
  data$seed_country <- parameters_select$seed_country[s]
  
  data
  
}, .id = "scenario")

# scenario_summaries <- scenario_summaries %>%
#   mutate(pathogen = ifelse(beta == 0.761, "flu", "sars-cov"))

saveRDS(scenario_summaries, "scenario_summaries.rds")

# Re-format scenario_summaries for results table ----

scenario_summaries <- scenario_summaries %>%
    group_by(pathogen, testing_rate, movement_input, seed_country) %>%
    summarise(across(where(is.numeric),
                     list(median = ~median(.),
                          quantile_2.5 = ~quantile(., 0.025),
                          quantile_97.5 = ~quantile(., 0.975),
                          quantile_25 = ~quantile(., 0.25),
                          quantile_75 = ~quantile(., 0.75),
                          mean = ~mean(.),
                          sd = ~sd(.)),
                     .names = "{col}-{fn}")) %>%
    pivot_longer(cols = -c(pathogen, testing_rate, movement_input, seed_country),
                 names_to = c("variable", "statistic"),
                 names_sep = "-",
                 values_to = "value") %>%
    pivot_wider(names_from = statistic, values_from = value)

# Round and combine median, quantile_2.5, and quantile_97.5 into a single column

scenario_summaries <- scenario_summaries %>%
  mutate(
    across(c(median, quantile_2.5, quantile_97.5, quantile_25, quantile_75),
           ~ if_else(variable %in% c("ksa_resident_attack_rate", "foreign_pilgrim_attack_rate"),
                     round(., 4),  # Round to 3 decimal places for the specified variables
                     round(., 1))) # Round to 1 decimal place for all other variables
  )

# scenario_summaries$median <- round(scenario_summaries$median, 1)
# scenario_summaries$quantile_2.5 <- round(scenario_summaries$quantile_2.5, 1)
# scenario_summaries$quantile_97.5 <- round(scenario_summaries$quantile_97.5, 1)
# scenario_summaries$quantile_25 <- round(scenario_summaries$quantile_25, 1)
# scenario_summaries$quantile_75 <- round(scenario_summaries$quantile_75, 1)

scenario_summaries <- scenario_summaries %>%
  # mutate(across(where(is.numeric), ~ comma(.))) %>%
  # mutate(value_str = print0(median, " (", quantile_2.5, " , ", quantile_97.5))
  mutate(value_str_95 = ifelse(
    variable %in% c("ksa_resident_attack_rate", "foreign_pilgrim_attack_rate"),
    sprintf("%.3f (%.3f; %.3f)",
            round(median, 4),
            round(quantile_2.5, 4),
            round(quantile_97.5, 4)),
    sprintf("%s (%s; %s)",
            comma(round(median, 1), accuracy = 0.1),
            comma(round(quantile_2.5, 1), accuracy = 0.1),
            comma(round(quantile_97.5, 1), accuracy = 0.1)
  ))) %>%
  mutate(value_str_iqr = ifelse(
    variable %in% c("ksa_resident_attack_rate", "foreign_pilgrim_attack_rate"),
    sprintf("%.3f (%.3f; %.3f)",
            round(median, 4),
            round(quantile_25, 4),
            round(quantile_75, 4)),
    sprintf("%s (%s; %s)",
            comma(round(median, 1), accuracy = 0.1),
            comma(round(quantile_25, 1), accuracy = 0.1),
            comma(round(quantile_75, 1), accuracy = 0.1)
  )))



# Pivot to wide format

for (i in c("Indonesia", "United Kingdom", "global")) {
  for (j in c("moh", "iata", "iata_method2")) {

    seed <- i
    movement <- j

    df_wide <- scenario_summaries %>%
      filter(seed_country == i & movement_input == j & variable != "sim") %>%
      ungroup() %>%
      mutate(testing_rate = ifelse(testing_rate == 9999, "symptomatic", testing_rate)) %>%
      select(variable, pathogen, testing_rate, value_str_95, value_str_iqr) %>%
      pivot_wider(names_from = c(pathogen, testing_rate),
                  values_from = c("value_str_95", "value_str_iqr"),
                  names_sep = "_")

    # Order the df variables as wanted for results table
    df_wide$variable <- factor(df_wide$variable, levels = c(
      "foreign_pilgrim_epidemic_size",
      "domestic_pilgrim_epidemic_size",
      "ksa_nonpilgrim_epidemic_size",
      "ksa_resident_attack_rate",
      "foreign_pilgrim_attack_rate",
      "imported_infections",
      "untested_importations",
      "total_tests",
      "true_positives",
      "false_positives",
      "true_negatives",
      "false_negatives"
    ))

    # Sort the dataframe by this new order
    df_wide <- df_wide[order(df_wide$variable), ]

    # Write the data frame to a CSV file
    filename <- paste0("scenario_summaries_", seed, "_", movement, ".csv")

    write.csv(df_wide, file = filename,
              row.names = FALSE)

  }
}