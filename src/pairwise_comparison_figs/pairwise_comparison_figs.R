# setwd("C:/Users/jw2519/OneDrive - Imperial College London/Hajj project/Flight model/hajj_project/src/estimate_deaths")

orderly2::orderly_strict_mode()

orderly2::orderly_parameters(nsim = NULL,
                             long_run = NULL)

# Define common figure functions
orderly2::orderly_shared_resource("utils.R")
source("utils.R")

# Load various functions used in task
orderly2::orderly_resource("R/utils.R")
source("R/utils.R")

# Set dependencies
# orderly2::orderly_dependency(
#   "visualize_sim_results",
#   "latest(parameter:nsim == this:nsim)",
#   c(scenario_summaries.rds = "scenario_summaries.rds"))

orderly2::orderly_dependency(
  "pairwise_sim_comparisons",
  # "latest(parameter:nsim == this:nsim)",
  quote(latest(
    # latest(
      parameter:nsim == this:nsim &&
        parameter:long_run == this:long_run)),
    # parameter:nsim == 1000 &&
    #   parameter:long_run == TRUE)),
  c(
    # First combination of age inputs
    ages1pairwise_outcomes_averted.rds = "ages1pairwise_outcomes_averted.rds",
    ages1pairwise_outcomes_averted_qntls.rds = "ages1pairwise_outcomes_averted_qntls.rds",
    ages1ranked_outcomes_averted.rds = "ages1ranked_outcomes_averted.rds",
    ages1ranked_outcomes_averted_qntls.rds = "ages1ranked_outcomes_averted_qntls.rds",
    ages1random_pairwise_outcomes_averted.rds = "ages1random_pairwise_outcomes_averted.rds",
    ages1random_pairwise_averted_qntls.rds = "ages1random_pairwise_averted_qntls.rds",
    # Second combination of age inputs
    ages2pairwise_outcomes_averted.rds = "ages2pairwise_outcomes_averted.rds",
    ages2pairwise_outcomes_averted_qntls.rds = "ages2pairwise_outcomes_averted_qntls.rds",
    ages2ranked_outcomes_averted.rds = "ages2ranked_outcomes_averted.rds",
    ages2ranked_outcomes_averted_qntls.rds = "ages2ranked_outcomes_averted_qntls.rds",
    ages2random_pairwise_outcomes_averted.rds = "ages2random_pairwise_outcomes_averted.rds",
    ages2random_pairwise_averted_qntls.rds = "ages2random_pairwise_averted_qntls.rds",
    # Full simulation data summaries
    scenario_summaries.rds = "ages1_full_summary.rds",
    scenario_summaries2.rds = "ages2_full_summary.rds"
    ))

# Load packages
library(tidyverse)
library(scales)
library(gt)
library(webshot2)
library(ggh4x)

# Read in results files
ages1pairwise_outcomes_averted_qntls <- readRDS("ages1pairwise_outcomes_averted_qntls.rds")
ages1random_pairwise_averted_qntls <- readRDS("ages1random_pairwise_averted_qntls.rds")
ages1ranked_outcomes_averted_qntls <- readRDS("ages1ranked_outcomes_averted_qntls.rds")
ages1pairwise_outcomes_averted <- readRDS("ages1pairwise_outcomes_averted.rds")
ages1random_pairwise_outcomes_averted <- readRDS("ages1random_pairwise_outcomes_averted.rds")
ages1ranked_outcomes_averted <- readRDS("ages1ranked_outcomes_averted.rds")

ages2pairwise_outcomes_averted_qntls <- readRDS("ages2pairwise_outcomes_averted_qntls.rds")
ages2random_pairwise_averted_qntls <- readRDS("ages2random_pairwise_averted_qntls.rds")
ages2ranked_outcomes_averted_qntls <- readRDS("ages2ranked_outcomes_averted_qntls.rds")
ages2pairwise_outcomes_averted <- readRDS("ages2pairwise_outcomes_averted.rds")
ages2random_pairwise_outcomes_averted <- readRDS("ages2random_pairwise_outcomes_averted.rds")
ages2ranked_outcomes_averted <- readRDS("ages2ranked_outcomes_averted.rds")

scenario_summaries <- readRDS("scenario_summaries.rds")
scenario_summaries2 <- readRDS("scenario_summaries2.rds")

### Note that in the main text we report results from the ages1 files with ranked_outcomes 

# Label the seeding methods
ages1pairwise_outcomes_averted_qntls$pairing <- "matched_seed"
ages1random_pairwise_averted_qntls$pairing <- "random"
ages1ranked_outcomes_averted_qntls$pairing <- "ranked"
ages1pairwise_outcomes_averted$pairing <- "matched_seed"
ages1random_pairwise_outcomes_averted$pairing <- "random"
ages1ranked_outcomes_averted$pairing <- "ranked"

ages2pairwise_outcomes_averted_qntls$pairing <- "matched_seed"
ages2random_pairwise_averted_qntls$pairing <- "random"
ages2ranked_outcomes_averted_qntls$pairing <- "ranked"
ages2pairwise_outcomes_averted$pairing <- "matched_seed"
ages2random_pairwise_outcomes_averted$pairing <- "random"
ages2ranked_outcomes_averted$pairing <- "ranked"

# Add an age label
ages1pairwise_outcomes_averted_qntls$age_input <- "ages1"
ages1random_pairwise_averted_qntls$age_input <- "ages1"
ages1ranked_outcomes_averted_qntls$age_input <- "ages1"
ages1pairwise_outcomes_averted$age_input <- "ages1"
ages1random_pairwise_outcomes_averted$age_input <- "ages1"
ages1ranked_outcomes_averted$age_input <- "ages1"

ages2pairwise_outcomes_averted_qntls$age_input <- "ages2"
ages2random_pairwise_averted_qntls$age_input <- "ages2"
ages2ranked_outcomes_averted_qntls$age_input <- "ages2"
ages2pairwise_outcomes_averted$age_input <- "ages2"
ages2random_pairwise_outcomes_averted$age_input <- "ages2"
ages2ranked_outcomes_averted$age_input <- "ages2"

scenario_summaries$age_input <- "ages1"
scenario_summaries2$age_input <- "ages2"

# Tabulate the results ----

table_function <- function(seed_pairing_results,
                           random_pairing_results,
                           ranked_pairing_results) {
  
  qntl_summaries <- list(
    seed_pairing_results,
    random_pairing_results,
    ranked_pairing_results
  )
  
  # qntl_summaries_df <- bind_rows(qntl_summaries, .id = "pairing")
  qntl_summaries_df <- bind_rows(qntl_summaries)
  
  qntl_summaries_df <- qntl_summaries_df %>% 
    filter(variable == "yll_ksa_total_diff")
  
  # Assuming qntl_summaries_df is your dataframe, create the gt table
  qntl_table <- qntl_summaries_df %>%
    group_by(pathogen, movement_input, seed_country) %>% 
    # Select the relevant columns
    select(
      pairing, testing_rate, median, quantile_2.5, quantile_97.5, mean, sd
    ) %>%
    arrange(testing_rate) %>% 
    
    # Create the gt table
    gt() %>%
    
    # Rename the columns for display
    cols_label(
      pairing = "Pairing",
      testing_rate = "Testing rate",
      median = "Median",
      quantile_2.5 = "Quantile 2.5",
      quantile_97.5 = "Quantile 97.5",
      mean = "Mean",
      sd = "SD"
    ) %>%
    
    # Add a title and subtitle
    tab_header(
      title = "Summary of YLL averted",
      subtitle = "Comparison of simulation pairing methods"
    ) %>%
    
    # Format numeric columns (optional)
    fmt_number(
      columns = c(median, quantile_2.5, quantile_97.5, mean, sd),
      decimals = 2
    ) %>%
    
    # Customize the table's appearance (optional)
    tab_options(
      table.font.size = 12,
      heading.align = "center"
    )
  
}

ages1_table <- table_function(ages1pairwise_outcomes_averted_qntls,
                              ages1random_pairwise_averted_qntls,
                              ages1ranked_outcomes_averted_qntls)

ages2_table <- table_function(ages2pairwise_outcomes_averted_qntls,
                              ages2random_pairwise_averted_qntls,
                              ages2ranked_outcomes_averted_qntls)

gtsave(ages1_table, "ages1_table.tex")
gtsave(ages2_table, "ages2_table.tex")

gtsave(ages1_table, "ages1_table.html")
gtsave(ages2_table, "ages2_table.html")


# Visualize results ----

yll_pairwise_comparison_figs <- function(pairing_quantile_data,
                                         seed_location) {
  
  pairing_quantile_data$pathogen <- recode(
    pairing_quantile_data$pathogen,
    "flu" = "Influenza X",
    "sars-cov" = "SARS-CoV-X")
  
  pairing_quantile_data$testing_rate <- factor(
    pairing_quantile_data$testing_rate,
    levels = c("0", "0.8", "0.99", "9999"),
    labels = c("0%", "80%", "99%", "Syndromic"))
  
  pairing_quantile_data$movement_input <- factor(
    pairing_quantile_data$movement_input,
    levels = c("moh", "iata", "iata_method2"),
    labels = c("MOH",
               "IATA w/ baseline adjustment",
               "IATA w/ no baseline adjustment"))
  
  pairing_quantile_data$seed_country <- recode(
    pairing_quantile_data$seed_country,
    "global" = "Global")
  
  yll_data <- pairing_quantile_data %>%
    filter(variable == "yll_ksa_total_diff")
  
  p <-
    ggplot(yll_data) +
    geom_linerange(aes(x = pathogen,
                       ymin = quantile_2.5, ymax = quantile_97.5,
                       fill = testing_rate),
                   alpha=0.9,
                   position = position_dodge(width = 0.5)) +
    geom_crossbar(aes(x = pathogen, y = median,
                      ymin = quantile_25, ymax = quantile_75,
                      fill = testing_rate),
                  position = position_dodge(width = 0.5),
                  width = 0.3) +
    # facet_grid(seed_country~movement_input) +
    facet_grid(seed_country~movement_input, scales = "free") + #long_run edit
    scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
    labs(#title = paste0("Seed location: ", unique(yll_data$seed_country)),
      x = "Pathogen scenario",
      y = "YLL averted\n(over 100 day simulation period)",
      fill = "Pilgrim testing\nstrategy") +
    scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby")
  
  ggsave(paste0(unique(yll_data$age_input),
                "_",
                unique(yll_data$pairing),
                "_yll_averted.png"),
         p,
         width = 8, height = 6, bg = "white")
  
}

qntl_dfs <- list(ages1pairwise_outcomes_averted_qntls,
                 ages1random_pairwise_averted_qntls,
                 ages1ranked_outcomes_averted_qntls,
                 ages2pairwise_outcomes_averted_qntls,
                 ages2random_pairwise_averted_qntls,
                 ages2ranked_outcomes_averted_qntls)

map(qntl_dfs, function(x) {
  
  yll_pairwise_comparison_figs(x)
  
})


### ICER cloud figure

# for dev: use ages1ranked_outcomes_averted.rds
# set that fail to be pairing_data
# seed_location = "indonesia", plot_pathogen = "SARS-CoV-X"
# pairing_data <- ages1ranked_outcomes_averted
# plot_pathogen <- "SARS-CoV-X"

icer_cloud_figs <- function(pairing_data,
                            plot_pathogen) {
  
  pairing_data$pathogen <- recode(
    pairing_data$pathogen,
    "flu" = "Influenza X",
    "sars-cov" = "SARS-CoV-X")
  
  pairing_data$testing_rate <- factor(
    pairing_data$testing_rate,
    levels = c("0", "0.8", "0.99", "9999"),
    labels = c("0%", "80%", "99%", "Symptomatic"))
  
  pairing_data$movement_input <- factor(
    pairing_data$movement_input,
    levels = c("moh", "iata", "iata_method2"),
    labels = c("MOH",
               "IATA w/ baseline adjustment",
               "IATA w/ no baseline adjustment"))
  
  pairing_data$seed_country <- recode(
    pairing_data$seed_country,
    "global" = "Global")
  
  icer_data <- pairing_data %>%
    filter(pathogen == plot_pathogen)
  
  p <-
    ggplot(icer_data) +
    geom_point(aes(x = yll_ksa_total_diff, y = total_costs_diff,
                   colour = testing_rate),
               # alpha = 0.5,
               size = 0.3) +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = 0)) + 
    # facet_grid(testing_rate~movement_input) +
      facet_grid(seed_country~movement_input) +
    
    labs(title = unique(icer_data$pathogen),
      x = "YLLs averted",
      y = "Cost difference (2023 international $)",
      colour = "Pilgrim testing\nstrategy") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)#,
          # legend.position = "none"
          ) +
    paletteer::scale_colour_paletteer_d("ggthemes::wsj_rgby")
  
  ggsave(paste0(unique(icer_data$age_input),
                "_", unique(icer_data$pairing),
                "_", unique(icer_data$pathogen),
                "_icer_cloud.png"),
         p,
         width = 8, height = 6, bg = "white")
  
}

averted_dfs <- list(ages1pairwise_outcomes_averted,
                    ages1random_pairwise_outcomes_averted,
                    ages1ranked_outcomes_averted,
                    ages2pairwise_outcomes_averted,
                    ages2random_pairwise_outcomes_averted,
                    ages2ranked_outcomes_averted)

map(averted_dfs, function(x) {
  
  icer_cloud_figs(x, "SARS-CoV-X")
  icer_cloud_figs(x, "Influenza X")
  icer_cloud_figs(x, "SARS-CoV-X")
  icer_cloud_figs(x, "Influenza X")
  icer_cloud_figs(x, "SARS-CoV-X")
  icer_cloud_figs(x, "Influenza X")
  
})


# ICER summary figure
# Compute ICER values and summarise

icer_summary_figs <- function(pairing_data,
                              fixed_variable,
                              fixed_variable_value
                              # seed_location
                              ) {
  
  pairing_data$pathogen <- recode(
    pairing_data$pathogen,
    "flu" = "Influenza X",
    "sars-cov" = "SARS-CoV-X")
  
  pairing_data$testing_rate <- factor(
    pairing_data$testing_rate,
    levels = c("0", "0.8", "0.99", "9999"),
    labels = c("0%", "80%", "99%", "Syndromic"))
  
  pairing_data$movement_input <- factor(
    pairing_data$movement_input,
    levels = c("moh", "iata", "iata_method2"),
    labels = c("MOH",
               "IATA w/ baseline adjustment",
               "IATA w/ no baseline adjustment"))
  
  pairing_data$seed_country <- recode(
    pairing_data$seed_country,
    "global" = "Global")
  
  
  if(fixed_variable == "seed_country") {
    icer_data <- pairing_data %>%
      filter(seed_country == fixed_variable_value)
  } else {
    icer_data <- pairing_data %>%
      filter(movement_input == fixed_variable_value)
  }
  
  icer_data$icer_value <- icer_data$total_costs_diff / icer_data$yll_ksa_total_diff  
  
  icer_data_qntls <- averted_outcomes_qntls(icer_data) %>% 
    filter(variable == "icer_value")
  
  wrapping_var <- ifelse(fixed_variable == "seed_country",
                         "movement_input",
                         "seed_country")
  
  fixed_part_of_filenames <- list(
    "MOH" = "MOH",
    "IATA w/ baseline adjustment" = "iata_with_bl_adjust",
    "IATA w/ no baseline adjustment" = "iata_no_bl_adjust",
    "Indonesia" = "Indonesia",
    "United Kingdom" = "United Kingdom",
    "Global" = "Global"
  )
  
  p <-
    ggplot(icer_data_qntls) +
    # For now, just plotting IQR to get things on more easily comparable scales
    # geom_linerange(aes(x = pathogen,
    #                    ymin = quantile_2.5, ymax = quantile_97.5,
    #                    fill = testing_rate),
    #                alpha=0.9,
    #                position = position_dodge(width = 0.5)) +
    geom_crossbar(aes(x = pathogen, y = median,
                      ymin = quantile_25, ymax = quantile_75,
                      fill = testing_rate),
                  position = position_dodge(width = 0.5),
                  width = 0.3) +
    # facet_wrap(~movement_input, nrow = 1) +
      facet_wrap(~get(wrapping_var), nrow = 1) +
      # facet_wrap(~seed_country, nrow = 1, scales = "free") +
    # ggh4x::facet_grid2(seed_country~movement_input, scales = "free_y", independent = "y") +
    
    labs(#title = paste0("Seed location: ", unique(icer_data$seed_country)),
         x = "Pathogen scenario",
         y = "ICER",
         fill = "Pilgrim testing\nstrategy") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby")
  
  ggsave(paste0(unique(icer_data$age_input), "_",
                unique(icer_data$pairing), "_",
                fixed_part_of_filenames[[fixed_variable_value]], "_icer_summary.png"),
         p,
         width = 8, height = 6, bg = "white")
  
}

imap(averted_dfs, function(x, i) {
  print(i)
  icer_summary_figs(x, "seed_country", "Indonesia")
  icer_summary_figs(x, "seed_country", "United Kingdom")
  icer_summary_figs(x, "seed_country", "Global")
  icer_summary_figs(x, "movement_input", "MOH")
  icer_summary_figs(x, "movement_input", "IATA w/ baseline adjustment")
  # if (long_run != T) icer_summary_figs(x, "movement_input", "IATA w/ no baseline adjustment")
  icer_summary_figs(x, "movement_input", "IATA w/ no baseline adjustment")
  
})

# ICER table

icer_table_function <- function(seed_pairing_results,
                                random_pairing_results,
                                ranked_pairing_results) {
  
  seed_pairing_results$icer_value <- seed_pairing_results$total_costs_diff / seed_pairing_results$yll_ksa_total_diff  
  seed_pairing_qntls <- averted_outcomes_qntls(seed_pairing_results) %>% 
    filter(variable == "icer_value")
  seed_pairing_qntls$pairing <- unique(seed_pairing_results$pairing)
  
  random_pairing_results$icer_value <- random_pairing_results$total_costs_diff / random_pairing_results$yll_ksa_total_diff  
  random_pairing_qntls <- averted_outcomes_qntls(random_pairing_results) %>% 
    filter(variable == "icer_value")
  random_pairing_qntls$pairing <- unique(random_pairing_results$pairing)
  
  ranked_pairing_results$icer_value <- ranked_pairing_results$total_costs_diff / ranked_pairing_results$yll_ksa_total_diff  
  ranked_pairing_qntls <- averted_outcomes_qntls(ranked_pairing_results) %>% 
    filter(variable == "icer_value")
  ranked_pairing_qntls$pairing <- unique(ranked_pairing_results$pairing)
  
  results_list <- list(
    seed_pairing_qntls,
    random_pairing_qntls,
    ranked_pairing_qntls
  )
  
  results_df <- bind_rows(results_list)
  
  results_df <- results_df %>% 
    filter(variable == "icer_value")
  
  # Assuming qntl_summaries_df is your dataframe, create the gt table
  qntl_table <- results_df %>%
    group_by(pathogen, movement_input, seed_country) %>% 
    # Select the relevant columns
    select(
      pairing, testing_rate, median, quantile_2.5, quantile_97.5, mean, sd
    ) %>%
    arrange(testing_rate) %>% 
    
    # Create the gt table
    gt() %>%
    
    # Rename the columns for display
    cols_label(
      pairing = "Pairing",
      testing_rate = "Testing rate",
      median = "Median",
      quantile_2.5 = "Quantile 2.5",
      quantile_97.5 = "Quantile 97.5",
      mean = "Mean",
      sd = "SD"
    ) %>%
    
    # Add a title and subtitle
    tab_header(
      title = "Summary of ICERs",
      subtitle = "Comparison of simulation pairing methods"
    ) %>%
    
    # Format numeric columns (optional)
    fmt_number(
      columns = c(median, quantile_2.5, quantile_97.5, mean, sd),
      decimals = 2
    ) %>%
    
    # Customize the table's appearance (optional)
    tab_options(
      table.font.size = 12,
      heading.align = "center"
    )
  
}

ages1_icer_table <- icer_table_function(ages1pairwise_outcomes_averted,
                                        ages1random_pairwise_outcomes_averted,
                                        ages1ranked_outcomes_averted)

ages2_icer_table <- icer_table_function(ages2pairwise_outcomes_averted,
                                        ages2random_pairwise_outcomes_averted,
                                        ages2ranked_outcomes_averted)

gtsave(ages1_table, "ages1_icer_table.tex")
gtsave(ages2_table, "ages2_icer_table.tex")

gtsave(ages1_table, "ages1_icer_table.html")
gtsave(ages2_table, "ages2_icer_table.html")



# NMB calculations ----

# First get the willingness to pay thresholds for KSA

# WTP thresholds for KSA from: https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(23)00162-6/fulltext
# (see Suppl. Mat. Table 2: Cost-effectiveness threshold per Life Year in US dollars (2019))
ksa_wtp <- 9257
ksa_wtp_lo <- 7879
ksa_wtp_hi <- 11593

usd_to_sar <- 3.75 #https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=SA
ksa_cpi_2023 <- 132.4 #https://data.worldbank.org/indicator/FP.CPI.TOTL?end=2023&locations=SA%29-SA)-SA&start=2000&view=chart
ksa_cpi_2019 <- 118.4 
sar_per_int_dol_2023 <- 1.91 #https://data.worldbank.org/indicator/PA.NUS.PRVT.PP?locations=SA)

convert_cost_to_int_dollars <- function(intervention_cost,
                                        sar_exchange_rate,
                                        ksa_cpi_2023, ksa_cpi_year1,
                                        sar_per_int_dol_2023) {
  
  # Step i: Convert intervention cost to SAR using historical exchange rate
  testing_cost_sar_year1 <- intervention_cost * sar_exchange_rate
  
  # Step ii: Adjust for inflation using KSA CPI
  testing_cost_sar_2023 <- testing_cost_sar_year1 * (ksa_cpi_2023 / ksa_cpi_year1)
  
  # Step iii: Convert 2023 SAR to international dollars
  testing_cost_int_dol_2023 <- testing_cost_sar_2023 / sar_per_int_dol_2023  
  
}

ksa_wtp_int_dol <- convert_cost_to_int_dollars(ksa_wtp, usd_to_sar,
                                               ksa_cpi_2023, ksa_cpi_2019,
                                               sar_per_int_dol_2023)
names(ksa_wtp_int_dol) <- "wtp_central"

ksa_wtp_lo_int_dol <- convert_cost_to_int_dollars(ksa_wtp_lo, usd_to_sar,
                                                  ksa_cpi_2023, ksa_cpi_2019,
                                                  sar_per_int_dol_2023)
names(ksa_wtp_lo_int_dol) <- "wtp_lo"

ksa_wtp_hi_int_dol <- convert_cost_to_int_dollars(ksa_wtp_hi, usd_to_sar,
                                                  ksa_cpi_2023, ksa_cpi_2019,
                                                  sar_per_int_dol_2023)
names(ksa_wtp_hi_int_dol) <- "wtp_hi"


pairing_data <- ages1ranked_outcomes_averted

fixed_variable <- "seed_country"
fixed_variable_value <- "Indonesia"
wtp_threshold <- ksa_wtp_int_dol

nmb_summary_figs <- function(pairing_data,
                             wtp_threshold
) {
  
  pairing_data$pathogen <- recode(
    pairing_data$pathogen,
    "flu" = "Influenza X",
    "sars-cov" = "SARS-CoV-X")
  
  pairing_data$testing_rate <- factor(
    pairing_data$testing_rate,
    levels = c("0", "0.8", "0.99", "9999"),
    labels = c("0%", "80%", "99%", "Syndromic"))
  
  pairing_data$movement_input <- factor(
    pairing_data$movement_input,
    levels = c("moh", "iata", "iata_method2"),
    labels = c("MOH",
               "IATA w/ baseline adjustment",
               "IATA w/ no baseline adjustment"))
  
  pairing_data$seed_country <- recode(
    pairing_data$seed_country,
    "global" = "Global")
  
  
  # if(fixed_variable == "seed_country") {
  #   nmb_data <- pairing_data %>%
  #     filter(seed_country == fixed_variable_value)
  # } else {
  #   nmb_data <- pairing_data %>%
  #     filter(movement_input == fixed_variable_value)
  # }
  
  nmb_data <- pairing_data
  
  nmb_data$nmb_value <- (wtp_threshold * nmb_data$yll_ksa_total_diff) - 
    nmb_data$total_costs_diff 
  
  nmb_data_qntls <- averted_outcomes_qntls(nmb_data) %>% 
    filter(variable == "nmb_value")
  
  # wrapping_var <- ifelse(fixed_variable == "seed_country",
  #                        "movement_input",
  #                        "seed_country")
  
  # fixed_part_of_filenames <- list(
  #   "MOH" = "MOH",
  #   "IATA w/ baseline adjustment" = "iata_with_bl_adjust",
  #   "IATA w/ no baseline adjustment" = "iata_no_bl_adjust",
  #   "Indonesia" = "Indonesia",
  #   "United Kingdom" = "United Kingdom",
  #   "Global" = "Global"
  # )
  
  # Base plot code
  p <-
    ggplot(nmb_data_qntls) +
    geom_linerange(aes(x = pathogen,
                       ymin = quantile_2.5, ymax = quantile_97.5,
                       fill = testing_rate),
                   alpha=0.9,
                   position = position_dodge(width = 0.5)) +
    geom_crossbar(aes(x = pathogen, y = median,
                      ymin = quantile_25, ymax = quantile_75,
                      fill = testing_rate),
                  position = position_dodge(width = 0.5),
                  width = 0.3)
  
  # Plot 1: all facets on same scale (allows easier comparison)
  p1 <-
    p +
    facet_grid(seed_country~movement_input) +
    scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
    labs(x = "Pathogen scenario",
         y = "Net monetary benefit (2023 international $)",
         fill = "Pilgrim testing\nstrategy") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby")
  
  # Plot 2: facets on different scales (can see underlying values more easily)
  p2 <-
    p +
    ggh4x::facet_grid2(seed_country~movement_input, scales = "free_y", independent = "y") +
    scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
    labs(x = "Pathogen scenario",
         y = "Net monetary benefit (2023 international $)",
         fill = "Pilgrim testing\nstrategy") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby")
  
  ggsave(paste0(unique(nmb_data$age_input), "_",
                unique(nmb_data$pairing), "_nmb_summary_fixedscales_",
                names(wtp_threshold), ".png"),
         p1,
         width = 8, height = 6, bg = "white")
  
  ggsave(paste0(unique(nmb_data$age_input), "_",
                unique(nmb_data$pairing), "_nmb_summary_freescales_",
                names(wtp_threshold), ".png"),
         p2,
         width = 8, height = 6, bg = "white")
  
}

imap(averted_dfs, function(x, i) {
  nmb_summary_figs(x, ksa_wtp_int_dol)
  nmb_summary_figs(x, ksa_wtp_lo_int_dol)
  nmb_summary_figs(x, ksa_wtp_hi_int_dol)
})


nmb_summary_tabs <- function(pairing_data,
                             wtp_threshold
) {
  
  pairing_data$pathogen <- recode(
    pairing_data$pathogen,
    "flu" = "Influenza X",
    "sars-cov" = "SARS-CoV-X")
  
  pairing_data$testing_rate <- factor(
    pairing_data$testing_rate,
    levels = c("0", "0.8", "0.99", "9999"),
    labels = c("0%", "80%", "99%", "Syndromic"))
  
  pairing_data$movement_input <- factor(
    pairing_data$movement_input,
    levels = c("moh", "iata", "iata_method2"),
    labels = c("MOH",
               "IATA w/ baseline adjustment",
               "IATA w/ no baseline adjustment"))
  
  pairing_data$seed_country <- recode(
    pairing_data$seed_country,
    "global" = "Global")
  
  nmb_data <- pairing_data
  
  nmb_data$nmb_value <- (wtp_threshold * nmb_data$yll_ksa_total_diff) - 
    nmb_data$total_costs_diff 
  
  nmb_data_qntls <- averted_outcomes_qntls(nmb_data) %>% 
    filter(variable == "nmb_value")
  
  # Save plot_data as a table for extracting specific values
  
  # Create the gt table
  nmb_table <- nmb_data_qntls %>%
    group_by(pathogen, movement_input, seed_country) %>% 
    # Select the relevant columns
    select(
      testing_rate, median, quantile_2.5, quantile_97.5
    ) %>%
    arrange(testing_rate) %>% 
    
    # Create the gt table
    gt() %>%
    
    # Rename the columns for display
    cols_label(
      testing_rate = "Testing rate",
      median = "Median",
      quantile_2.5 = "2.5% quantile",
      quantile_97.5 = "97.5% quantile"
    ) %>%
    
    # Add a title and subtitle
    tab_header(
      title = "Summary of NMBs",
      subtitle = "Comparison of simulation scenarios"
    ) %>%
    
    # Customize the table's appearance
    tab_options(
      table.font.size = 12,
      heading.align = "center"
    )
  
  gtsave(nmb_table, paste0(unique(nmb_data$age_input), "_",
                           unique(nmb_data$pairing),
                             "_nmb_table_",
                           names(wtp_threshold), ".html"))
  gtsave(nmb_table, paste0(unique(nmb_data$age_input), "_",
                           unique(nmb_data$pairing),
                           "_nmb_table_",
                           names(wtp_threshold), ".tex"))
  
}

imap(averted_dfs, function(x, i) {
  nmb_summary_tabs(x, ksa_wtp_int_dol)
  nmb_summary_tabs(x, ksa_wtp_lo_int_dol)
  nmb_summary_tabs(x, ksa_wtp_hi_int_dol)
})



## WTP sensitivity plot ----
# (suggested by KH 04/02/2026)

nmb_wtp_sensitivity <- function(pairing_data,
                                wtp_value
) {
  
  pairing_data$pathogen <- recode(
    pairing_data$pathogen,
    "flu" = "Influenza X",
    "sars-cov" = "SARS-CoV-X")
  
  pairing_data$testing_rate <- factor(
    pairing_data$testing_rate,
    levels = c("0", "0.8", "0.99", "9999"),
    labels = c("0%", "80%", "99%", "Syndromic"))
  
  pairing_data$movement_input <- factor(
    pairing_data$movement_input,
    levels = c("moh", "iata", "iata_method2"),
    labels = c("MOH",
               "IATA w/ baseline adjustment",
               "IATA w/ no baseline adjustment"))
  
  pairing_data$seed_country <- recode(
    pairing_data$seed_country,
    "global" = "Global")
  
  nmb_data <- pairing_data
  
  nmb_data$nmb_value <- (wtp_value * nmb_data$yll_ksa_total_diff) - 
    nmb_data$total_costs_diff
  
  nmb_summary <- nmb_data %>% 
    group_by(scenario, pathogen, testing_rate, movement_input, seed_country) %>% 
    summarise(positive_nmb_prob = mean(nmb_value > 0), .groups = "drop") %>% 
    mutate(wtp_value = wtp_value)
  
  return(nmb_summary)
  
}


wtp_range <- seq(0, 45000, by = 1000)
results_all_wtp <- map_dfr(wtp_range,
                           ~ nmb_wtp_sensitivity(ages1ranked_outcomes_averted,
                                                 .x))
p_ceac <-
  ggplot(results_all_wtp,
         aes(x = wtp_value,
             y = positive_nmb_prob,
             colour = testing_rate,
             linetype = pathogen,
             group = interaction(testing_rate, pathogen))) +
  geom_line(linewidth = 1) +
  facet_grid(seed_country ~ movement_input) +
  scale_colour_paletteer_d("ggthemes::wsj_rgby") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  
  scale_x_continuous(
    breaks = seq(0, 45000, by = 10000),
    labels = function(x) ifelse(x == 0, "0", paste0(x/1000, "k"))
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25)
  ) +

labs(
  x = "Willingness to pay (Intl$ per YLL averted)",
  y = "Probability cost-effective",
  colour = "Pilgrim testing\nstrategy",
  linetype = "Pathogen"
) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave("wtp_sensitivity.png",
       p_ceac,
       width = 8.5, height = 6, bg = "white")



# Overall outcomes figures (summaries across all sims for each scenario) ----

# Add cases_ksa_total column
scenario_summaries <- scenario_summaries %>%
  mutate(cases_ksa_total = rowSums(across(c(cases_domestic,
                                            cases_ksa_nonpilgrim))))

scenario_summaries2 <- scenario_summaries2 %>%
  mutate(cases_ksa_total = rowSums(across(c(cases_domestic,
                                            cases_ksa_nonpilgrim))))

relabel_factors <- function(scenario_data) {
  
  scenario_data$pathogen <- recode(
    scenario_data$pathogen,
    "flu" = "Influenza X",
    "sars-cov" = "SARS-CoV-X")
  
  scenario_data$seed_country <- recode(
    scenario_data$seed_country,
    "global" = "Global")
  
  scenario_data$testing_rate <- factor(
    scenario_data$testing_rate,
    levels = c("0", "0.8", "0.99", "9999"),
    labels = c("0%", "80%", "99%", "Syndromic"))
  
  scenario_data$movement_input <- factor(
    scenario_data$movement_input,
    levels = c("moh", "iata", "iata_method2"),
    labels = c("MOH",
               "IATA w/ baseline adjustment",
               "IATA w/ no baseline adjustment"))
  
  scenario_data
  
}

scenario_summaries <- relabel_factors(scenario_summaries)
scenario_summaries2 <- relabel_factors(scenario_summaries2)

# Figure for main text
# cases_metric is either "ksa_cases_total" or "cases_total"

cases_comparison_figs <- function(scenario_data,
                                  cases_metric) {
  
  scenario_data_qntls <- averted_outcomes_qntls(scenario_data)
  
  plot_data <- scenario_data_qntls %>%
    filter(variable == cases_metric)
  
  ylabel <- ifelse(cases_metric == "cases_ksa_total",
                   "Total cases in KSA population\n(over 100 day simulation period)",
                   "Total cases\n(over 100 day simulation period)")
  
  p <-
    ggplot(plot_data) +
    geom_linerange(aes(x = pathogen,
                       ymin = quantile_2.5, ymax = quantile_97.5,
                       fill = testing_rate),
                   alpha=0.9,
                   position = position_dodge(width = 0.5)) +
    geom_crossbar(aes(x = pathogen, y = median,
                      ymin = quantile_25, ymax = quantile_75,
                      fill = testing_rate),
                  position = position_dodge(width = 0.5),
                  width = 0.3) +
    facet_grid(seed_country~movement_input) +
    labs(x = "Pathogen scenario",
         y = ylabel,
         fill = "Pilgrim testing\nstrategy") +
    scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby")
  
  ggsave(paste0(unique(scenario_data$age_input), "_",
                cases_metric, ".png"),
         p,
         width = 8, height = 6, bg = "white")
  
}

cases_comparison_figs(scenario_summaries, "cases_total")
cases_comparison_figs(scenario_summaries, "cases_ksa_total")
cases_comparison_figs(scenario_summaries2, "cases_total")
cases_comparison_figs(scenario_summaries2, "cases_ksa_total")

# Also generate a table containing the summary of all simulation outputs
outcomes_table_function <- function(scenario_data) {
  
  scenario_data <- scenario_data %>%
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
  scenario_data <- scenario_data %>%
    mutate(
      across(c(median, quantile_2.5, quantile_97.5, quantile_25, quantile_75, mean, sd),
             ~ if_else(variable %in% c("ksa_resident_attack_rate", "foreign_pilgrim_attack_rate"),
                       round(., 4),  # Round to 4 decimal places for the specified variables
                       round(., 1))) # Round to 1 decimal place for all other variables
    )
  
  scenario_data <- scenario_data %>%
    mutate(value_str_95 = ifelse(
      variable %in% c("ksa_resident_attack_rate", "foreign_pilgrim_attack_rate"),
      sprintf("%.4f (%.4f; %.4f)",
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
      sprintf("%.4f (%.4f; %.4f)",
              round(median, 4),
              round(quantile_25, 4),
              round(quantile_75, 4)),
      sprintf("%s (%s; %s)",
              comma(round(median, 1), accuracy = 0.1),
              comma(round(quantile_25, 1), accuracy = 0.1),
              comma(round(quantile_75, 1), accuracy = 0.1)
      ))) %>% 
    mutate(value_str_meansd = ifelse(
      variable %in% c("ksa_resident_attack_rate", "foreign_pilgrim_attack_rate"),
      sprintf("%.4f (%.4f)",
              round(mean, 4),
              round(sd, 4)),
      sprintf("%s (%s)",
              comma(round(mean, 1), accuracy = 0.1),
              comma(round(sd, 1), accuracy = 0.1)
      )))
  
  # Set order for "variable"
  scenario_data$variable <- factor(scenario_data$variable, levels = c(
    "cases_foreign",  
    "cases_domestic",
    "cases_ksa_nonpilgrim",
    "cases_ksa_total",
    "cases_total",
    "ksa_resident_attack_rate", 
    "foreign_pilgrim_attack_rate",
    "hosps_foreign",  
    "hosps_domestic",
    "hosps_ksa_nonpilgrim",
    "hosps_ksa_total",
    "hosps_total",
    "deaths_foreign",  
    "deaths_domestic",
    "deaths_ksa_nonpilgrim",
    "deaths_ksa_total",
    "deaths_total",
    "yll_foreign",  
    "yll_domestic",
    "yll_ksa_nonpilgrim",
    "yll_ksa_total",
    "yll_total",
    "imported_infections",            
    "untested_importations",
    "total_tests",
    "true_positives",                 
    "false_positives",               
    "true_negatives",                 
    "false_negatives"                
  ))
  
  # Create the gt table
  model_outputs_table <- scenario_data %>%
    filter(variable != "sim") %>% 
    group_by(pathogen, movement_input, seed_country) %>% 
    # Select the relevant columns
    select(
      testing_rate, variable, value_str_95, value_str_iqr, value_str_meansd
    ) %>%
    arrange(testing_rate, variable) %>% 
    
    # Create the gt table
    gt() %>%
    
    # Rename the columns for display
    cols_label(
      testing_rate = "Testing rate",
      variable = "Variable",
      value_str_95 = "Median (95% quantiles)",
      value_str_iqr = "Median (IQR)",
      value_str_meansd = "Mean (SD)"
    ) %>%
    
    # Add a title and subtitle
    tab_header(
      title = "Summary of model outputs",
      subtitle = "Comparison of simulation scenarios"
    ) %>%
    
    # Customize the table's appearance
    tab_options(
      table.font.size = 12,
      heading.align = "center"
    )
}

ages1_all_model_outcomes_table <- outcomes_table_function(scenario_summaries)
ages2_all_model_outcomes_table <- outcomes_table_function(scenario_summaries2)

gtsave(ages1_all_model_outcomes_table, "ages1_all_model_outcomes_table.tex")
gtsave(ages1_all_model_outcomes_table, "ages1_all_model_outcomes_table.html")

gtsave(ages2_all_model_outcomes_table, "ages2_all_model_outcomes_table.tex")
gtsave(ages2_all_model_outcomes_table, "ages2_all_model_outcomes_table.html")


# Cases figs for suppl.

outcome_comparison_suppl_figs <- function(scenario_data,
                                          outcomes_metric,
                                          movement_source) {
  
  scenario_data_qntls <- averted_outcomes_qntls(scenario_data)
  
  # Define lookup table for outcomes and y-axis labels
  outcome_lookup <- list(
    "cases" = list(
      outcomes = c("cases_foreign", "cases_domestic", "cases_ksa_nonpilgrim"),
      ylabel = "Number of cases\n(over 100 day simulation period)"
    ),
    "hosps" = list(
      outcomes = c("hosps_foreign", "hosps_domestic", "hosps_ksa_nonpilgrim"),
      ylabel = "Number of hospitalisations\n(over 100 day simulation period)"
    ),
    "deaths" = list(
      outcomes = c("deaths_foreign", "deaths_domestic", "deaths_ksa_nonpilgrim"),
      ylabel = "Number of deaths\n(over 100 day simulation period)"
    )
  )
  
  movement_part_of_filenames <- list(
    "MOH" = "MOH",
    "IATA w/ baseline adjustment" = "iata_with_bl_adjust",
    "IATA w/ no baseline adjustment" = "iata_no_bl_adjust"
  )
  
  outcomes <- outcome_lookup[[outcomes_metric]]$outcomes
  ylabel <- outcome_lookup[[outcomes_metric]]$ylabel
  
  plot_data <- scenario_data_qntls %>%
    filter(variable %in% outcomes & 
             movement_input == movement_source)
  
  # Change some variable names / types for plotting
  plot_data$variable <- factor(plot_data$variable,
                               levels = outcomes,
                               labels = c("Foreign pilgrims",
                                          "Domestic pilgrims",
                                          "Domestic non-pilgrims"))
  
  p <-
    ggplot(plot_data) +
    geom_linerange(aes(x = pathogen,
                       ymin = quantile_2.5, ymax = quantile_97.5,
                       fill = testing_rate),
                   alpha=0.9,
                   position = position_dodge(width = 0.5)) +
    geom_crossbar(aes(x = pathogen, y = median,
                      ymin = quantile_25, ymax = quantile_75,
                      fill = testing_rate),
                  position = position_dodge(width = 0.5),
                  width = 0.3) +
    ggh4x::facet_grid2(seed_country~variable, scales = "free_y", independent = "y") +
    # scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
    labs(title = movement_source,
         x = "Pathogen scenario",
         y = ylabel,
         fill = "Pilgrim testing\nstrategy") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby")
  
  ggsave(paste0(unique(scenario_data$age_input), "_",
                movement_part_of_filenames[[movement_source]], "_",
                outcomes_metric, "_by_group.png"),
         p,
         width = 8, height = 6, bg = "white")
  
}

for (outcome in c("cases", "hosps", "deaths")) {
  for (source in c("MOH", "IATA w/ baseline adjustment", "IATA w/ no baseline adjustment")) {
    outcome_comparison_suppl_figs(scenario_summaries, outcome, source)
    outcome_comparison_suppl_figs(scenario_summaries2, outcome, source)
  }
}

# Costs comparison figure

costs_comparisons <- function(scenario_data) {
  
  scenario_data_qntls <- scenario_data %>%
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
  
  plot_data <- scenario_data_qntls %>%
    filter(variable == "total_costs")
  
  ylabel <- "Total costs (2023 international $)\n(over 100 day simulation period)"
  
  p1 <-
    ggplot(plot_data) +
    geom_linerange(aes(x = pathogen,
                       ymin = quantile_2.5, ymax = quantile_97.5,
                       fill = testing_rate),
                   alpha=0.9,
                   position = position_dodge(width = 0.5)) +
    geom_crossbar(aes(x = pathogen, y = median,
                      ymin = quantile_25, ymax = quantile_75,
                      fill = testing_rate),
                  position = position_dodge(width = 0.5),
                  width = 0.3) +
    facet_grid(seed_country~movement_input) +
    scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
    labs(x = "Pathogen scenario",
         y = ylabel,
         fill = "Pilgrim testing\nstrategy") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby")
  
  p2 <-
    ggplot(plot_data) +
    geom_linerange(aes(x = pathogen,
                       ymin = quantile_2.5, ymax = quantile_97.5,
                       fill = testing_rate),
                   alpha=0.9,
                   position = position_dodge(width = 0.5)) +
    geom_crossbar(aes(x = pathogen, y = median,
                      ymin = quantile_25, ymax = quantile_75,
                      fill = testing_rate),
                  position = position_dodge(width = 0.5),
                  width = 0.3) +
    facet_grid(seed_country~movement_input, scales = "free") +
    scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
    labs(x = "Pathogen scenario",
         y = ylabel,
         fill = "Pilgrim testing\nstrategy") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby")
  
  ggsave(paste0(unique(scenario_data$age_input),
                "_total_costs.png"),
         p1,
         width = 8, height = 6, bg = "white")
  
  ggsave(paste0(unique(scenario_data$age_input),
                "_total_costs_freescales.png"),
         p2,
         width = 8, height = 6, bg = "white")
  
  # Save plot_data as a table for extracting specific values
  
  # Create the gt table
  costs_table <- plot_data %>%
    group_by(pathogen, movement_input, seed_country) %>% 
    # Select the relevant columns
    select(
      testing_rate, median, quantile_2.5, quantile_97.5
    ) %>%
    arrange(testing_rate) %>% 
    
    # Create the gt table
    gt() %>%
    
    # Rename the columns for display
    cols_label(
      testing_rate = "Testing rate",
      median = "Variable",
      quantile_2.5 = "2.5% quantile",
      quantile_97.5 = "97.5% quantile"
    ) %>%
    
    # Add a title and subtitle
    tab_header(
      title = "Summary of costs",
      subtitle = "Comparison of simulation scenarios"
    ) %>%
    
    # Customize the table's appearance
    tab_options(
      table.font.size = 12,
      heading.align = "center"
    )
  
  gtsave(costs_table, paste0(unique(scenario_data$age_input),
                             "_costs_table.html"))
  gtsave(costs_table, paste0(unique(scenario_data$age_input),
                             "_costs_table.tex"))
  
}

costs_comparisons(scenario_summaries)
costs_comparisons(scenario_summaries2)


# Costs comparison figure

test_outcome_comparisons <- function(scenario_data,
                                     seed_location) {
  
  scenario_data$quarantined <- scenario_data$true_positives + scenario_data$false_positives
  
  scenario_data_qntls <- scenario_data %>%
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
  
  plot_data <- scenario_data_qntls %>%
    filter(variable %in% c("imported_infections", "untested_importations",
                           "false_negatives", "quarantined") &
             seed_country == seed_location)
  
  # Change some variable names / types for plotting
  plot_data$variable <- factor(plot_data$variable,
                               levels = c("imported_infections", "untested_importations",
                                          "false_negatives", "quarantined"),
                               labels = c("Imported infections", "Untested importations",
                                          "False negatives", "Quarantined"))
  
  ylabel <- "Number of individuals\n(over 100 day simulation period)"
  
  p1 <-
    ggplot(plot_data) +
    geom_linerange(aes(x = pathogen,
                       ymin = quantile_2.5, ymax = quantile_97.5,
                       fill = testing_rate),
                   alpha=0.9,
                   position = position_dodge(width = 0.5)) +
    geom_crossbar(aes(x = pathogen, y = median,
                      ymin = quantile_25, ymax = quantile_75,
                      fill = testing_rate),
                  position = position_dodge(width = 0.5),
                  width = 0.3) +
    facet_grid(variable~movement_input, scales = "free") +
    labs(title = seed_location,
         x = "Pathogen scenario",
         y = ylabel,
         fill = "Pilgrim testing\nstrategy") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby")
  
  ggsave(paste0(unique(scenario_data$age_input), "_",
                seed_location,
                "_testing_outcomes.png"),
         p1,
         width = 8, height = 8, bg = "white")
  
  # Save plot_data as a table for extracting specific values
  
  # Create the gt table
  test_outcomes_table <- plot_data %>%
    group_by(pathogen, movement_input, seed_country) %>% 
    # Select the relevant columns
    select(
      testing_rate, variable, median, quantile_2.5, quantile_97.5, quantile_25, quantile_75,
    ) %>%
    arrange(testing_rate) %>% 
    
    # Create the gt table
    gt() %>%
    
    # Rename the columns for display
    cols_label(
      testing_rate = "Testing rate",
      variable = "Variable",
      median = "Median",
      quantile_2.5 = "2.5% quantile",
      quantile_97.5 = "97.5% quantile",
      quantile_25 = "25% quantile",
      quantile_75 = "75% quantile"
    ) %>%
    
    # Add a title and subtitle
    tab_header(
      title = "Summary of test outcomes",
      subtitle = "Comparison of simulation scenarios"
    ) %>%
    
    # Customize the table's appearance
    tab_options(
      table.font.size = 12,
      heading.align = "center"
    )
  
  gtsave(test_outcomes_table, paste0(unique(scenario_data$age_input),
                                     "_",
                                     seed_location,
                                     "_tests_table.html"))
  gtsave(test_outcomes_table, paste0(unique(scenario_data$age_input),
                                     "_",
                                     seed_location,
                                     "_tests_table.tex"))
  
}

test_outcome_comparisons(scenario_summaries, "Indonesia")
test_outcome_comparisons(scenario_summaries2, "Indonesia")
test_outcome_comparisons(scenario_summaries, "Global")
test_outcome_comparisons(scenario_summaries2, "Global")
test_outcome_comparisons(scenario_summaries, "United Kingdom")
test_outcome_comparisons(scenario_summaries2, "United Kingdom")