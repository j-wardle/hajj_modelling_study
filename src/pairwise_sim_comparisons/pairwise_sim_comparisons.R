# setwd("C:/Users/jw2519/OneDrive - Imperial College London/Hajj project/Flight model/hajj_project/src/estimate_deaths")

orderly2::orderly_strict_mode()

orderly2::orderly_parameters(nsim = NULL,
                             long_run = NULL)

# Set dependencies
orderly2::orderly_dependency(
  "visualize_sim_results",
  quote(latest(
    # latest(
    parameter:nsim == this:nsim &&
    parameter:long_run == this:long_run)),
      # parameter:nsim == 1000 &&
      # parameter:long_run == TRUE)),
  # "latest(parameter:nsim == this:nsim)",
  c(scenario_summaries.rds = "scenario_summaries.rds"))

orderly2::orderly_dependency(
  "pilgrim_age_distribution",
  "latest()",
  c(pilgrim_age_distribution.rds = "pilgrim_age_distribution.rds",
    ksa_age_dist_incl_children.rds = "ksa_age_dist_incl_children.rds",
    ksa_age_dist_excl_children.rds = "ksa_age_dist_excl_children.rds"))

# Set resources
orderly2::orderly_resource("influenza_ifr.csv")
orderly2::orderly_resource("influenza_ihr.csv")
orderly2::orderly_resource("sarscov2_ifr.csv")
orderly2::orderly_resource("sarscov2_ihr.csv")

# Load various functions used in task
orderly2::orderly_resource("R/utils.R")
source("R/utils.R")

# Set task artefacts
orderly2::orderly_artefact(description = "Pairwise outputs",
                           files = c("ages1pairwise_outcomes_averted.rds",
                                     "ages1random_pairwise_outcomes_averted.rds",
                                     "ages1ranked_outcomes_averted.rds",
                                     "ages1pairwise_outcomes_averted_qntls.rds",
                                     "ages1random_pairwise_averted_qntls.rds",
                                     "ages1ranked_outcomes_averted_qntls.rds",
                                     "ages2pairwise_outcomes_averted.rds",
                                     "ages2random_pairwise_outcomes_averted.rds",
                                     "ages2ranked_outcomes_averted.rds",
                                     "ages2pairwise_outcomes_averted_qntls.rds",
                                     "ages2random_pairwise_averted_qntls.rds",
                                     "ages2ranked_outcomes_averted_qntls.rds",
                                     "ages1_full_summary.rds",
                                     "ages2_full_summary.rds")
)

# Load packages
library(tidyverse)
library(scales)
library(testthat)

# Load in scenario summaries
scenario_summaries <- readRDS("scenario_summaries.rds")

# Read in the ifr/ihr files
flu_ifr_distribution <- read.csv("influenza_ifr.csv")
sars_ifr_distribution <- read.csv("sarscov2_ifr.csv")
flu_ihr_distribution <- read.csv("influenza_ihr.csv")
sars_ihr_distribution <- read.csv("sarscov2_ihr.csv")

# Read in the age files
pilgrim_age_distribution <- readRDS("pilgrim_age_distribution.rds")
ksa_age_dist_incl_children <- readRDS("ksa_age_dist_incl_children.rds")
ksa_age_dist_excl_children = readRDS("ksa_age_dist_excl_children.rds")

test_that("pilgrim age categories are the same",
          expect_identical(pilgrim_age_distribution$age,
                           as.vector(ksa_age_dist_excl_children$age)))

# scenario_summaries <- scenario_summaries_orig # just while we develop

# Set seed for reproducibility
set.seed(42)

# Adjusted IFR and IHR distributions for use with pilgrim age dists
pathogen_ifr_adjusted <- list(
  "flu" = adjust_ifr_categories(flu_ifr_distribution),
  "sars-cov" = adjust_ifr_categories(sars_ifr_distribution)
)
pathogen_ihr_adjusted <- list(
  "flu" = adjust_ihr_categories(flu_ihr_distribution),
  "sars-cov" = adjust_ihr_categories(sars_ihr_distribution)
)

# Adjusted IFR and IHR distributions for use with pilgrim age dists
pathogen_ifr_adjusted_inc_child <- list(
  "flu" = adjust_ifr_categories_child(flu_ifr_distribution),
  "sars-cov" = adjust_ifr_categories_child(sars_ifr_distribution)
)
pathogen_ihr_adjusted_inc_child <- list(
  "flu" = adjust_ihr_categories_child(flu_ihr_distribution),
  "sars-cov" = adjust_ihr_categories_child(sars_ihr_distribution)
)



# Add the average age of each age category (repeat because we have M&F categories)
pilgrim_age_distribution$average_age <- rep(
  c(20, 30, 40, 50, 60, 70, 80, 90),
  times = 2
)

ksa_age_dist_excl_children$average_age <- rep(
  c(20, 30, 40, 50, 60, 70, 80, 90),
  times = 2
)

ksa_age_dist_incl_children$average_age <- rep(
  c(7.5, 20, 30, 40, 50, 60, 70, 80, 90),
  times = 2
)

# Set the life expectancy value for use in YLL calculations
# Use 2022 life tables from Japan:
# https://www.mhlw.go.jp/english/database/db-hw/lifetb22/dl/lifetb22-06.pdf

life_expectancy <- c(
  # Females by average age of categories
  "F20" = 67.39, "F30" = 57.56,
  "F40" = 47.77, "F50" = 38.16,
  "F60" = 28.84, "F70" = 19.89,
  "F80" = 11.74, "F90" = 5.47,
  # Males by average age of categories
  "M20" = 61.39, "M30" = 51.66,
  "M40" = 41.97, "M50" = 32.51,
  "M60" = 23.59, "M70" = 15.56,
  "M80" = 8.89, "M90" = 4.14
)

life_expectancy_child <- c(
  # Females by average age of categories
  "F<15" = mean(c(87.09, 82.28, 77.30, 72.33)), #take mean across ages 0/5/10/15
  "F20" = 67.39, "F30" = 57.56,
  "F40" = 47.77, "F50" = 38.16,
  "F60" = 28.84, "F70" = 19.89,
  "F80" = 11.74, "F90" = 5.47,
  # Males by average age of categories
  "M<15" = mean(c(81.05, 76.25, 71.28, 66.31)), #take mean across ages 0/5/10/15
  "M20" = 61.39, "M30" = 51.66,
  "M40" = 41.97, "M50" = 32.51,
  "M60" = 23.59, "M70" = 15.56,
  "M80" = 8.89, "M90" = 4.14
)

# life_expectancy <- 84 # compare with Japan 2022: https://data.worldbank.org/indicator/SP.DYN.LE00.IN?locations=JP

# Process scenario_summaries ----

# For ease, rename epidemic_size cols as 'cases'
# Also remove the testing outputs; not using those for the moment
scenario_summaries <- scenario_summaries %>% 
  rename(cases_foreign = foreign_pilgrim_epidemic_size,
         cases_domestic = domestic_pilgrim_epidemic_size,
         cases_ksa_nonpilgrim = ksa_nonpilgrim_epidemic_size) #%>% 
  # select(scenario, sim, cases_foreign, cases_domestic, cases_ksa_nonpilgrim,
         # pathogen, testing_rate, movement_input, seed_country)


# Add a 'total_epidemic_size' column
scenario_summaries <- scenario_summaries %>%
  mutate(cases_total = rowSums(across(c(cases_foreign, 
                                        cases_domestic, 
                                        cases_ksa_nonpilgrim))))

# Add columns that compute the hospitalisations and deaths for each case output

# We will run two combinations of age inputs
# First inputs
foreign_pilgrim_age <- pilgrim_age_distribution
domestic_pilgrim_age <- ksa_age_dist_excl_children
ksa_nonpilgrim_age <- ksa_age_dist_incl_children

# Second inputs
foreign_pilgrim_age2 <- pilgrim_age_distribution
domestic_pilgrim_age2 <- pilgrim_age_distribution
ksa_nonpilgrim_age2 <- ksa_age_dist_incl_children

# Function for summarising scenario output

summarise_scenarios <- function(scenario_summaries,
                                foreign_pilgrim_age,
                                domestic_pilgrim_age,
                                ksa_non_pilgrim_age) {

scenario_summaries <- scenario_summaries %>%
  rowwise() %>%
  mutate(
    
    # Apply estimate_hosps to each epidemic size column
    hosps_foreign = estimate_hosps(cases = cases_foreign, 
                                   age_dist = foreign_pilgrim_age, 
                                   adjusted_ihr = pathogen_ihr_adjusted[[pathogen]]),
    hosps_domestic = estimate_hosps(cases = cases_domestic, 
                                    age_dist = domestic_pilgrim_age, 
                                    adjusted_ihr = pathogen_ihr_adjusted[[pathogen]]),
    hosps_ksa_nonpilgrim = estimate_hosps(cases = cases_ksa_nonpilgrim, 
                                          age_dist = ksa_nonpilgrim_age, 
                                          adjusted_ihr = pathogen_ihr_adjusted_inc_child[[pathogen]]),
    
    # Apply estimate_deaths to each epidemic size column
    deaths_foreign = estimate_deaths(cases = cases_foreign, 
                                     age_dist = foreign_pilgrim_age, 
                                     adjusted_ifr = pathogen_ifr_adjusted[[pathogen]]),
    deaths_domestic = estimate_deaths(cases = cases_domestic, 
                                      age_dist = domestic_pilgrim_age, 
                                      adjusted_ifr = pathogen_ifr_adjusted[[pathogen]]),
    deaths_ksa_nonpilgrim = estimate_deaths(cases = cases_ksa_nonpilgrim, 
                                            age_dist = ksa_nonpilgrim_age, 
                                            adjusted_ifr = pathogen_ifr_adjusted_inc_child[[pathogen]]),
    
    # Apply estimate_ylls to each epidemic size column
    yll_foreign = estimate_ylls(cases = cases_foreign, 
                                age_dist = foreign_pilgrim_age, 
                                adjusted_ifr = pathogen_ifr_adjusted[[pathogen]],
                                life_expectancy = life_expectancy),
    yll_domestic = estimate_ylls(cases = cases_domestic, 
                                 age_dist = domestic_pilgrim_age, 
                                 adjusted_ifr = pathogen_ifr_adjusted[[pathogen]],
                                 life_expectancy = life_expectancy),
    yll_ksa_nonpilgrim = estimate_ylls(cases = cases_ksa_nonpilgrim, 
                                       age_dist = ksa_nonpilgrim_age, 
                                       adjusted_ifr = pathogen_ifr_adjusted_inc_child[[pathogen]],
                                       life_expectancy = life_expectancy_child)
    
  ) %>%
  ungroup() %>% 
  # Compute totals (within KSA and across total population)
  mutate(hosps_ksa_total = rowSums(across(c(hosps_domestic, 
                                            hosps_ksa_nonpilgrim))),
         deaths_ksa_total = rowSums(across(c(deaths_domestic, 
                                             deaths_ksa_nonpilgrim))),
         yll_ksa_total = rowSums(across(c(yll_domestic,
                                          yll_ksa_nonpilgrim))),
         hosps_total = rowSums(across(c(hosps_foreign, 
                                        hosps_domestic, 
                                        hosps_ksa_nonpilgrim))),
         deaths_total = rowSums(across(c(deaths_foreign, 
                                         deaths_domestic, 
                                         deaths_ksa_nonpilgrim))),
         yll_total = rowSums(across(c(yll_foreign,
                                      yll_domestic,
                                      yll_ksa_nonpilgrim)))
  )
}

# Apply this function to the two sets of age inputs

summaries_using_ages1 <- summarise_scenarios(
  scenario_summaries,
  foreign_pilgrim_age,
  domestic_pilgrim_age,
  ksa_non_pilgrim_age
)

summaries_using_ages2 <- summarise_scenarios(
  scenario_summaries,
  foreign_pilgrim_age2,
  domestic_pilgrim_age2,
  ksa_non_pilgrim_age2
)


# Estimate costs for each sim ----

# Define the costs to use
# copied values over from cost_estimates.R script in 'dev' folder
costs <- c(airport_screening = 0.5178323,
           diagnostic_low = 10.61011,
           diagnostic_hi = 106.1011,
           hospital_treatment = 21607.48,
           quarantine = 2272.685)

## Set total scenario airport costs

foreign_pilgrim_population <- 1732320

scenario_costs_airport_screening <- foreign_pilgrim_population * costs[["airport_screening"]]

## Function for estimating costs in each scenario

add_costs <- function(summary_data) {
  
  summary_data$test_costs_lo <- summary_data$total_tests * 
    costs["diagnostic_low"]
  summary_data$test_costs_hi <- summary_data$total_tests * 
    costs["diagnostic_hi"]
  
  summary_data$quarantine_costs <- (
    summary_data$true_positives +
      summary_data$false_positives) *
    costs["quarantine"]
  
  summary_data$treatment_costs <- summary_data$hosps_total *
    costs["hospital_treatment"]
  
  # For airport screening costs, we assume that the total cost has a linear
  # relationship with proportion of people tested.
  # This ignores potential set-up/equipment costs.
  # In 80% and 99% testing scenarios, we 
  # In syndromic screening strategy, we assume that everyone passes through a preliminary screening step
  summary_data$airport_screening_costs <- 
    ifelse(summary_data$testing_rate != 9999,
           summary_data$testing_rate * scenario_costs_airport_screening,
           # (summary_data$testing_rate/foreign_pilgrim_population) *
           #   scenario_costs_airport_screening)
           scenario_costs_airport_screening) 
            # For syndromic scenario, assume we have thermal screening for everyone,
            # the cost of which per passenger is the same as the administrative/staffing
            # costs of implementing the other airport testing strategies
  
  # Total scenario costs:
  
  summary_data$total_costs <- 
    summary_data$airport_screening_costs +
    summary_data$test_costs_lo +
    summary_data$quarantine_costs +
    summary_data$treatment_costs
  
  # Also compute for sensitivity where we assume that tests are 10x higher in cost
  summary_data$total_costs_hi <- 
    summary_data$airport_screening_costs +
    summary_data$test_costs_hi +
    summary_data$quarantine_costs +
    summary_data$treatment_costs
  
  summary_data
  
}

summaries_using_ages1 <- add_costs(summaries_using_ages1)
summaries_using_ages2 <- add_costs(summaries_using_ages2)

# Save outputs at this stage as we will create some plots based on the full scenario summaries
saveRDS(summaries_using_ages1, "ages1_full_summary.rds")
saveRDS(summaries_using_ages2, "ages2_full_summary.rds")

  
# Now compute diffs vs baseline

# List of columns to compare
columns_to_compare <- c(
  # Case cols
  "cases_foreign", "cases_domestic",
  "cases_ksa_nonpilgrim", "cases_total",
  
  # Hospitalisation cols
  "hosps_foreign", "hosps_domestic",
  "hosps_ksa_nonpilgrim", "hosps_ksa_total", "hosps_total",
  
  # Deaths cols
  "deaths_foreign", "deaths_domestic",
  "deaths_ksa_nonpilgrim", "deaths_ksa_total", "deaths_total",
  
  # YLL cols
  "yll_foreign", "yll_domestic",
  "yll_ksa_nonpilgrim", "yll_ksa_total", "yll_total",
  
  # Costs cols
  "total_costs",
  "total_costs_hi"
)

# Loop for computing and saving averted outcomes across different sim pairing approaches

for (i in c("ages1", "ages2")) {
  
  print(i)
  
  if(i == "ages1") {
    scenario_summaries <- summaries_using_ages1
  } else {
    scenario_summaries <- summaries_using_ages2
  }
  
  
  ## 1st approach: pair sims by sim number ----
  
  # Set baselines: Filter scenarios where testing_rate == 0
  baseline <- scenario_summaries %>%
    filter(testing_rate == 0) %>%
    select(sim, seed_country, movement_input, pathogen, all_of(columns_to_compare))
  
  # Rename baseline columns to distinguish them
  baseline <- baseline %>%
    rename_with(~ paste0(., "_baseline"), all_of(columns_to_compare))
  
  # Compute the averted outcomes with function defined above
  scenario_summaries_with_averts <- compute_averted_outcomes(baseline,
                                                             scenario_summaries,
                                                             "sim")
  
  # Invert the costs_diff
  # compute_averted_outcomes function above computes (baseline - testing_scenario).
  # This is fine for epi outcomes, as we want to know how many cases, hosps, deaths,
  # and YLL are averted by applying the testing scenario. However, the additional
  # cost of the testing scenario should be computed as testing_cost - baseline_cost.
  scenario_summaries_with_averts$total_costs_diff <- -(scenario_summaries_with_averts$total_costs_diff)
  
  scenario_summaries_with_averts$total_costs_hi_diff <- -(scenario_summaries_with_averts$total_costs_hi_diff)
  
  saveRDS(scenario_summaries_with_averts,
          paste0(i, "pairwise_outcomes_averted.rds"))
  
  
  ## 2nd approach: pair sims by outcome ranking (total cases) ----
  
  # Rank sims within each scenario based on cases_total
  ranked_scenario_summaries <- scenario_summaries %>% 
    group_by(scenario) %>% 
    arrange(scenario, desc(cases_total)) %>%
    mutate(cases_rank = rank(cases_total, ties.method = "first"))
  
  # Set baselines: Filter scenarios where testing_rate == 0
  ranked_baseline <- ranked_scenario_summaries %>%
    ungroup() %>% 
    filter(testing_rate == 0) %>%
    select(cases_rank, seed_country, movement_input, pathogen, all_of(columns_to_compare))
  
  # Rename baseline columns to distinguish them
  ranked_baseline <- ranked_baseline %>%
    rename_with(~ paste0(., "_baseline"), all_of(columns_to_compare))
  
  # Compute the averted outcomes with function defined above
  ranked_scenario_summaries_with_averts <- compute_averted_outcomes(ranked_baseline,
                                                                    ranked_scenario_summaries,
                                                                    "cases_rank")
  
  # Invert the costs_diff
  ranked_scenario_summaries_with_averts$total_costs_diff <- -(ranked_scenario_summaries_with_averts$total_costs_diff)
  ranked_scenario_summaries_with_averts$total_costs_hi_diff <- -(ranked_scenario_summaries_with_averts$total_costs_hi_diff)
  
  saveRDS(ranked_scenario_summaries_with_averts,
          paste0(i, "ranked_outcomes_averted.rds"))
  
  
  ## 3rd approach: pair sims 'randomly' ----
  # I do this by pairing with the sim that is 20 along in the sim sequence
  
  random_scenario_summaries <- scenario_summaries %>% 
    mutate(revised_sim = sim + 20)
  
  random_scenario_summaries$revised_sim[random_scenario_summaries$revised_sim > 1000] <- 
    random_scenario_summaries$revised_sim[random_scenario_summaries$revised_sim > 1000] - 1000
  
  random_scenario_summaries2 <- random_scenario_summaries %>% 
    mutate(sim = ifelse(testing_rate == 0, sim, revised_sim))
  
  # Set baselines: Filter scenarios where testing_rate == 0
  random_baseline <- random_scenario_summaries2 %>%
    filter(testing_rate == 0) %>%
    select(sim, seed_country, movement_input, pathogen, all_of(columns_to_compare))
  
  # Rename baseline columns to distinguish them
  random_baseline <- random_baseline %>%
    rename_with(~ paste0(., "_baseline"), all_of(columns_to_compare))
  
  # Compute the averted outcomes with function defined above
  random_scenario_summaries_with_averts <- compute_averted_outcomes(random_baseline,
                                                                    random_scenario_summaries2,
                                                                    "sim")
  
  # Invert the costs_diff
  random_scenario_summaries_with_averts$total_costs_diff <- -(random_scenario_summaries_with_averts$total_costs_diff)
  random_scenario_summaries_with_averts$total_costs_hi_diff <- -(random_scenario_summaries_with_averts$total_costs_hi_diff)
  
  saveRDS(random_scenario_summaries_with_averts,
          paste0(i, "random_pairwise_outcomes_averted.rds"))
  
  
  # Summarise the averted outcome metrics for figs/tables ----
  
  ## Apply quantile functions to outputs from each approach
  
  ## 1st: pairwise comparisons
  scenario_summaries_with_averts_qntls <- averted_outcomes_qntls(
    scenario_summaries_with_averts)
  
  saveRDS(scenario_summaries_with_averts_qntls,
          paste0(i, "pairwise_outcomes_averted_qntls.rds"))
  
  ## 2nd: ranked comparisons
  ranked_scenario_summaries_with_averts_qntls <- averted_outcomes_qntls(
    ranked_scenario_summaries_with_averts)
  
  saveRDS(ranked_scenario_summaries_with_averts_qntls,
          paste0(i, "ranked_outcomes_averted_qntls.rds"))
  
  ## 3rd: random pairwise comparisons
  random_scenario_summaries_with_averts_qntls <- averted_outcomes_qntls(
    random_scenario_summaries_with_averts)
  
  saveRDS(random_scenario_summaries_with_averts_qntls,
          paste0(i, "random_pairwise_averted_qntls.rds"))
  
  
}



# # Create a table comparing the different sim approaches
# # and what this means for median and uncertainty
# # To do: move this to the pairwise figs task
# 
# library(gt)
# 
# qntl_summaries <- list(
#   "matched_seed" = ages1pairwise_outcomes_averted_qntls,
#   "random" = ages1random_pairwise_averted_qntls,
#   "ranked" = ages1ranked_outcomes_averted_qntls
# )
# 
# qntl_summaries_df <- bind_rows(qntl_summaries, .id = "pairing")
# 
# qntl_summaries_df <- qntl_summaries_df %>% 
#   filter(variable == "yll_ksa_total_diff")
# 
# # Load the necessary package
# library(gt)
# 
# # Assuming qntl_summaries_df is your dataframe, create the gt table
# qntl_table <- qntl_summaries_df %>%
#   group_by(pathogen, movement_input, seed_country) %>% 
#   # filter(pathogen == "sars-cov" & movement_input == "moh" & seed_country) %>% 
#   # Select the relevant columns
#   select(
#     pairing, testing_rate, median, quantile_2.5, quantile_97.5, mean, sd
#   ) %>%
#   arrange(testing_rate) %>% 
#   
#   # Create the gt table
#   gt() %>%
#   
#   # Rename the columns for display
#   cols_label(
#     pairing = "Pairing",
#     testing_rate = "Testing rate",
#     median = "Median",
#     quantile_2.5 = "Quantile 2.5",
#     quantile_97.5 = "Quantile 97.5",
#     mean = "Mean",
#     sd = "SD"
#   ) %>%
#   
#   # Add a title and subtitle
#   tab_header(
#     title = "Summary of YLL averted",
#     subtitle = "Comparison of simulation pairing methods"
#   ) %>%
#   
#   # Format numeric columns (optional, customize as per your needs)
#   fmt_number(
#     columns = c(median, quantile_2.5, quantile_97.5, mean, sd),
#     decimals = 2
#   ) %>%
#   
#   # Customize the table's appearance (optional)
#   tab_options(
#     table.font.size = 12,
#     heading.align = "center"
#   )
# 
# # Save as PDF
# gtsave(qntl_table, "ages1_table_output.html")







# NEXT STEPS FOR THIS TASK ----

# Do we want to apply any of this to the testing effectiveness numbers? (NOT FOR NOW)


# 
# 
# 
# # Define the columns to which the functions will be applied
# epidemic_columns <- c("foreign_pilgrim_epidemic_size_diff", 
#                       "domestic_pilgrim_epidemic_size_diff", 
#                       "ksa_nonpilgrim_epidemic_size_diff", 
#                       "total_epidemic_size_diff")
# 
# # Apply the functions to each row
# scenario_summaries_with_estimates <- scenario_summaries_with_diff %>%
#   rowwise() %>%
#   mutate(
#     
#     # Apply estimate_deaths to each epidemic size column
#     deaths_foreign = estimate_deaths(cases = foreign_pilgrim_epidemic_size_diff, 
#                                      age_dist = age_distribution, 
#                                      adjusted_ifr = pathogen_ifr_adjusted[[pathogen]]),
#     deaths_domestic = estimate_deaths(cases = domestic_pilgrim_epidemic_size_diff, 
#                                       age_dist = age_distribution, 
#                                       adjusted_ifr = pathogen_ifr_adjusted[[pathogen]]),
#     deaths_ksa_nonpilgrim = estimate_deaths(cases = ksa_nonpilgrim_epidemic_size_diff, 
#                                             age_dist = age_distribution, 
#                                             adjusted_ifr = pathogen_ifr_adjusted[[pathogen]]),
#     deaths_total = estimate_deaths(cases = total_epidemic_size_diff, 
#                                    age_dist = age_distribution, 
#                                    adjusted_ifr = pathogen_ifr_adjusted[[pathogen]])
#     
#   ) %>%
#   ungroup()
# 
# # THIS CODE IS WORKING
# # I THINK IT'S BEST TO APPLY IT AT THE TOP ON THE RAW CASE NUMBERS, THEN COMPUTE THE DIFF
# # LET'S CONSIDER HOW WE MAKE IT A STOCHASTIC PROCESS TOO, SO THAT THE % AVERTED IS NOT ALWAYS THE SAME