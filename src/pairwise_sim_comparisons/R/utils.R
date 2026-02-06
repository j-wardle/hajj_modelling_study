# Define functions and process age/ifr/ihr inputs ----
# create a function to align the categories of the IFR/IHR distribution with
# the categories in the age_distribution

# Generalized function to adjust categories for IFR or IHR
adjust_categories <- function(distribution, metric) {
  
  # Convert to proportion (assumes values are percentages)
  # distribution[[metric]] <- distribution[[metric]] / 100 # Patrick's values were already proportions
  
  adjusted_values <- c(
    "15-24" = (distribution[[metric]][distribution$age == "15-19"] +
                 distribution[[metric]][distribution$age == "20-24"]) / 2,
    "25-34" = (distribution[[metric]][distribution$age == "25-29"] +
                 distribution[[metric]][distribution$age == "30-34"]) / 2,
    "35-44" = (distribution[[metric]][distribution$age == "35-39"] +
                 distribution[[metric]][distribution$age == "40-44"]) / 2,
    "45-54" = (distribution[[metric]][distribution$age == "45-49"] +
                 distribution[[metric]][distribution$age == "50-54"]) / 2,
    "55-64" = (distribution[[metric]][distribution$age == "55-59"] +
                 distribution[[metric]][distribution$age == "60-64"]) / 2,
    "65-74" = (distribution[[metric]][distribution$age == "65-69"] +
                 distribution[[metric]][distribution$age == "70-74"]) / 2,
    "75-84" = (distribution[[metric]][distribution$age == "75-79"] +
                 distribution[[metric]][distribution$age == ">80"]) / 2,
    ">85" = distribution[[metric]][distribution$age == ">80"]
  )
  
  adjusted_values
}

# Generalized function to adjust categories for IFR or IHR incl. children
adjust_categories_child <- function(distribution, metric) {
  
  # Convert to proportion (assumes values are percentages)
  # distribution[[metric]] <- distribution[[metric]] / 100 # Patrick's values were already proportions
  
  adjusted_values <- c(
    "<15" = (distribution[[metric]][distribution$age == "0-4"] +
                 distribution[[metric]][distribution$age == "5-9"] +
               distribution[[metric]][distribution$age == "10-14"]) / 3,
    "15-24" = (distribution[[metric]][distribution$age == "15-19"] +
                 distribution[[metric]][distribution$age == "20-24"]) / 2,
    "25-34" = (distribution[[metric]][distribution$age == "25-29"] +
                 distribution[[metric]][distribution$age == "30-34"]) / 2,
    "35-44" = (distribution[[metric]][distribution$age == "35-39"] +
                 distribution[[metric]][distribution$age == "40-44"]) / 2,
    "45-54" = (distribution[[metric]][distribution$age == "45-49"] +
                 distribution[[metric]][distribution$age == "50-54"]) / 2,
    "55-64" = (distribution[[metric]][distribution$age == "55-59"] +
                 distribution[[metric]][distribution$age == "60-64"]) / 2,
    "65-74" = (distribution[[metric]][distribution$age == "65-69"] +
                 distribution[[metric]][distribution$age == "70-74"]) / 2,
    "75-84" = (distribution[[metric]][distribution$age == "75-79"] +
                 distribution[[metric]][distribution$age == ">80"]) / 2,
    ">85" = distribution[[metric]][distribution$age == ">80"]
  )
  
  adjusted_values
}

# Specific functions for IFR and IHR by calling the generalized adjust_categories
adjust_ifr_categories <- function(ifr_distribution) {
  adjust_categories(ifr_distribution, "ifr")
}

adjust_ihr_categories <- function(ihr_distribution) {
  adjust_categories(ihr_distribution, "ihr")
}

# Specific functions for IFR and IHR by calling the generalized adjust_categories
adjust_ifr_categories_child <- function(ifr_distribution) {
  adjust_categories_child(ifr_distribution, "ifr")
}

adjust_ihr_categories_child <- function(ihr_distribution) {
  adjust_categories_child(ihr_distribution, "ihr")
}

# Define functions to estimate hosps/deaths ----

## Deterministic functions (using these) ----

# define function to generate death estimates

estimate_deaths <- function(cases, age_dist, adjusted_ifr) {
  
  # Calculate estimated number of cases and deaths for each age group
  age_dist$cases_by_age <- age_dist$proportion * cases
  age_dist$deaths_by_age <- age_dist$cases_by_age * adjusted_ifr
  
  # Total number of deaths
  sum(age_dist$deaths_by_age)
  
}

# define function to generate death estimates

estimate_hosps <- function(cases, age_dist, adjusted_ihr) {
  
  # Calculate estimated number of cases and deaths for each age group
  age_dist$cases_by_age <- age_dist$proportion * cases
  age_dist$hosp_by_age <- age_dist$cases_by_age * adjusted_ihr
  
  # Total number of deaths
  sum(age_dist$hosp_by_age)
  
}

# define function to generate YLLs

estimate_ylls <- function(cases, age_dist, adjusted_ifr, life_expectancy) {
  
  # if(length(adjusted_ifr) < length(age_dist$age)) {
  #   warning("Length of IFR is shorter than age distribution. Shorter vector will be recycled.")
  #   # Note: this warning is expected as age contains both sexes, whereas ifr
  #   # vector is a single value for each age-group (no sex disaggregation)
  # }
    
  # Calculate estimated number of cases and deaths for each age group
  age_dist$cases_by_age <- age_dist$proportion * cases
  # age_dist$deaths_by_age <- round(age_dist$cases_by_age * adjusted_ifr)
  age_dist$deaths_by_age <- age_dist$cases_by_age * adjusted_ifr # Note
  
  # Years of life lost (yll)
  # age_dist$ylls <- (life_expectancy - age_dist$average_age) * age_dist$deaths_by_age
  age_dist$ylls <- life_expectancy * age_dist$deaths_by_age
  
  # age_dist$ylls[age_dist$ylls<0] <- 0 # Remove negative ylls for older age groups
  
  # Total number of YLLs
  sum(age_dist$ylls)
  
}

## Stochastic functions (not using these) ----

# # define function to generate death estimates
# 
# estimate_deaths <- function(cases, age_dist, adjusted_ifr) {
#   
#   # Calculate estimated number of cases and deaths for each age group
#   cases_by_age <- rmultinom(1, size = cases, prob = age_dist$proportion)
#   deaths_by_age <- rbinom(length(cases_by_age),
#                           size = cases_by_age, prob = adjusted_ifr)
#   
#   # Total number of deaths
#   sum(deaths_by_age)
#   
# }
# 
# # define function to generate death estimates
# 
# estimate_hosps <- function(cases, age_dist, adjusted_ihr) {
#   
#   # Calculate estimated number of cases and hosps for each age group
#   cases_by_age <- rmultinom(1, size = cases, prob = age_dist$proportion)
#   hosps_by_age <- rbinom(length(cases_by_age),
#                          size = cases_by_age, prob = adjusted_ihr)
#   
#   # Total number of hospitalisations
#   sum(hosps_by_age)
#   
# }

# Function to compute averted outcomes ----

compute_averted_outcomes <- function(baseline, scenario_summaries,
                                     pairing_variable) {
  
  # Join the baseline data with the rest of the dataset
  scenario_summaries_with_baseline <- scenario_summaries %>%
    left_join(baseline, by = c(pairing_variable,
                               "seed_country", "movement_input", "pathogen"))
  
  # Subtract the baseline values from the corresponding columns for each testing_rate != 0
  scenario_summaries_with_baseline <- scenario_summaries_with_baseline %>%
    mutate(across(all_of(columns_to_compare),
                  ~ get(paste0(cur_column(), "_baseline")) - ., # Subtract from baseline
                  .names = "{.col}_diff")) # Store the result in new columns with "_diff" suffix
  
  # Test the subtraction was applied correctly
  test_that("Case differences are computed correctly by generic code above",
            expect_equal(scenario_summaries_with_baseline$cases_foreign_baseline -
                           scenario_summaries_with_baseline$cases_foreign,
                         scenario_summaries_with_baseline$cases_foreign_diff))
  test_that("Hosp differences are computed correctly by generic code above",
            expect_equal(scenario_summaries_with_baseline$hosps_foreign_baseline -
                           scenario_summaries_with_baseline$hosps_foreign,
                         scenario_summaries_with_baseline$hosps_foreign_diff))
  test_that("Death differences are computed correctly by generic code above",
            expect_equal(scenario_summaries_with_baseline$deaths_domestic_baseline -
                           scenario_summaries_with_baseline$deaths_domestic,
                         scenario_summaries_with_baseline$deaths_domestic_diff))
  
  # Keep only the averted columns
  
  averted_cols <- paste0(columns_to_compare, "_diff")
  
  scenario_summaries_with_averts <- scenario_summaries_with_baseline %>%
    filter(testing_rate != 0) %>% 
    select(scenario, sim, pathogen, testing_rate, movement_input, seed_country,
           all_of(averted_cols))
  
}

compute_averted_outcomes_ratio <- function(baseline, scenario_summaries,
                                     pairing_variable) {
  
  # Join the baseline data with the rest of the dataset
  scenario_summaries_with_baseline <- scenario_summaries %>%
    left_join(baseline, by = c(pairing_variable,
                               "seed_country", "movement_input", "pathogen"))
  
  # Subtract the baseline values from the corresponding columns for each testing_rate != 0
  scenario_summaries_with_baseline <- scenario_summaries_with_baseline %>%
    mutate(across(all_of(columns_to_compare),
                  ~ get(paste0(cur_column(), "_baseline")) - ., # Subtract from baseline
                  .names = "{.col}_diff")) # Store the result in new columns with "_diff" suffix
  
  # Test the subtraction was applied correctly
  test_that("Case differences are computed correctly by generic code above",
            expect_equal(scenario_summaries_with_baseline$cases_foreign_baseline -
                           scenario_summaries_with_baseline$cases_foreign,
                         scenario_summaries_with_baseline$cases_foreign_diff))
  test_that("Hosp differences are computed correctly by generic code above",
            expect_equal(scenario_summaries_with_baseline$hosps_foreign_baseline -
                           scenario_summaries_with_baseline$hosps_foreign,
                         scenario_summaries_with_baseline$hosps_foreign_diff))
  test_that("Death differences are computed correctly by generic code above",
            expect_equal(scenario_summaries_with_baseline$deaths_domestic_baseline -
                           scenario_summaries_with_baseline$deaths_domestic,
                         scenario_summaries_with_baseline$deaths_domestic_diff))
  
  # Optionally filter out rows where testing_rate == 0
  scenario_summaries_with_diff <- scenario_summaries_with_baseline %>%
    filter(testing_rate != 0)
  
  # Find cases/hosps/deaths averted rel to baseline (testing_rate == 0)
  scenario_summaries_with_diff <- scenario_summaries_with_diff %>%
    mutate(across(
      all_of(columns_to_compare),
      # .fns = ~ get(paste0(cur_column(), "_diff")) / get(paste0(cur_column(), "_baseline")),
      .fns = ~ get(cur_column()) / get(paste0(cur_column(), "_baseline")), # gives ratio
      .names = "{.col}_averted"
    ))
  
  # Keep only the averted columns
  
  averted_cols <- paste0(columns_to_compare, "_averted")
  
  scenario_summaries_with_averts <- scenario_summaries_with_diff %>%
    select(scenario, sim, pathogen, testing_rate, movement_input, seed_country,
           all_of(averted_cols))
  
}

# Function to get the quantiles of averted outcomes ----

averted_outcomes_qntls <- function(averted_outcome_df) {
  
  # There are some -Inf averted values (occur when baseline deaths are 0, and
  # control scenario deaths are >0). Convert these to NA
  
  is.na(averted_outcome_df) <- sapply(
    averted_outcome_df, is.infinite)
  
  # Now get quantiles and format
  
  scenario_summaries_with_averts_qntls <- averted_outcome_df %>%
    select(-sim) %>% 
    group_by(pathogen, testing_rate, movement_input, seed_country) %>%
    summarise(across(where(is.numeric),
                     list(median = ~median(., na.rm = TRUE),
                          quantile_2.5 = ~quantile(., 0.025, na.rm = TRUE),
                          quantile_97.5 = ~quantile(., 0.975, na.rm = TRUE),
                          quantile_25 = ~quantile(., 0.25, na.rm = TRUE),
                          quantile_75 = ~quantile(., 0.75, na.rm = TRUE),
                          mean = ~mean(., na.rm = TRUE),
                          sd = ~sd(., na.rm = TRUE)),
                     .names = "{col}-{fn}")) %>%
    pivot_longer(cols = -c(pathogen, testing_rate, movement_input, seed_country),
                 names_to = c("variable", "statistic"),
                 names_sep = "-",
                 values_to = "value") %>%
    pivot_wider(names_from = statistic, values_from = value)
  
  # Round and combine median, quantile_2.5, and quantile_97.5 into a single column
  
  scenario_summaries_with_averts_qntls <-
    scenario_summaries_with_averts_qntls %>%
    mutate(
      across(c(median, quantile_2.5, quantile_97.5, quantile_25, quantile_75),
             ~ round(., 3))
    )
  
  scenario_summaries_with_averts_qntls <- scenario_summaries_with_averts_qntls %>%
    mutate(value_str_95 = sprintf("%s (%s; %s)",
                                  comma(round(median, 3), accuracy = 0.001),
                                  comma(round(quantile_2.5, 3), accuracy = 0.001),
                                  comma(round(quantile_97.5, 3), accuracy = 0.001)
    )) %>%
    mutate(value_str_iqr = sprintf("%s (%s; %s)",
                                   comma(round(median, 3), accuracy = 0.001),
                                   comma(round(quantile_25, 3), accuracy = 0.001),
                                   comma(round(quantile_75, 3), accuracy = 0.001)
    )) 
  
}
