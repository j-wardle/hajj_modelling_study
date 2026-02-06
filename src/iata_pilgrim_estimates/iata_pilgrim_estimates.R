# setwd("C:/Users/jw2519/OneDrive - Imperial College London/Hajj project/Flight model/hajj_project/src/iata_pilgrim_estimates")

# Hajj pilgrim numbers from IATA:
orderly2::orderly_dependency(
  "get_flight_data",
  "latest()",
  "ksa_iata_data.rds")

# Hajj pilgrim numbers from 2008 (from KSA MoH): #csv created from Table 1 https://academic.oup.com/jtm/article/17/2/75/1800622?login=true#27427044
# Used to adjust pilgrim numbers so that we have the same number of pilgrims in both
orderly2::orderly_resource("pilgrim_numbers_2008_khan_et_al_jtm.csv")


orderly2::orderly_artefact("Number of pilgrims", c("compare_proportions.rds",
                                                   "arrival_numbers_for_sim.rds",
                                                   "departure_numbers_for_sim.rds")
)

library(tidyverse)
library(testthat)

iata_pilgrim_estimate <- readRDS("ksa_iata_data.rds")
moh_2008_pilgrim_numbers <- read.csv("pilgrim_numbers_2008_khan_et_al_jtm.csv")

# A tiny bit of cleaning of country names

iata_pilgrim_estimate <- iata_pilgrim_estimate %>%
  dplyr::mutate(orig_country = dplyr::case_when(
    orig_country == "Hong Kong (SAR), China" ~ "Hong Kong",
    orig_country == "Chinese Taipei" ~ "Taiwan",
    TRUE ~ orig_country
  )) %>%
  dplyr::mutate(dest_country = dplyr::case_when(
    dest_country == "Hong Kong (SAR), China" ~ "Hong Kong",
    dest_country == "Chinese Taipei" ~ "Taiwan",
    TRUE ~ dest_country
  ))

iata_pilgrim_estimate <- iata_pilgrim_estimate %>%
  dplyr::mutate(orig_country = dplyr::case_when(
    orig_country == "Grenada and South Grenadines" ~ "Grenada",
    orig_country == "Guinea Bissau" ~ "Guinea-Bissau",
    orig_country == "Ivory Coast (Cote d'Ivoire)" ~ "Cote d'Ivoire",
    orig_country == "Macedonia" ~ "North Macedonia",
    orig_country == "Swaziland" ~ "Eswatini",
    TRUE ~ orig_country
  )) %>%
  dplyr::mutate(dest_country = dplyr::case_when(
    dest_country == "Grenada and South Grenadines" ~ "Grenada",
    dest_country == "Guinea Bissau" ~ "Guinea-Bissau",
    dest_country == "Ivory Coast (Cote d'Ivoire)" ~ "Cote d'Ivoire",
    dest_country == "Macedonia" ~ "North Macedonia",
    dest_country == "Swaziland" ~ "Eswatini",
    TRUE ~ dest_country
  ))

# Remove internal KSA flights

iata_pilgrim_estimate <- iata_pilgrim_estimate %>%
  filter(orig_country != "Saudi Arabia")

# What is the maximum number of pilgrims we might estimate from the IATA data?
# Let's make some conservative assumptions here
# Assume that the minimum baseline 'non-Hajj' arrivals is equal to the minimum monthly arrivals in KSA for the analysis period
# This is Jan 2012 when there were 915020 arrivals
min_baseline_arrivals <- 915020
# Assume that the maximum pilgrim attendance occurred in the month with the highest monthly arrivals
# This is August 2018 when there were 2037460 arrivals
# Note: we could have conditioned this on the month needing to include the Hajj, or be the month prior, but this was not necessary - Hajj 2018 was Aug 19-24)
max_hajj_arrivals <- 2037460
# From these 'extreme' assumptions, the largest estimate we could get of Hajj foreign pilgrims based on IATA data is:
max_iata_pilgrim_estimate <- max_hajj_arrivals - min_baseline_arrivals

# Filter for the specified years and months
df_2012 <- iata_pilgrim_estimate %>% filter(year == '2012' & month == '01')
df_2018 <- iata_pilgrim_estimate %>% filter(year == '2018' & month == '08')

# Join the two dataframes on orig_country and dest_country
df_merged <- df_2012 %>%
  inner_join(df_2018, by = c("orig_country", "dest_country"), suffix = c("_2012", "_2018"))

# Calculate pilgrim_estimate
df_merged <- df_merged %>%
  mutate(pilgrim_estimate = passengers_2018 - passengers_2012)

# Select the desired columns for the new dataframe
result_df <- df_merged %>%
  select(orig_country, pilgrim_estimate)


hajj_quotas <- result_df

# Replace any negative pilgrim numbers with zeros
hajj_quotas <- hajj_quotas %>%
  dplyr::mutate(pilgrim_estimate = dplyr::case_when(
    pilgrim_estimate < 0 ~ 0,
    TRUE ~ pilgrim_estimate
  ))

# Adjust pilgrim numbers
# We are adjusting the numbers here so that the total pilgrims is the same in both the iata-derived and moh-derived estimates
adjust_factor <- sum(moh_2008_pilgrim_numbers$number_of_pilgrims) /
  sum(hajj_quotas$pilgrim_estimate)

hajj_quotas$pilgrim_estimate <- round(hajj_quotas$pilgrim_estimate * adjust_factor)

test_that("Foreign pilgrim #s are within 0.1% of each other", {
  value1 <- sum(moh_2008_pilgrim_numbers$number_of_pilgrims)
  value2 <- sum(hajj_quotas$pilgrim_estimate)
  
  # Calculate the relative difference
  relative_difference <- abs(value1 - value2) / abs(value1)
  
  # Test if the relative difference is within 0.1% (0.001)
  expect_true(relative_difference <= 0.001, 
              info = paste("Relative difference is", relative_difference, 
                           "- exceeds 0.1%"))
})


# Create a version of hajj_quotas that can be used to compare proportion
# of foreign pilgrims arriving from each origin country

compare_proportions <- hajj_quotas %>% 
  ungroup() %>% 
  mutate(total_pilgrims = sum(pilgrim_estimate),                    
         proportion = pilgrim_estimate / total_pilgrims)

compare_proportions$method <- "iata_method1"

saveRDS(compare_proportions, "compare_proportions.rds")

# # For development, let's focus on three countries only, plus domestic pilgrims
# hajj_quotas <- hajj_quotas[1:3,1:2]
# hajj_quotas[nrow(hajj_quotas) + 1,] <- c("KSA", 634379) # Number taken from 2019 domestic pilgrims statistic (https://www.stats.gov.sa/sites/default/files/haj_40_en.pdf)
# hajj_quotas$dest_country <- "KSA"
# colnames(hajj_quotas)[1] <- "orig_country"
# colnames(hajj_quotas)[2] <- "pilgrims"
# hajj_quotas$pilgrims <- as.numeric(hajj_quotas$pilgrims)

# Let's extend this to the full set of countries in the quota list
hajj_quotas <- hajj_quotas[,1:2]
hajj_quotas[nrow(hajj_quotas) + 1,] <- list("KSA", 679008) # Number extracted from 2016 statistics document, pg 15 (2008 Hajj was 1429 H) (https://www.stats.gov.sa/sites/default/files/hajj_1437_en_0.pdf)
hajj_quotas$dest_country <- "KSA"
colnames(hajj_quotas)[1] <- "orig_country"
colnames(hajj_quotas)[2] <- "pilgrims"
hajj_quotas$pilgrims <- as.numeric(hajj_quotas$pilgrims)


# Create all combinations of the countries
hajj_allcombs <- expand_grid(hajj_quotas$orig_country, hajj_quotas$orig_country)

# Rename the columns in the combinations dataframe
colnames(hajj_allcombs) <- c("orig_country", "dest_country")

# join passenger numbers to the all combs
# joined <-
#   left_join(hajj_allcombs, hajj_quotas, by = c("orig_country", "dest_country")) %>%
#   mutate(pilgrims = replace_na(pilgrims, 0)) %>%
#   mutate(dest_country = ifelse(dest_country == "KSA",
#                                paste0("KSA_", orig_country),
#                                dest_country)) %>%
#     mutate(orig_country = ifelse(orig_country == "KSA",
#                                  paste0("KSA_", dest_country),
#                                  orig_country)) %>%
#     mutate(orig_country = str_replace(orig_country, "KSA_KSA_KSA", "KSA_KSA"))


joined <-
  left_join(hajj_allcombs, hajj_quotas, by = c("orig_country", "dest_country")) %>%
  mutate(pilgrims = replace_na(pilgrims, 0)) %>%
  mutate(dest_country = ifelse(dest_country == "KSA" & orig_country != "KSA",
                               paste0("KSA_", orig_country),
                               dest_country)) %>%
  mutate(orig_country = ifelse(orig_country == "KSA" & dest_country != "KSA",
                               paste0("KSA_", dest_country),
                               orig_country)) %>%
  mutate(dest_country = ifelse(dest_country == "KSA",
                               "KSA_KSA",
                               dest_country))

joined[nrow(joined) + 1,] <- list("KSA_KSA", "KSA", 0)

# Create sub-patch for KSA non-pilgrims who are at risk (i.e. increased contact with pilgrims vs general population)
joined[nrow(joined) + 1,] <- list("KSA_KSA", "KSA_AtRisk", 0)
joined[nrow(joined) + 1,] <- list("KSA_AtRisk", "KSA_KSA", 0)

probability_matrix <- joined %>%
  arrange(orig_country, dest_country)

out <-
  joined %>%
  pivot_wider(id_cols = orig_country,
              names_from = dest_country,
              values_from = pilgrims) %>%
  select(orig_country, order(colnames(.))) %>%
  arrange(orig_country, .locale = "en") %>%
  replace(is.na(.), 0)

out <- as.matrix(out[,-1])
rownames(out) <- colnames(out)

# Define a matrix for travel to KSA, and one for return from KSA.
travel_to_ksa <- out
travel_from_ksa <- t(out) # transpose of matrix, reflects along diagonal

# TESTS ----
test_that("Column names match rownames",
          expect_equal(rownames(travel_to_ksa), colnames(travel_to_ksa)))

test_that("Transposing of matrices seems correct",
          expect_equal(travel_to_ksa["United Kingdom", "KSA_United Kingdom"],
                       travel_from_ksa["KSA_United Kingdom", "United Kingdom"]))



# When do pilgrims arrive? For now we will assume uniform numbers per day
# but we can also use table on pg 42 here (https://www.stats.gov.sa/sites/default/files/haj_40_en.pdf) to distribute the arrivals in more clever way.
arrival_period <- 30
departure_period <- 30 # assume it is the same as arrival period. Not sure if this is case. We don't have data.


daily_travel_to_ksa <- round(travel_to_ksa / arrival_period)
daily_travel_from_ksa <- round(travel_from_ksa / departure_period)

saveRDS(daily_travel_to_ksa, "arrival_numbers_for_sim.rds")
saveRDS(daily_travel_from_ksa, "departure_numbers_for_sim.rds")
