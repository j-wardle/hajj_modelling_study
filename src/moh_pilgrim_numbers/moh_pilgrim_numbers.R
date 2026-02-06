orderly2::orderly_strict_mode()

# Hajj pilgrim numbers from 2008 (from KSA MoH): #csv created from Table 1 https://academic.oup.com/jtm/article/17/2/75/1800622?login=true#27427044
orderly2::orderly_resource(c("pilgrim_numbers_2008_khan_et_al_jtm.csv",
                             "muslim_populations_by_country.csv"))

# Hajj pilgrim numbers from IATA (for consistent naming of countries)
orderly2::orderly_dependency(
  "get_flight_data",
  "latest()",
  c(ksa_iata_data.rds = "ksa_iata_data.rds"))

orderly2::orderly_artefact("Number of pilgrims", c("compare_proportions.rds",
                                                   "arrival_numbers_for_sim.rds",
                                                   "departure_numbers_for_sim.rds")
)

library(tidyverse)
library(testthat)
library(janitor)

moh_2008_pilgrim_numbers <- read.csv("pilgrim_numbers_2008_khan_et_al_jtm.csv")
ksa_iata_data <- readRDS("ksa_iata_data.rds")
muslim_populations <- read.csv("muslim_populations_by_country.csv") # source: https://www.pewresearch.org/chart/interactive-data-table-world-muslim-population-by-country/

muslim_populations <- janitor::clean_names(muslim_populations)

muslim_populations <- muslim_populations %>%
  select(country, estimated_2009_muslim_population) %>%
  mutate(country = str_replace_all(country, "\\*", ""), # remove asterisk from end of some country names
         estimated_2009_muslim_population = replace_na(estimated_2009_muslim_population, 0))

# check if any cleaning of country names needed
moh_2008_pilgrim_numbers$country[!(moh_2008_pilgrim_numbers$country %in% muslim_populations$country)]
# The country names in moh_2008_pilgrim_numbers are also included in the muslim_populations df

# check moh country names against iata country names
moh_2008_pilgrim_numbers$country[!(moh_2008_pilgrim_numbers$country %in% ksa_iata_data$orig_country)]

moh_2008_pilgrim_numbers <- moh_2008_pilgrim_numbers %>%
  mutate(country = if_else(country == "Russia", "Russian Federation", country))

# check muslim_populations country names against iata country names
muslim_populations$country[!(muslim_populations$country %in% ksa_iata_data$orig_country)]
# manually clean some names that are worded/spelt slightly differently
# some names in muslim_populations do not feature in iata dataset of flights to Saudi Arabia.
# That's fine - we can keep those as they are

muslim_populations <- muslim_populations %>%
  mutate(country = if_else(country == "Bosnia-Herzegovina", "Bosnia and Herzegovina", country),
         country = if_else(country == "Brunei", "Brunei Darussalam", country),
         country = if_else(country == "Burma (Myanmar)", "Myanmar", country),
         country = if_else(country == "Grenada", "Grenada and South Grenadines", country),
         country = if_else(country == "Guinea Bissau", "Guinea-Bissau", country),
         country = if_else(country == "Ivory Coast", "Cote d'Ivoire", country),
         country = if_else(country == "Congo", "Democratic Republic of the Congo", country),
         country = if_else(country == "Republic of Congo", "Congo", country),
         country = if_else(country == "Republic of Macedonia", "North Macedonia", country),
         country = if_else(country == "Russia", "Russian Federation", country),
         country = if_else(country == "Samoa", "Western Samoa", country),
         country = if_else(country == "Saudi Arabia", "KSA", country),
         country = if_else(country == "St. Kitts and Nevis", "Saint Kitts and Nevis", country),
         country = if_else(country == "St. Lucia", "Saint Lucia", country),
         country = if_else(country == "St. Vincent and the Grenadines", "Saint Vincent and Grenadines", country),
         country = if_else(country == "Swaziland", "Eswatini", country),
         country = if_else(country == "Timor-Leste", "East Timor", country)
         )

muslim_populations <- muslim_populations %>% 
  filter(country != "KSA")

# Now distribute 'Others' from moh_2008_pilgrim_estimate according to proportions in muslim_populations
number_to_distribute <- moh_2008_pilgrim_numbers$number_of_pilgrims[moh_2008_pilgrim_numbers$country == "Others"]

distribute_others <- muslim_populations %>%
  filter(!(country %in% moh_2008_pilgrim_numbers$country)) %>%
  mutate(proportion_of_global_muslims = estimated_2009_muslim_population / sum(estimated_2009_muslim_population)) %>%
  mutate(distributed_value = round(number_to_distribute * proportion_of_global_muslims))

distributed_values_for_binding <- distribute_others %>%
  rename(number_of_pilgrims = distributed_value) %>%
  select(country, number_of_pilgrims)

hajj_quotas <- bind_rows(moh_2008_pilgrim_numbers, distributed_values_for_binding) %>%
  filter(country != "Others" & number_of_pilgrims > 0)

# Create a version of hajj_quotas that can be used to compare proportion
# of foreign pilgrims arriving from each origin country

compare_proportions <- hajj_quotas %>% 
  mutate(total_pilgrims = sum(number_of_pilgrims),                    
         proportion = number_of_pilgrims / total_pilgrims)

compare_proportions$method <- "moh"

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
hajj_quotas[nrow(hajj_quotas) + 1,] <- c("KSA", 679008) # Number extracted from 2016 statistics document, pg 15 (2008 Hajj was 1429 H) (https://www.stats.gov.sa/sites/default/files/hajj_1437_en_0.pdf)
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