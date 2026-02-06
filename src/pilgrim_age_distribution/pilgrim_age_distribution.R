# setwd("C:/Users/jw2519/OneDrive - Imperial College London/Hajj project/Flight model/hajj_project/src/pilgrim_age_distribution")

orderly2::orderly_parameters(age_data_source = NULL)
# Use "azarpazhooh_et_al" as preliminary entry here:
# https://doi.org/10.1186/1471-2377-13-193

age_data_file <- paste0("age_distribution_", age_data_source, ".csv")

orderly2::orderly_resource(age_data_file)
# orderly2::orderly_resource("age_distribution_azarpazhooh_et_al.csv")
orderly2::orderly_resource("age_distribution_ksa_population.csv") # from https://population.un.org/dataportal/data/indicators/46/locations/682/start/2023/end/2023/table/pivotbyage?df=f3bdb140-abb9-46ab-84c1-31d8a3d1b46e

orderly2::orderly_artefact(description = "Age distributions",
                           files = c("pilgrim_age_distribution.rds",
                                     "ksa_age_dist_incl_children.rds",
                                     "ksa_age_dist_excl_children.rds")
                           )

library(dplyr)

# Pilgrim age data ----

age_distribution <- read.csv(age_data_file)

if(!"proportion" %in% colnames(age_distribution)) {
  age_distribution$proportion <- age_distribution$population / sum(age_distribution$population)
}

saveRDS(age_distribution, "pilgrim_age_distribution.rds")


# KSA population age data ----

ksa_age_distribution <- read.csv("age_distribution_ksa_population.csv") 

# Create a new column that combines age groups
ksa_age_distribution <- ksa_age_distribution %>%
  mutate(new_age_group = case_when(
    age %in% c("F0-4", "F5-9", "F10-14") ~ "F<15",
    age %in% c("F15-19", "F20-24") ~ "F15-24",
    age %in% c("F25-29", "F30-34") ~ "F25-34",
    age %in% c("F35-39", "F40-44") ~ "F35-44",
    age %in% c("F45-49", "F50-54") ~ "F45-54",
    age %in% c("F55-59", "F60-64") ~ "F55-64",
    age %in% c("F65-69", "F70-74") ~ "F65-74",
    age %in% c("F75-79", "F80-84") ~ "F75-84",
    age == "F>85" ~ "F>85",
    age %in% c("M0-4", "M5-9", "M10-14") ~ "M<15",
    age %in% c("M15-19", "M20-24") ~ "M15-24",
    age %in% c("M25-29", "M30-34") ~ "M25-34",
    age %in% c("M35-39", "M40-44") ~ "M35-44",
    age %in% c("M45-49", "M50-54") ~ "M45-54",
    age %in% c("M55-59", "M60-64") ~ "M55-64",
    age %in% c("M65-69", "M70-74") ~ "M65-74",
    age %in% c("M75-79", "M80-84") ~ "M75-84",
    age == "M>85" ~ "M>85"
  )) %>%
  
  # Group by the new age groups and sum the population
  group_by(new_age_group) %>%
  summarise(
    population = sum(population)
  ) %>% 
  rename(age = new_age_group)

ksa_age_distribution$age <- factor(ksa_age_distribution$age,
                                   levels = c("F<15", "F15-24", "F25-34", "F35-44",
                                              "F45-54", "F55-64", "F65-74", "F75-84",
                                              "F>85", "M<15", "M15-24", "M25-34",
                                              "M35-44", "M45-54", "M55-64", "M65-74",
                                              "M75-84", "M>85"))

ksa_age_distribution <- arrange(ksa_age_distribution, age)


# Create two versions of ksa_age_distribution
# One with children included (use this for the whole population of KSA)
# One with children excluded (use this for the domestic pilgrims)
# Add a proportion column to each

# Incl. children
ksa_age_dist_incl_children <- ksa_age_distribution

ksa_age_dist_incl_children$proportion <- ksa_age_dist_incl_children$population /
  sum(ksa_age_dist_incl_children$population)

saveRDS(ksa_age_dist_incl_children, "ksa_age_dist_incl_children.rds")

# Excl. children
ksa_age_dist_excl_children <- filter(ksa_age_distribution,
                                     !(age %in% c("F<15", "M<15")))

ksa_age_dist_excl_children$proportion <- ksa_age_dist_excl_children$population /
  sum(ksa_age_dist_excl_children$population)

saveRDS(ksa_age_dist_excl_children, "ksa_age_dist_excl_children.rds")
