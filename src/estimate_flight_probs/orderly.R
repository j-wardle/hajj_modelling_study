orderly2::orderly_strict_mode()

orderly2::orderly_parameters(analysis_years_start = NULL, analysis_years_end = NULL)

# Note this requires a download of the IATA data as downloaded from get_flight_data
orderly2::orderly_resource(c("Population-EstimatesData.csv", "iata_data.rds"))

# orderly2::orderly_dependency("get_flight_data", "latest", c(iata_data.rds = "iata_data.rds"))

orderly2::orderly_artefact(
  "Probability matrix",
  c("probs_for_sim.rds", "pops_for_sim.rds"))

library(dplyr)
library(stringr)
library(forcats)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)

iata_data <- readRDS("iata_data.rds")
analysis_period <- seq(analysis_years_start, analysis_years_end, by = 1)

# Some discrepancies in naming between previously downloaded IATA datasets and newer downloads

iata_data <- iata_data %>%
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

# Manually clean names of countries for consistency with population dataset

iata_data <- iata_data %>%
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

# Load and clean the population data (source: World Bank)

population_data <- readr::read_csv("Population-EstimatesData.csv") %>%
  dplyr::filter(`Indicator Name` == "Population, total") %>%
  dplyr::slice(-(1:49)) %>%
  janitor::clean_names()

colnames(population_data)[5:ncol(population_data)] <- stringr::str_sub(
  colnames(population_data[5:ncol(population_data)]), 2)

population_data <- population_data %>%
  tidyr::pivot_longer(cols = '1960':'2050',
                      names_to = "year",
                      values_to = "population")

population_data$year<- as.numeric(population_data$year)
population_data <- population_data %>%
  dplyr::filter(year < 2022 & year > 2010) %>%
  dplyr::select(-c(country_code, indicator_name, indicator_code))

# Eritrea has missing population sizes for 2012-2020. Repeat the 2011 value
# Bermuda and xxx have missing population for 2021. Repeat 2020 value

population_data <- population_data %>%
  dplyr::mutate(population = replace(population, country_name == "Eritrea", 3213969),
                population = replace(population, country_name == "Bermuda" & year == 2021, 63903),
                population = replace(population, country_name == "Greenland" & year == 2021, 56367))

# Change some country names to be consistent with naming in IATA data

population_data <- population_data %>%
  dplyr::mutate(country_name = dplyr::case_when(
    str_detect(country_name, "Bahamas, The") ~ "Bahamas",
    str_detect(country_name, "Cabo Verde") ~ "Cape Verde",
    str_detect(country_name, "Congo, Dem. Rep.") ~ "Democratic Republic of the Congo",
    str_detect(country_name, "Congo, Rep.") ~ "Congo",
    str_detect(country_name, "Egypt, Arab Rep.") ~ "Egypt",
    str_detect(country_name, "Gambia, The") ~ "Gambia",
    str_detect(country_name, "Hong Kong SAR, China") ~ "Hong Kong",
    str_detect(country_name, "Iran, Islamic Rep.") ~ "Iran",
    str_detect(country_name, "Korea, Dem. People's Rep.") ~ "South Korea",
    str_detect(country_name, "Korea, Rep.") ~ "North Korea",
    str_detect(country_name, "Kyrgyz Republic") ~ "Kyrgyzstan",
    str_detect(country_name, "Lao PDR") ~ "Laos",
    str_detect(country_name, "Macao SAR, China") ~ "Macau",
    str_detect(country_name, "Micronesia, Fed. Sts.") ~ "Micronesia",
    # str_detect(country_name, "Sint Maarten (Dutch part)") ~ "Sint Maarten",
    str_detect(country_name, "Slovak Republic") ~ "Slovakia",
    str_detect(country_name, "St. Kitts and Nevis") ~ "Saint Kitts and Nevis",
    str_detect(country_name, "St. Lucia") ~ "Saint Lucia",
    str_detect(country_name, "St. Vincent and the Grenadines") ~ "Saint Vincent and Grenadines",
    str_detect(country_name, "Syrian Arab Republic") ~ "Syria",
    str_detect(country_name, "Venezuela, RB") ~ "Venezuela",
    str_detect(country_name, "Yemen, Rep.") ~ "Yemen",

    TRUE ~ country_name
  ))


# check for differences between the countries featured in the two datasets
a <- unique(population_data$country_name)
b <- unique(iata_data$orig_country)

diff_a <- setdiff(a,b)
diff_b <- setdiff(b,a)
diff_a
diff_b

# for now, can remove these differences from the flights dataset
# v small population sizes so shouldn't affect things much
# exception: Taiwan is largest source of flights not in the World Bank populations

iata_data <- iata_data %>%
  dplyr::filter(!(orig_country %in% diff_a) & !(orig_country %in% diff_b)) %>%
  dplyr::mutate(passengers = replace(passengers, orig_country == dest_country, 0))

iata_data$year <- as.numeric(iata_data$year)

iata_data <- dplyr::filter(iata_data, year %in% analysis_period)

# create all possible origin-destination journeys
iata_allcombs <- expand_grid(orig_country = unique(iata_data$orig_country),
                             dest_country = unique(iata_data$dest_country))

iata_allcombs <- iata_allcombs %>%
  filter(dest_country %in% unique(orig_country)) %>%
  filter(orig_country != "Nauru" & dest_country != "Nauru")

x <- iata_allcombs$orig_country
y <- iata_allcombs$dest_country
repeats <- length(analysis_period) * 12

z <- data.frame(dummy = rep(NA, times = length(x)*repeats))
z$orig_country <- rep(x, times = repeats)
z$dest_country <- rep(y, times = repeats)
z$year <- rep(analysis_period, each = length(x)*12)
z$month <- rep(rep(unique(iata_data$month), each = length(x)),
               times = length(analysis_period))

# join passenger numbers to the all combs
joined <- left_join(z, iata_data, by = c("orig_country", "dest_country", "year", "month")) %>%
  select(-dummy) %>%
  mutate(passengers = replace_na(passengers, 0))


# join flight and population data

passenger_flows <- dplyr::inner_join(joined, population_data, by = c("orig_country" = "country_name",
                                                                   "year" = "year"))


# estimate probabilities of flying

fly_probs <- passenger_flows %>%
  dplyr::group_by(year, month, orig_country) %>%
  dplyr::mutate(total_departures = sum(passengers)) %>%
  dplyr::summarise(total_departures = mean(total_departures),
                   population = mean(population)) %>%
  dplyr::mutate(p_fly = total_departures / population) %>%
  dplyr::mutate(p_stay = (1 - p_fly))

fly_probs <- fly_probs %>%
  dplyr::mutate(days_in_month = dplyr::case_when(
    month == "01" ~ 31,
    year %% 4 == 0 & month == "02" ~ 29,
    year %% 4 != 0 & month == "02" ~ 28,
    month == "03" ~ 31,
    month == "04" ~ 30,
    month == "05" ~ 31,
    month == "06" ~ 30,
    month == "07" ~ 31,
    month == "08" ~ 31,
    month == "09" ~ 30,
    month == "10" ~ 31,
    month == "11" ~ 30,
    month == "12" ~ 31
  )) %>%
  dplyr::mutate(p_fly_per_day = p_fly / days_in_month,
                p_stay_per_day = 1 - p_fly_per_day)


matrix_data <- inner_join(passenger_flows, fly_probs, by = c("orig_country", "year", "month"))

matrix_data <- matrix_data %>%
  group_by(orig_country, year, month) %>%
  dplyr::mutate(passenger_proportion = passengers/sum(passengers))

matrix_data <- matrix_data %>%
  mutate(matrix_prob = ifelse(orig_country == dest_country,
                              p_stay_per_day,
                              p_fly_per_day * passenger_proportion),
         date = paste0(year, "_", month))

probability_matrix <- matrix_data %>%
  select(orig_country, dest_country, matrix_prob, date) %>%
  ungroup() %>%
  arrange(orig_country)

probability_matrix <- split(probability_matrix, f = probability_matrix$date)

probability_matrix <- map(probability_matrix, function(date) {

  out <- date %>%
    select(orig_country, dest_country, matrix_prob) %>%
    pivot_wider(id_cols = orig_country,
                names_from = dest_country,
                values_from = matrix_prob) %>%
    select(orig_country, order(colnames(.))) %>%
    replace(is.na(.), 0)

  out <- as.matrix(out[,-1])
  rownames(out) <- colnames(out)

  out

}
)

saveRDS(probability_matrix, "probs_for_sim.rds")

population_data_for_sim <- population_data %>%
  filter(country_name %in% colnames(probability_matrix[[1]])) %>%
  arrange(country_name)

saveRDS(population_data_for_sim, "pops_for_sim.rds")
