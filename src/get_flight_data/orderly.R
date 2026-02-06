orderly2::orderly_strict_mode()

orderly2::orderly_parameters(folder = NULL, analysis_years_start = NULL, analysis_years_end = NULL)

orderly2::orderly_artefact("Flight data in usable format", "ksa_iata_data.rds")

library(janiata)

analysis_years <- seq(analysis_years_start, analysis_years_end, by = 1)

flight_data <- import_iata_multi_nat(analysis_years, folder)

# saveRDS(flight_data, "iata_data.rds")

# Only keep the KSA flight numbers to keep size smaller
ksa_flight_data <- dplyr::filter(flight_data, dest_country == "Saudi Arabia")

saveRDS(ksa_flight_data, "ksa_iata_data.rds")

# Note: this dataframe has the following 5 columns
# year "character"
# month "character"
# orig_country "character"
# dest_country "character"
# passengers "numeric"
