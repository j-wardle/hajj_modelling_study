# Set task dependencies
orderly2::orderly_dependency(
  "moh_pilgrim_numbers",
  "latest",
  c(compare_proportions_moh.rds = "compare_proportions.rds"))

orderly2::orderly_dependency(
  "iata_pilgrim_estimates",
  "latest",
  c(compare_proportions_iata1.rds = "compare_proportions.rds"))

orderly2::orderly_dependency(
  "iata_pilgrim_estimates_method2",
  "latest",
  c(compare_proportions_iata2.rds = "compare_proportions.rds"))

# Define common figure functions
orderly_shared_resource("utils.R")
source("utils.R")

# Set task artefacts
orderly2::orderly_artefact("Comparison figure", "compare_top10_origins.png")
orderly2::orderly_artefact("Comparison figure (pct version)", "compare_toppct_origins.png")

# Load packages
library(tidyverse)
library(scales)

# Read in pilgrim inputs
compare_proportions_iata1 <- readRDS(
  "compare_proportions_iata1.rds")

compare_proportions_iata2 <- readRDS(
  "compare_proportions_iata2.rds")

compare_proportions_moh <- readRDS(
  "compare_proportions_moh.rds")

# Align column names between datasets
compare_proportions_moh <- compare_proportions_moh %>% 
  rename(orig_country = country,
         pilgrim_estimate = number_of_pilgrims)

# select top 10 orig countries
moh_top10 <- compare_proportions_moh %>% 
  arrange(desc(proportion)) %>% 
  slice_head(n = 10) %>% 
  select(-total_pilgrims)

iata1_top10 <- compare_proportions_iata1 %>% 
  arrange(desc(proportion)) %>% 
  slice_head(n = 10) %>% 
  select(-total_pilgrims)

iata2_top10 <- compare_proportions_iata2 %>% 
  arrange(desc(proportion)) %>% 
  slice_head(n = 10) %>% 
  select(-total_pilgrims)

top10s <- bind_rows(moh_top10, iata1_top10, iata2_top10)

# For plotting purposes, let's ensure each method includes the same set of countries

## Step 1: Identify the full set of top 10 countries
top_countries <- unique(top10s$orig_country)

moh_ranked <- compare_proportions_moh %>% 
  arrange(desc(proportion)) %>%
  mutate(rank = if_else(row_number() <= 10, "Top 10", "Outside top 10")) %>%
  filter(orig_country %in% top_countries) %>% 
  select(-total_pilgrims)

iata1_ranked <- compare_proportions_iata1 %>% 
  arrange(desc(proportion)) %>%
  mutate(rank = if_else(row_number() <= 10, "Top 10", "Outside top 10")) %>%
  filter(orig_country %in% top_countries) %>% 
  select(-total_pilgrims)

iata2_ranked <- compare_proportions_iata2 %>% 
  arrange(desc(proportion)) %>%
  mutate(rank = if_else(row_number() <= 10, "Top 10", "Outside top 10")) %>%
  filter(orig_country %in% top_countries) %>% 
  select(-total_pilgrims)

plottop10s <- bind_rows(moh_ranked, iata1_ranked, iata2_ranked)

# # figure <-
#   ggplot(plottop10s) +
#   geom_bar(aes(x = orig_country, y = pilgrim_estimate,
#                alpha = rank,
#                fill = method,
#                linetype = rank),
#            stat = "identity", position = "dodge", color = "black") +
#   labs(x = "Pilgrim origin country",
#        y = "Number of pilgrims",
#        fill = "Movement input method",
#        alpha = "Country rank",
#        linetype = "Country rank") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_y_continuous(label=comma) +
#   scale_linetype_manual(values = c("Top 10" = "solid", "Outside top 10" = "dashed")) +
#   scale_alpha_manual(values = c("Top 10" = 1, "Outside top 10" = 0.4))
# 
# ggsave("compare_top10_origins.png", figure, bg = "white")


# Reorder countries by pilgrim_estimate for the "moh" method, and apply this order across all methods
plottop10s <- plottop10s %>%
  group_by(orig_country) %>%
  mutate(rank_moh = max(pilgrim_estimate[method == "moh"])) %>%  # Get max value for 'moh' method for each country
  ungroup() %>%
  mutate(orig_country = fct_reorder(orig_country, rank_moh, .desc = FALSE))   # Reorder by 'moh' estimate

plottop10s$method <- factor(plottop10s$method,
                            levels = c("iata_method2", "iata_method1", "moh"),
                            labels = c("IATA w/ no baseline adjustment",
                                       "IATA w/ baseline adjustment",
                                       "MOH"))

plottop10s$rank <- factor(plottop10s$rank,
                          levels = c("Top 10", "Outside top 10"))

figure <- ggplot(plottop10s) +
  geom_bar(aes(x = orig_country, y = proportion,
               fill = method, alpha = rank, 
               linetype = rank, group = method),
           stat = "identity", position = "dodge",
           color = "black", width = 0.7) +
  labs(x = "Pilgrim origin country",
       y = "Proportion of foreign pilgrims\nfrom each origin country",
       fill = "Movement input method",
       alpha = "Country rank",
       linetype = "Country rank") +
  theme_minimal() +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1) #,
    # panel.spacing = unit(2, "lines")
  ) +  # Increase the space between country groups
  scale_y_continuous(label = comma) +
  scale_linetype_manual(values = c("Top 10" = "solid", "Outside top 10" = "dashed")) +
  # scale_fill_manual(values = datasource_palette()) +
  scale_alpha_manual(values = c("Top 10" = 1, "Outside top 10" = 0.4)) +
  scale_fill_manual(values = datasource_palette(),
                    # breaks = c("moh", "iata_method1", "iata_method2")) +
                    breaks = c("MOH", "IATA w/ baseline adjustment",
                               "IATA w/ no baseline adjustment"
                    )) +
  coord_flip()  # Flip coordinates for horizontal bars

ggsave("compare_top10_origins.png", figure, bg = "white")


# Alternative visualization for manuscript ----

pct_threshold <- 0.81

# select top 10 orig countries
moh_toppct <- compare_proportions_moh %>% 
  arrange(desc(proportion)) %>%
  mutate(cumulative_prop = cumsum(proportion)) %>% 
  filter(cumulative_prop < pct_threshold) %>% 
  select(-total_pilgrims)

iata1_toppct <- compare_proportions_iata1 %>% 
  arrange(desc(proportion)) %>% 
  mutate(cumulative_prop = cumsum(proportion)) %>%   
  filter(cumulative_prop < pct_threshold) %>% 
  select(-total_pilgrims)

iata2_toppct <- compare_proportions_iata2 %>% 
  arrange(desc(proportion)) %>% 
  mutate(cumulative_prop = cumsum(proportion)) %>%   
  filter(cumulative_prop < pct_threshold) %>% 
  select(-total_pilgrims)

toppcts <- bind_rows(moh_toppct, iata1_toppct, iata2_toppct)

# For plotting purposes, let's ensure each method includes the same set of countries

## Step 1: Identify the full set of top 10 countries
plot2_top_countries <- unique(toppcts$orig_country)

moh_ranked2 <- compare_proportions_moh %>% 
  arrange(desc(proportion)) %>%
  mutate(cumulative_prop = cumsum(proportion),
         prominent_source = if_else(cumulative_prop < pct_threshold,
                                    "Major contributor", "Minor contributor")) %>%
  filter(orig_country %in% plot2_top_countries) %>% 
  select(-total_pilgrims)

iata1_ranked2 <- compare_proportions_iata1 %>% 
  arrange(desc(proportion)) %>%
  mutate(cumulative_prop = cumsum(proportion),
         prominent_source = if_else(cumulative_prop < pct_threshold,
                                    "Major contributor", "Minor contributor")) %>%
  filter(orig_country %in% plot2_top_countries) %>% 
  select(-total_pilgrims)

iata2_ranked2 <- compare_proportions_iata2 %>% 
  arrange(desc(proportion)) %>%
  mutate(cumulative_prop = cumsum(proportion),
         prominent_source = if_else(cumulative_prop < pct_threshold,
                                    "Major contributor", "Minor contributor")) %>%
  filter(orig_country %in% plot2_top_countries) %>% 
  select(-total_pilgrims)

plottoppcts <- bind_rows(moh_ranked2, iata1_ranked2, iata2_ranked2)

# # figure <-
#   ggplot(plottop10s) +
#   geom_bar(aes(x = orig_country, y = pilgrim_estimate,
#                alpha = rank,
#                fill = method,
#                linetype = rank),
#            stat = "identity", position = "dodge", color = "black") +
#   labs(x = "Pilgrim origin country",
#        y = "Number of pilgrims",
#        fill = "Movement input method",
#        alpha = "Country rank",
#        linetype = "Country rank") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_y_continuous(label=comma) +
#   scale_linetype_manual(values = c("Top 10" = "solid", "Outside top 10" = "dashed")) +
#   scale_alpha_manual(values = c("Top 10" = 1, "Outside top 10" = 0.4))
# 
# ggsave("compare_top10_origins.png", figure, bg = "white")


# Reorder countries by pilgrim_estimate for the "moh" method, and apply this order across all methods
plottoppcts <- plottoppcts %>%
  group_by(orig_country) %>%
  mutate(rank_moh = max(pilgrim_estimate[method == "moh"])) %>%  # Get max value for 'moh' method for each country
  ungroup() %>%
  mutate(orig_country = fct_reorder(orig_country, rank_moh, .desc = FALSE))   # Reorder by 'moh' estimate


# Syria and Yemen to not feature in IATA estimates so need to add rows with zeros for these
missing_countries <- c("Syria", "Yemen")
dummy_rows <- expand.grid(
  orig_country     = missing_countries,
  method           = c("iata_method2",
                       "iata_method1"),
  proportion       = 0,
  prominent_source = "Minor contributor", 
  stringsAsFactors = FALSE
)

# add dummy rows to main dataframe
plottoppcts <- bind_rows(plottoppcts, dummy_rows)

# set all factor levels
plottoppcts$method <- factor(plottoppcts$method,
                               levels = c("iata_method2", "iata_method1", "moh"),
                               labels = c("IATA w/ no baseline adjustment",
                                          "IATA w/ baseline adjustment",
                                          "MOH"))

plottoppcts$prominent_source <- factor(plottoppcts$prominent_source,
                                       levels = c("Major contributor", "Minor contributor"),
                                       labels = c("High-contribution countries", "Lower-contribution countries"))


# Reorder countries by pilgrim_estimate for the "moh" method, and apply this order across all methods
plottoppcts <- plottoppcts %>%
  group_by(orig_country) %>%
  mutate(rank_moh = max(pilgrim_estimate[method == "MOH"])) %>%  # Get max value for 'moh' method for each country
  ungroup() %>%
  mutate(orig_country = fct_reorder(orig_country, rank_moh, .desc = FALSE))   # Reorder by 'moh' estimate



figure_pct <- ggplot(plottoppcts) +
  geom_bar(aes(x = orig_country, y = proportion,
               fill = method, alpha = prominent_source, 
               linetype = prominent_source, group = method),
           stat = "identity", position = "dodge",
           color = "black", width = 0.7) +
  labs(x = "Pilgrim origin country",
       y = "Proportion of foreign pilgrims\nfrom each origin country",
       fill = "Movement input method",
       alpha = "Relative contribution\nof pilgrims",
       linetype = "Relative contribution\nof pilgrims") +
  theme_minimal() +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1) #,
    # panel.spacing = unit(2, "lines")
  ) +  # Increase the space between country groups
  scale_y_continuous(label = comma) +
  scale_fill_manual(values = datasource_palette(),
                    # breaks = c("moh", "iata_method1", "iata_method2")) +
                    breaks = c("MOH", "IATA w/ baseline adjustment",
                               "IATA w/ no baseline adjustment"
                    )) +
  scale_linetype_manual(values = c("High-contribution countries" = "solid", "Lower-contribution countries" = "dashed")) +
  # scale_fill_manual(values = datasource_palette()) +
  scale_alpha_manual(values = c("High-contribution countries" = 1, "Lower-contribution countries" = 0.4)) +
  coord_flip()  # Flip coordinates for horizontal bars

ggsave("compare_toppct_origins.png", figure_pct, bg = "white")
