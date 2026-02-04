# ==========================================================
# Equity in Ocean Access (Benefits and Barriers - BAB)
# Manuela Diaz
# California Marine Sanctuary Foundation / CINMS
#
# GOAL:
# Clean county where respondent spends most time.
# The survey is single-select, but responses live in different columns:
#   Q3a1 (one region), Q3a2 (another), Q3a3 (another)
#
# INPUT:  ./results/data_long9.csv
# OUTPUT: ./results/data_long10.csv
# ==========================================================

library(tidyverse)
library(stringr)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("/Users/manu/Desktop/research/r_projects/bab_survey_md")

d0 <- read_csv("./results/data_long8.csv") %>%
  mutate(
    Q3a1 = as.character(Q3a1),
    Q3a2 = as.character(Q3a2),
    Q3a3 = as.character(Q3a3)
  ) %>%
  glimpse()

d1 <- d0

# ----------------------------------------------------------
# Quick check (optional)
# ----------------------------------------------------------
unique(d1$Q3a1)
unique(d1$Q3a2)
unique(d1$Q3a3)

# ----------------------------------------------------------
# Create single county variable
# (take the non-NA value across Q3a1, Q3a2, Q3a3)
# ----------------------------------------------------------
d1 <- d1 %>%
  mutate(
    county_spend_most_time = case_when(
      !is.na(Q3a1) & Q3a1 != "" ~ Q3a1,
      !is.na(Q3a2) & Q3a2 != "" ~ Q3a2,
      !is.na(Q3a3) & Q3a3 != "" ~ Q3a3,
      TRUE ~ NA_character_
    )
  )

# ----------------------------------------------------------
# Minimal cleanup (same spirit as Jenny)
# ----------------------------------------------------------
d1$county_spend_most_time <- str_squish(d1$county_spend_most_time)

# ----------------------------------------------------------
# Fix known typos in county names
# ----------------------------------------------------------
d1$county_spend_most_time <- gsub(
  "Mendicino",
  "Mendocino",
  d1$county_spend_most_time
)

# ----------------------------------------------------------
# Quick check
# ----------------------------------------------------------
unique(d1$county_spend_most_time)

# ----------------------------------------------------------
# Save
# ----------------------------------------------------------
write_csv(d1, "./results/data_long9.csv")
