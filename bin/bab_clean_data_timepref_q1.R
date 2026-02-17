# ==========================================================
# Equity in Ocean Access (Benefits and Barriers - BAB)
# Manuela Diaz
# California Marine Sanctuary Foundation / CINMS
#
# GOAL:
# Clean preferred change in time spent at ocean & coast (Q1)
# ==========================================================

library(tidyverse)

## ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("/Users/manu/Desktop/research/r_projects/bab_survey_md")

d0 <- read_csv("./results/data_long6.csv") %>%
  mutate(Q1 = as.character(Q1)) %>%
  glimpse()
d0

d1 <- d0

# ----------------------------------------------------------
# Check original values
# ----------------------------------------------------------
unique(d1$Q1)

# ----------------------------------------------------------
# Minimal cleanup (same logic as Q2)
# ----------------------------------------------------------
# Remove extra spaces just in case
d1$Q1 <- str_squish(d1$Q1)

#  if survey exports ever change wording slightly, align them — but for now, no changes needed
unique(d1$Q1)

# ----------------------------------------------------------
# Save
# ----------------------------------------------------------
write_csv(d1, "./results/data_long7.csv")
