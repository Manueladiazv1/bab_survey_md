# ==========================================================
# Equity in Ocean Access (Benefits and Barriers - BAB)
# Manuela Diaz
# California Marine Sanctuary Foundation / CINMS
#
# GOAL:
# Final tidy file – no header rows to remove (data already clean)
#
# INPUT:  ./results/data_long9.csv
# OUTPUT: ./results/data_long_clean.csv
# ==========================================================

library(tidyverse)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("/Users/manu/Desktop/research/r_projects/bab_survey_md")

d1 <- read_csv("./results/data_long9.csv") %>%
  glimpse()

# ----------------------------------------------------------
# No header rows to remove – data starts at row 1
# ----------------------------------------------------------
d2 <- d1

# ----------------------------------------------------------
# Save master tidy file
# ----------------------------------------------------------
write_csv(d2, "./results/data_long_clean.csv")
