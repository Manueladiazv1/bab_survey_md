# ==========================================================
# Equity in Ocean Access (Benefits and Barriers - BAB)
# Manuela Diaz
# California Marine Sanctuary Foundation / CINMS
#
# GOAL:
# Clean frequency of coastal visits (Q2)
# ==========================================================

library(tidyverse)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("/Users/manu/Desktop/research/r_projects/bab_survey_md")

d0 <- read_csv("./results/data_long5.csv") %>%
  mutate(Q2 = as.character(Q2)) %>%
  glimpse()
d0

d1 <- d0

# ----------------------------------------------------------
# Check original values
# ----------------------------------------------------------
unique(d1$Q2)


# Optional but OK to keep (minimal hygiene)
d1$Q2 <- str_squish(d1$Q2)

unique(d1$Q2)

# ----------------------------------------------------------
# Simplify wording (survey label cleanup)
# ----------------------------------------------------------
d1$Q2 <- gsub(
  "Less than once a year \\(i.e., rarely or never\\)",
  "Less than once a year",
  d1$Q2
)

unique(d1$Q2)

# ----------------------------------------------------------
# Save
# ----------------------------------------------------------
write_csv(d1, "./results/data_long6.csv")
