# ==========================================================
# Equity in Ocean Access (Benefits and Barriers - BAB)
# Manuela Diaz
# California Marine Sanctuary Foundation / CINMS
#
# GOAL:
# Clean household income (Q45) in long-format survey data.
#
# NOTES:
# - Q45 is a single-choice question (only one option allowed)
# - We keep the original survey income categories
# - Known survey export typo is corrected explicitly - one typo: "$60,000 to $119,000" should be "$60,000 to $119,999"
# - No numeric grouping is applied at this stage
# ==========================================================


# ----------------------------------------------------------
# 1. Load libraries
# ----------------------------------------------------------
library(tidyverse)
library(stringr)


# ----------------------------------------------------------
# 2. Load data
# ----------------------------------------------------------
# Clear environment to avoid conflicts with old objects
rm(list = ls(all = TRUE))

# Set working directory
setwd("/Users/manu/Desktop/research/r_projects/bab_survey_md")

# Read the latest cleaned dataset from the previous step
d0 <- read_csv("./results/data_long4.csv") %>%
  # Ensure income is treated as text
  mutate(Q45 = as.character(Q45)) %>%
  glimpse()

unique(d0$Q45)

# Work on a copy 
d1 <- d0

# ----------------------------------------------------------
# 3. Initial check
# ----------------------------------------------------------
# Inspect raw income values
unique(d1$Q45)

# ----------------------------------------------------------
# 4. Basic text cleanup
# ----------------------------------------------------------
# Remove leading/trailing spaces and collapse multiple spaces
d1$Q45 <- str_squish(d1$Q45)

# Normalize curly apostrophes if they appear in exports
d1$Q45 <- str_replace_all(d1$Q45, "\u2019", "'")


# ----------------------------------------------------------
# 4.5 Fix known survey export typo
# ----------------------------------------------------------
# The survey incorrectly exported "$60,000 to $119,000"
# We standardize it to "$60,000 to $119,999"
# to match the official income brackets

d1$Q45 <- str_replace_all(
  d1$Q45,
  "^\\$60,000 to \\$119,000$",
  "$60,000 to $119,999"
)

unique(d1$Q45)

# ----------------------------------------------------------
# 5. Align wording to official survey categories
# ----------------------------------------------------------
# Expected income categories from the survey
allowed_income <- c(
  "Less than $10,000",
  "$10,000 to $19,999",
  "$20,000 to $39,999",
  "$40,000 to $59,999",
  "$60,000 to $119,999",
  "$120,000 to $179,999",
  "$180,000 to $239,999",
  "$240,000 or more",
  "Choose not to answer"
)

# Defensive cleanup in case exports add odd spacing
d1$Q45 <- str_replace_all(d1$Q45, "\\$\\s+", "\\$")     # "$ 10,000" -> "$10,000"
d1$Q45 <- str_replace_all(d1$Q45, "\\s+to\\s+", " to ") # normalize "to" spacing

# Re-check values after cleanup
unique(d1$Q45)


# Convert empty strings to NA
d1$Q45[d1$Q45 == ""] <- NA

# ----------------------------------------------------------
# 7. Validate final values
# ----------------------------------------------------------
# Identify any values that do not match expected categories
unexpected_q45 <- d1 %>%
  distinct(Q45) %>%
  filter(!is.na(Q45)) %>%
  filter(!(Q45 %in% allowed_income))

# Should return 0 rows
unexpected_q45

# Final inspection
unique(d1$Q45)

# ----------------------------------------------------------
# 10. Save cleaned output
# ----------------------------------------------------------
write_csv(d1, "./results/data_long5.csv")


