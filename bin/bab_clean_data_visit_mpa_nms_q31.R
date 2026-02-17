# ==========================================================
# Equity in Ocean Access (Benefits and Barriers - BAB)
# Manuela Diaz
# California Marine Sanctuary Foundation / CINMS
#
# GOAL:
# Clean visits to MPAs / National Marine Sanctuaries (Q31)
# Q31 is multi-select (comma-separated in the long file)
# ==========================================================

library(tidyverse)
library(stringr)

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("/Users/manu/Desktop/research/r_projects/bab_survey_md")

d0 <- read_csv("./results/data_long7.csv") %>%
  mutate(Q31 = as.character(Q31)) %>%
  glimpse()

d1 <- d0

# ----------------------------------------------------------
# Check original values
# ----------------------------------------------------------
unique(d1$Q31)

# ----------------------------------------------------------
# ADD FLAG COLUMN: record who selected "I'm not sure"
# (do this BEFORE cleaning/removal so you keep a record)
# ----------------------------------------------------------
d1 <- d1 %>%
  mutate(
    Q31_not_sure_flag = if_else(
      str_detect(Q31, regex("I.?m not sure", ignore_case = TRUE)),
      1,
      0,
      missing = 0
    )
  )

# ----------------------------------------------------------
# Minimal cleanup (same spirit as Jenny)
# ----------------------------------------------------------
d1$Q31 <- str_squish(d1$Q31)

# ----------------------------------------------------------
# Remove options we do NOT want to keep
# (only present in some survey versions)
# ----------------------------------------------------------
d1$Q31 <- gsub("Visit a State Park", "", d1$Q31)
d1$Q31 <- gsub("Visit a National Park", "", d1$Q31)

unique(d1$Q31)
# ----------------------------------------------------------
# Standardize wording across survey versions
# ----------------------------------------------------------

# State MPA wording 
d1$Q31 <- gsub( "Visit a State Marine Protected Area \\(MPA\\)", "California Marine Protected Area (MPA)", d1$Q31 ) 

# National Marine Sanctuary wording (both variants -> NMS) 
d1$Q31 <- gsub( "Visit a National Marine Sanctuary|National Marine Sanctuary", "National Marine Sanctuary (NMS)", d1$Q31 )


unique(d1$Q31)

# ----------------------------------------------------------
# APPLY YOUR RULES ABOUT "I'm not sure"
# If it's selected WITH MPA and/or NMS, drop only "I'm not sure"
# ----------------------------------------------------------

# Case 1: MPA + NMS + I'm not sure  -> keep MPA + NMS
d1$Q31 <- gsub(
  "California Marine Protected Area \\(MPA\\),\\s*National Marine Sanctuary \\(NMS\\),\\s*I.?m not sure",
  "California Marine Protected Area (MPA),National Marine Sanctuary (NMS)",
  d1$Q31
)

# Case 2: MPA + I'm not sure -> keep MPA
d1$Q31 <- gsub(
  "California Marine Protected Area \\(MPA\\),\\s*I.?m not sure",
  "California Marine Protected Area (MPA)",
  d1$Q31
)

# Case 3: NMS + I'm not sure -> keep NMS
d1$Q31 <- gsub(
  "National Marine Sanctuary \\(NMS\\),\\s*I.?m not sure",
  "National Marine Sanctuary (NMS)",
  d1$Q31
)

## ----------------------------------------------------------
# Tidy commas and whitespace after replacements
# ----------------------------------------------------------
d1$Q31 <- gsub(",+", ",", d1$Q31)   # collapse double commas
d1$Q31 <- gsub("^,|,$", "", d1$Q31) # remove leading/trailing commas
d1$Q31 <- trimws(d1$Q31)

unique(d1$Q31)


# Convert empty strings to NA
d1$Q31[d1$Q31 == ""] <- NA

# ----------------------------------------------------------
# Final check (optional)
# ----------------------------------------------------------
unique(d1$Q31)

# ----------------------------------------------------------
# Save
# ----------------------------------------------------------
write_csv(d1, "./results/data_long8.csv")