# Equity in Ocean Access (Benefits and Barriers – bab)
# Manuela Diaz – 2025
# California Marine Sanctuary Foundation/ CINMS

# goal: clean race data Q39 and Q39_TEXT (other race or ethnicity) for long data
# source: bab_clean_data_race_q39.R

# ----------------------------------------------------------
# load libraries - cargar librerías ######-------------------------------------
library(tidyverse)
library(stringr)

# ----------------------------------------------------------
# load data_long - Cargar data_long ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("/Users/manu/Desktop/research/r_projects/bab_survey_md")

d1 <- readr::read_csv("./results/data_long.csv",
                      show_col_types = FALSE) %>%
  mutate(
    Q39        = as.character(Q39),
    Q39_10_TEXT = as.character(Q39_10_TEXT)
  )

# initial check - inspeccionar al inicio:
glimpse(d1)
unique(d1$Q39)
unique(d1$Q39_10_TEXT)

# ----------------------------------------------------------
# fix "Native Hawaiian or Other Pacific Islander" category
d1$Q39 <- gsub("Native Hawaiian or Other Pacific Islander",
               "Native Hawaiian or Pacific Islander",
               d1$Q39)

# fix version of "Another race or ethnicity"
d1$Q39 <- gsub("Other",
               "Another race or ethnicity",
               d1$Q39)

# survey specific response
d1$Q39 <- gsub("Another race or ethnicity, please specify:",
               "Another race or ethnicity",
               d1$Q39)

unique(d1$Q39)

# ----------------------------------------------------------
#  if people wrote a race and "choose not to answer" we assigned them the race they chose 
# - si eligieron una raza + "Choose not to answer", nos quedamos con la raza

d1$Q39 <- gsub("White,Choose not to answer", "White", d1$Q39)

d1$Q39 <- gsub(
  "Hispanic or Latino,Native Hawaiian or Pacific Islander,White,Choose not to answer",
  "Hispanic or Latino,Native Hawaiian or Pacific Islander,White",
  d1$Q39
)

d1$Q39 <- gsub("Hispanic or Latino,Choose not to answer",
               "Hispanic or Latino", d1$Q39)

d1$Q39 <- gsub("Black or African American,Choose not to answer",
               "Black or African American", d1$Q39)

d1$Q39 <- gsub("Asian,Choose not to answer", "Asian", d1$Q39)

d1$Q39 <- gsub("American Indian or Alaska Native,Choose not to answer",
               "American Indian or Alaska Native", d1$Q39)

d1$Q39 <- gsub("Middle Eastern or North African,Choose not to answer",
               "Middle Eastern or North African", d1$Q39)

d1$Q39 <- gsub("Another race or ethnicity,Choose not to answer",
               "Another race or ethnicity", d1$Q39)


# ----------------------------------------------------------
# if people selected and also choose not to answer or all responses
# - personas que marcaron "todo" o casi todo, nos quedamos con "Choose not to answer"  

d1$Q39 <- gsub(
  "American Indian or Alaska Native,Asian,Black or African American,Hispanic or Latino,Middle Eastern or North African,Native Hawaiian or Pacific Islander,White,Another race or ethnicity,Choose not to answer",
  "Choose not to answer",
  d1$Q39
)

d1$Q39 <- gsub(
  "American Indian or Alaska Native,Asian,Black or African American,Hispanic or Latino,Middle Eastern or North African,Native Hawaiian or Pacific Islander,White,Another race or ethnicity",
  "Choose not to answer",
  d1$Q39
)

d1$Q39 <- gsub(
  "Asian,Black or African American,Hispanic or Latino,Native Hawaiian or Pacific Islander,White,Choose not to answer",
  "Choose not to answer",
  d1$Q39
)

d1$Q39 <- gsub(
  "American Indian or Alaska Native,Asian,Black or African American,Hispanic or Latino,Middle Eastern or North African,Native Hawaiian or Pacific Islander,White,Another race or ethnicity",
  "Choose not to answer",
  d1$Q39
)

# d1$Q24<-gsub("Another race or ethnicity,Choose not to answer" ,"Choose not to answer",d1$Q24)

# consider grouping with "white because they are such a small community
# d1$Q24<-gsub("Middle Eastern or North African","White",d1$Q24)


#Check "Choose not to answer" combinations 
d1 %>%
  filter(str_detect(Q39, "Choose not to answer"),
         str_detect(Q39, ",")) %>%   # hay más de una categoría en la cadena
  distinct(Q39)

# ----------------------------------------------------------
# fix duplicates - duplicados tipo "White,White" ####

d1$Q39 <- gsub("White,White", "White", d1$Q39)
d1$Q39 <- gsub("White,Native Hawaiian or Pacific Islander,White",
               "Native Hawaiian or Pacific Islander,White",
               d1$Q39)

unique(d1$Q39)

# ----------------------------------------------------------
#  (Q39_10_TEXT)  ###### ---------------------------------
# clean "Another race or ethnicity" text entry (Q39_10_TEXT)- limpiar texto libre de Q39_10_TEXT ####
d1 <- d1 %>%
  mutate(
    Q39_10_TEXT = case_when(
      !is.na(Q39_10_TEXT) ~ Q39_10_TEXT %>%
        str_replace_all("(?i)\\sand\\s", ",") %>%
        str_replace_all("(?i)\\s*&\\s*", ",") %>%
        str_replace_all("/", ",") %>%
        str_replace_all(";", ",") %>%
        str_squish(),
      TRUE ~ NA_character_
    )
  )

unique(d1$Q39_10_TEXT)

#correct typos: 
d1 <- d1 %>%
  mutate(
    Q39_10_TEXT = recode(Q39_10_TEXT,
                         "Euopean"          = "European",
                         "Pacific Island"   = "Pacific Islander",
                         "jewish"           = "Jewish",
                         "Ashkenazi jewish" = "Ashkenazi Jewish",
                         "Sicilian,"        = "Sicilian",
                         "Ukraine"          = "Ukrainian",
                         "Colombia ingjes"  = "Colombian",
                         "Indian,black"  = "Indian,Black",
                         "Belizean,black"  = "Belizean,Black",
                      
    )
  )

#check unique
unique(d1$Q39_10_TEXT)

# replace for NA phrases that are not race/ethnicity - eliminar frases que NO son razas (troll / decline)
d1 <- d1 %>%
  mutate(
    lowered = tolower(Q39_10_TEXT),
    Q39_10_TEXT = case_when(
      !is.na(lowered) &
        str_detect(lowered,
                   "don.?t make this about race|rather not say|a dog|898|humsna|human|californian"
        ) ~ NA_character_,
      TRUE ~ Q39_10_TEXT
    )
  ) %>%
  select(-lowered)

#check unique
unique(d1$Q39_10_TEXT)

# ----------------------------------------------------------
# save csv - guardar resultado ####

readr::write_csv(d1, "./results/data_long2.csv")
