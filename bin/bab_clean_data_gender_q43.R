# Equity in Ocean Access (Benefits and Barriers – bab)
# Manuela Diaz
# California Marine Sanctuary Foundation / CINMS

# goal: limpiar género (Q43) para data_long (2025)

# ----------------------------------------------------------
# load libraries - cargar librerias######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

# ----------------------------------------------------------
# 2. Cargar data_long2 ####
rm(list = ls(all = TRUE))
setwd("/Users/manu/Desktop/research/r_projects/bab_survey_md")

d0<-read_csv("./results/data_long2.csv")%>%
  mutate(Q43 = as.character(Q43))%>%
  glimpse()

d1<-d0

# Chequeo rápido (opcional):
glimpse(d1)
unique(d1$Q43)

# simplify responses if gender + choose not to answer
# simplificar combinaciones con "Choose not to answer": si alguien marcó un género + "Choose not to answer" nos quedamos con el género que sí marcó.

# Female + Choose not to answer -> Female
d1$Q43 <- gsub("Female,Choose not to answer", "Female", d1$Q43)

# Male + Choose not to answer -> Male
d1$Q43 <- gsub("Male,Choose not to answer", "Male", d1$Q43)

# Trans / non-binary + Choose not to answer -> Trans / non-binary
d1$Q43 <- gsub(
  "Transgender, non-binary, or another gender,Choose not to answer",
  "Transgender, non-binary, or another gender",
  d1$Q43
)

unique(d1$Q43)

# ----------------------------------------------------------
# save csv_ guardar output 
readr::write_csv(d1, "./results/data_long3.csv")