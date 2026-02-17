# Equity in Ocean Access (Benefits and Barriers – bab)
# Manuela Diaz
## California Marine Sanctuary Foundation/ CINMS
# Driver de análisis - Manuela (enfoque especies valoradas)
# Basado en bab_driver.R de Jenny

# goal: driver file for code to analyze surveys about ocean access done in 2025, with focus on analyzing valued marine species

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(likert)

# ----------------------------------------------------------
# --------------------------------------------------------------------------
# 0. load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("/Users/manu/Desktop/research/r_projects/bab_survey_md")

# -------------------------------
# 1. -- data cleaning --

# 1.1. import joined data from qualitrics, remove low quality data, and organize headers - importar data combinada, filtrar calidad y organizar headers
source("./bin/bab_clean_data.R")
# input:   ./data/bab_2025_combined_data_20260121.csv
# outputs:          ./results/data_wide.csv
#                   ./results/data_long.csv
#                   ./results/header_meta_top.csv
#                   ./results/header_meta_labels.csv

# 1.2.clean by race (Q39) - limpiar RACE en categorías que usará todo el equipo
source("./bin/bab_clean_data_race_q39.R")
# input:            ./results/data_long.csv
# output:           ./results/data_long2.csv

# 1.3. clean by gender (Q25) - limpiar GENDER
source("./bin/bab_clean_data_gender_q25.R")
# input:            ./results/data_long2.csv
# output:           ./results/data_long3.csv

# 1.4. clean by activities (Q4, Q5) - limpiar ACTIVITIES
source("./bin/bab_clean_data_activities_q4_q5.R")
# input:            ./results/data_long3.csv
# output:           ./results/data_long4.csv

# 1.5 clean income (Q45)
source("./bin/bab_clean_data_income_q45.R")  
# input: ./results/data_long4.csv
# outputs: ./results/data_long5.csv

# 1.6 clean by frequency of visit (access) (Q2) - Frecuencia de visitas a la costa 
#    creates categories - crea categorías ordenadas tipo: "Several times per week" ... "Less than once a year"
source("./bin/bab_clean_frequency_visits_q2.R")
# **(nuevo script que tú crearás)
# input:   ./results/data_long5.csv
# output:  ./results/data_long6.csv  # agrega columna p.ej. freq_coast_cat

# 1.7 clean preferred change in time (Q1)
source("./bin/bab_clean_data_timepref_q1.R")
# input: ./results/data_long6.csv
# outputs: ./results/data_long7.csv

# 1.8 clean visits to MPAs / NMS (Q31)
source("./bin/bab_clean_data_visit_mpa_nms_q31.R")
# input: ./results/data_long7.csv
# outputs: ./results/data_long8.csv

# 1.9 clean county where spend most time (county column: depending on southern, central or northern CA; Q3a1,Q3a2,Q3a3)
source("./bin/bab_clean_data_county_spend.R")
# input: ./results/data_long8.csv
# outputs: ./results/data_long9.csv

# 1.10. clean location where spend most time (map_location)
# source("./bin/bab_clean_data_county_spend_map.R")
# input: ./results/data_longx.csv
# outputs: ./results/data_longx.csv

#1.11.remove headers filas de headers: Remover filas de headers redundantes y dejar nombres finales
source("./bin/bab_clean_data_remove_headers.R")
# input:           ./results/data_long9.csv
# output:          ./results/data_long_clean.csv   # <-- master tidy file

#----------------------------------------------------------
# ----------------------------------------------------------
# 2. Load clean data - cargar data larga limpia para trabajar #### -----------
#    Ésta es tu “data master” para análisis

#bab_long <- readr::read_csv("./results/data_long_clean.csv",
#                            show_col_types = FALSE)
# Chequeo rápido
#glimpse(bab_long)

# ----------------------------------------------------------
# 4. ANALYSIS / ANÁLISIS DE ESPECIES VALORADAS (Q14_4 / Q14_5) #### -------------

# 4.1. Limpieza y categorización de especies (Q14_4 / Q14_5)
source("./bin/bab_clean_species_q14_4_q14_5.R")
# outputs:
#   ./results/q14_species_long.csv
#   ./results/q14_species_unmatched.csv

# 4.2. Resúmenes y gráficos básicos
#     - top N especies valoradas (global)
#     - top especies por actividad costera
#     - top especies por región
# source("./bin/bab_q12_species_summary_plots.R")

# 4.3. Análisis de equidad
#     - especies valoradas x raza/etnicidad (race_cat)
#     - x género (gender_cat)
#     - x ingreso (income_cat)
#     - x frecuencia de visitas (freq_coast_cat)
#     - x visitas a MPA / NMS (visit_mpa / visit_nms)
# source("./bin/bab_q12_species_equity.R")

# ----------------------------------------------------------
# 5. VARIABLES DE ACCESO COSTERO (para figuras/tablas) ####-

# Ejemplos de scripts futuros que usan bab_long ya limpio:

# - Frecuencia de visita a la costa (Q2) por grupo demográfico
# source("./bin/bab_q2_coastal_frequency.R")

# - Condado donde pasan más tiempo por race / income
# source("./bin/bab_q_county_most_time.R")

# - Relación entre frecuencia de visita y preferencia de “more time”
# source("./bin/bab_q1_q2_time_pref_vs_frequency.R")

# ----------------------------------------------------------
# Fin del driver
# Corre todo este archivo una vez para generar los .csv limpios,
# luego puedes ir ejecutando solo los scripts específicos que edites.
# ----------------------------------------------------------