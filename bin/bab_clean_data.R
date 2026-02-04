# Equity in Ocean Access (MPAs Equity and Climate (mec))
# Manuela Diaz - base code from Tim Frawley & Jennifer Selgrath
# California Marine Sanctuary Foundation/ CINMS

# goal: organize and clean data from qualtrics - organizar y limpiar data combinada de Qualtrics
#      to create - para generar:
#         - ./results/data_long.csv  (1 fila = 1 respuesta)
#         - ./results/data_wide.csv  (1 fila = 1 "pregunta"/variable)

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(likert) 

# --------------------------------------------------------------------------
# load data - cargar datos ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("/Users/manu/Desktop/research/r_projects/bab_survey_md")

d1 <- readr::read_csv("./data/bab_2025_combined_data_20260121.csv",
                      show_col_types = FALSE)

glimpse(d1)
colnames(d1)

###Data Cleaning ------------------------------------------------------

# save metadata 

# 1. Guardar filas de metadatos y quedarnos SOLO con respuestas 2025 ####
# En tu export de Qualtrics:
#   - Fila 1: metadatos técnicos (start_date, status, etc.)
#   - Fila 2: question-specific prompts / labels
#   - Filas 3+: respuestas reales
#
# Para seguir el espíritu del código de Jenny:
#   - guardamos las dos filas de metadatos (header técnico + header de preguntas)
#   - dejamos solo las respuestas reales en d2
#   - filtramos solo year == 2025

header_top    <- d1 %>% slice(1)   # fila 1: meta técnico
header_labels <- d1 %>% slice(2)   # fila 2: labels de preguntas

# dejamos solo respuestas reales y filtramos 2025
d2 <- d1 %>%
  slice(-c(1, 2)) %>%         # quitar dos filas de metadatos
  filter(year == 2025)        # solo encuestas del año 2025

glimpse(d2)

## 3. Tag Data that are Paper Survey Entries - Crear variable "format" (Q50) ####
# En el código original, Jenny:
#   - Miraba Q32 para ver si contenía "PAPER_DATA_ENTRY"
#   - Solo aplicaba esa lógica para YEAR == 2024
#   - Clasificaba como "Paper" o "Digital" y dejaba "Unknown" para el resto
#
# Para tu versión 2025:
#   - Solo trabajamos con year == 2025
#   - No usamos Q32; el indicador de encuestas en papel es que la persona
#     escribió la palabra "paper" en Q50
#   - Creamos:
#       * format0 = TRUE si Q50 contiene "paper" (insensible a may/min)
#       * format = "Paper" si format0 == TRUE
#       * format = "Digital" en todos los otros casos (incluye NA en Q50)

d2 <- d2 %>%
  mutate(format0 = str_detect(tolower(Q50), "paper")) %>%
  mutate(
    format = case_when(
      format0 ~ "Paper",
      TRUE    ~ "Digital"
    )
  ) %>%
  select(-format0)
names(d2)

# check visually some columns
d2 %>%
  select(1, 7, Q50, year, format) %>% 
  View()

#check format
d2 %>% count(format)


##Save row with question Specific Prompts for later
# En el código original, Jenny hacía algo como:
#   header <- d2[7,]
#   header2 <- paste0(d2[1,], "_", d2[7,])
#   header2 <- gsub(" ", "", header2)
#
# En tu export 2025, las "question-specific prompts" están en la fila 2 de d1,
# no en la fila 7 de d2. Así que adaptamos:
#   - usamos header_labels (fila 2 de d1)
#   - combinamos header_top (fila 1) + header_labels para hacer algo tipo header2

# header con labels de preguntas (similar a lo que Jenny quería guardar)
header <- header_labels

# combinar info técnica + labels en un vector estilo "Q1_startdate" pero sin espacios
header2 <- paste0(as.character(header_top[1, ]), "_", as.character(header_labels[1, ]))
header2 <- gsub(" ", "", header2)  # remove spaces

# (opcional) guardar lookup de headers para revisarlos en Excel
header_lookup <- tibble(
  colname = colnames(d1),
  tech    = as.character(header_top[1, ]),
  label   = as.character(header_labels[1, ]),
  combined = header2
)

readr::write_csv(header_lookup, "./results/header_lookup.csv")

# ##Remove Digital Responses (i.e., tablet or phone where people spent less than 400 seconds on the Survey)
# d2$format<-ifelse(d2$format=="Digital" & d2$Duration_Seconds < 400, "Bad_Digital", d2$format)
# d2<-d2[which(!d2$format=="Bad_Digital"),]
# 
# ##Remove Paper Data Entries of Low (i.e., 3) Quality
# Paper_Data<-d2[grepl("PAPER_DATA_ENTRY", d2$Q32), , drop = FALSE]
# Bad_Paper_Data<-d2[grepl("DATA_QUALITY: 3", d2$Q32), , drop = FALSE]
# Bad_Data<-as.list(Bad_Paper_Data$ResponseId)
# 
# ###Remove Data where people answered less than 60% of the questions
# d2<-d2[-which(d2$ResponseId %in% Bad_Data), ]
# d2$Progress<-as.numeric(d2$Progress)
# d2<-d2[which(d2$Progress > 60),]

###Remove Data where people are not residents of California
# d2<-d2[which(!d2$Q23=="I do not live in California"),]

# transpose --------------
###Load in Data file
d3 <- d2 %>%
  mutate(quest_comb = NA_character_) %>% #Crea una nueva columna llamada quest_comb rellena de NA
  select(
    quest_comb,
    response_id = ResponseId,  # renombramos para seguir la convención de Jenny
    Q1:format                  # todas las Qs + year + format
  ) %>%
  rename_with(
    ~ make.names(
      .x %>%
        str_remove_all(" ") %>%
        str_replace("Q3a2...24", "Q3a2") %>%
        str_replace("Q3a2...27", "Q3a2") %>%
        str_replace("Q30...234", "Q30") %>%
        str_replace("Q30...184", "Q30") %>%
        str_replace("Q31...185", "Q31a") %>%
        str_replace("Q31...235", "Q31b"),
      unique = TRUE
    )
  ) %>%
  glimpse()

questions2 <- colnames(d3)

## transpose Data so that each column is a survey response and each row is a prompt
d4 <- as.data.frame(t(d3)) %>%
  mutate(questions = V1) %>%
  glimpse()

d4[1:10, ]

d5 <- cbind(d4, questions2)

glimpse(d5)

# 5. export formatted data --------------------------------
write_csv(d3, "./results/data_long.csv")
write_csv(d5, "./results/data_wide.csv")

# también guardamos las filas de metadatos por si las quieres revisar aparte
write_csv(header_top,    "./results/header_meta_top.csv")
write_csv(header_labels, "./results/header_meta_labels.csv")