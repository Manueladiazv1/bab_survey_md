# Equity in Ocean Access (Benefits and Barriers (bab))
# Manuela Diaz based on Jennifer Selgrath code 
## California Marine Sanctuary Foundation/ CINMS

# goal: clean activity data Q4 & Q5 for long data

# ----------------------------------------------------------
# load libraries ######-------------------------------------
library(tidyverse)
library(scales)
library(colorspace)

# --------------------------------------------------------------------------
# load data ######-----------------------------------------------------------
rm(list = ls(all = TRUE))
setwd("/Users/manu/Desktop/research/r_projects/bab_survey_md")

d0<-read_csv("./results/data_long3.csv")%>%
  mutate(Q4 = as.character(Q4))%>%  # all activities
  mutate(Q5 = as.character(Q5))%>%  # important activity
  glimpse()
d0

d1<-d0

unique(d1$Q4)
unique(d1$Q5)

# simplify responses 
d1$Q4<-str_replace_all(d1$Q4,"Other, please specify:","Another activity")
d1$Q5<-str_replace_all(d1$Q5,"Other, please specify:","Another activity")

d1$Q4<-str_replace_all(d1$Q4,"None of the above","Another activity")
d1$Q5<-str_replace_all(d1$Q5,"None of the above","Another activity")

d1$Q4<-str_replace_all(d1$Q4,"Other","Another activity")
d1$Q5<-str_replace_all(d1$Q5,"Other","Another activity")

# put commas as / so separate correctly below
d1$Q4<-str_replace_all(d1$Q4,"Bicycling, roller skating, skateboarding, etc.","Bicycling/Roller skating/Skateboarding")
d1$Q5<-str_replace_all(d1$Q5,"Bicycling, roller skating, skateboarding, etc.","Bicycling/Roller skating/Skateboarding")

d1$Q4<-str_replace_all(d1$Q4,"Bicycling, roller skating, skateboarding, etc","Bicycling/Roller skating/Skateboarding")
d1$Q5<-str_replace_all(d1$Q5,"Bicycling, roller skating, skateboarding, etc","Bicycling/Roller skating/Skateboarding")

d1$Q4<-str_replace_all(d1$Q4,"Festivals with music and/or food","Festivals")
d1$Q5<-str_replace_all(d1$Q5,"Festivals with music and/or food","Festivals")

d1$Q4<-str_replace_all(d1$Q4,"Volunteering (e.g., beach clean-ups)","Volunteering")
d1$Q5<-str_replace_all(d1$Q5,"Volunteering (e.g., beach clean-ups)","Volunteering")

d1$Q4<-str_replace_all(d1$Q4,"Sailing/Boating (engine powered)","Sailing/Boating")
d1$Q5<-str_replace_all(d1$Q5,"Sailing/Boating (engine powered)","Sailing/Boating")

d1$Q4<-str_replace_all(d1$Q4,"Beach games or sports (e.g., frisbee, volleyball, yoga)","Beach games or sports")
d1$Q5<-str_replace_all(d1$Q5,"Beach games or sports (e.g., frisbee, volleyball, yoga)","Beach games or sports")

d1$Q4<-str_replace_all(d1$Q4,"Group/Family gatherings or activities","Group or family gatherings or activities")
d1$Q5<-str_replace_all(d1$Q5,"Group/Family gatherings or activities","Group or family gatherings or activities")

d1$Q4<-str_replace_all(d1$Q4,"Group or family gatherings or activities (e.g., family outing, bbq)","Group or family gatherings or activities")
d1$Q5<-str_replace_all(d1$Q5,"Group/Family gatherings or activities","Group or family gatherings or activities")

d1$Q4<-str_replace_all(d1$Q4,"Meditation, reading, and/or relaxing","Meditation/Reading/Relaxing")
d1$Q5<-str_replace_all(d1$Q5,"Meditation, reading, and/or relaxing","Meditation/Reading/Relaxing")

d1$Q4<-str_replace_all(d1$Q4,"Swimming or bodysurfing","Swimming/Bodysurfing")
d1$Q5<-str_replace_all(d1$Q5,"Swimming or bodysurfing","Swimming/Bodysurfing")

d1$Q4<-str_replace_all(d1$Q4,"Walking or running","Walking/Running")
d1$Q5<-str_replace_all(d1$Q5,"Walking or running","Walking/Running")

d1$Q4<-str_replace_all(d1$Q4,"Observing or photographing nature or wildlife, outdoor education","Observing/Photographing/Education")
d1$Q5<-str_replace_all(d1$Q5,"Observing or photographing nature or wildlife, outdoor education","Observing/Photographing/Education")

d1$Q4<-str_replace_all(d1$Q4,"Observing or photographing nature or wildlife, outdoor education","Observing/Photographing/Education")
d1$Q5<-str_replace_all(d1$Q5,"Observing or photographing nature or wildlife, outdoor education","Observing/Photographing/Education")

d1$Q4<-str_replace_all(d1$Q4,"Driving or sitting in your car to enjoy the views/sunsets","Driving/Sitting in car")
d1$Q5<-str_replace_all(d1$Q5,"Driving or sitting in your car to enjoy the views/sunsets","Driving/Sitting in car")

d1$Q4<-str_replace_all(d1$Q4,"Snorkeling/Scuba Diving","Snorkeling/Scuba diving")
d1$Q5<-str_replace_all(d1$Q5,"Snorkeling/Scuba Diving","Snorkeling/Scuba diving")

d1$Q4<-str_replace_all(d1$Q4,"Fishing or collecting food","Fishing/Collecting food")
d1$Q5<-str_replace_all(d1$Q5,"Fishing or collecting food","Fishing/Collecting food")

d1$Q4<-str_replace_all(d1$Q4,"Beach games or sports","Beach games/Sports")
d1$Q5<-str_replace_all(d1$Q5,"Beach games or sports","Beach games/Sports")

d1$Q4<-str_replace_all(d1$Q4,"Beach games or sports","Beach games/Sports")
d1$Q5<-str_replace_all(d1$Q5,"Beach games or sports","Beach games/Sports")


# eliminate sticky ()
d1$Q4<-str_replace_all(d1$Q4, "\\s*\\([^)]*\\)", "")
d1$Q5<-str_replace_all(d1$Q5, "\\s*\\([^)]*\\)", "")

unique(d1$Q4)
unique(d1$Q5)

#Check unique Q4: 
valid_acts <- unique(d1$Q5) %>% na.omit()

d1 %>%
  separate_rows(Q4, sep = ",") %>%
  distinct(Q4) %>%
  anti_join(tibble(Q4 = valid_acts), by = "Q4")

#Check add ","
d1$Q4 <- str_replace_all(
  d1$Q4,
  "Bicycling/Roller skating/SkateboardingDriving/Sitting in car",
  "Bicycling/Roller skating/Skateboarding,Driving/Sitting in car"
)

write_csv(d1,"./results/data_long4.csv")


unique(d1$Q45)
