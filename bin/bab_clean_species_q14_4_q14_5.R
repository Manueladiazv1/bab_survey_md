# =====================================================
# BAB Valued Species Cleaning & Analysis (Q14_4 + Q14_5)
# Autor: Manuela Díaz
# Last modification: Feb 2026
# Jenny-style: d0, d1, d2...
# Output:
#   - results/species_valued_almost_clean.csv
#   - results/clean_valued_species_long_with_common_and_group.csv
## =====================================================

# --------------------------------------------------------------------------
# load libraries ######------------------------------------------------------
rm(list = ls(all = TRUE))

library(tidyverse)
library(stringr)
library(stringi)
library(tidyr)

# --------------------------------------------------------------------------
# load data (Jenny style) ######---------------------------------------------
setwd("/Users/manu/Desktop/research/r_projects/bab_survey_md")

d0 <- read_csv("./results/data_long_clean.csv", show_col_types = FALSE) %>%
  mutate(
    Q14_4 = as.character(Q14_4),
    Q14_5 = as.character(Q14_5)
  ) %>%
  glimpse()

d0
d1 <- d0

# --------------------------------------------------------------------------
# keep only what you need for this analysis (master d0 stays intact)
# --------------------------------------------------------------------------
d1 <- d1 %>% select(ResponseId, Q5, Q14_4, Q14_5)

# --------------------------------------------------------------------------
# normalize text (small + practical; helps matching in if_else)
# --------------------------------------------------------------------------
norm_txt <- function(x){
  x %>%
    as.character() %>%
    str_trim() %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%  # remove accents
    str_squish() %>%
    na_if("")
}

d2 <- d1 %>%
  mutate(
    Q14_4 = norm_txt(Q14_4),
    Q14_5 = norm_txt(Q14_5)
  ) %>%
  glimpse()

# =====================================================
# SLOT 1: Q14_4 (FULL BLOCK INCLUDED + FIXES)
# FIXES vs your old script:
#   - Removed the old "Mollusks" collapsing line (it was swallowing nudibranchs/clams/snails/etc)
#   - Added COMMON-NAME OVERRIDES at the end (so those specifics remain)
# =====================================================

d3 <- d2 %>%
  mutate(Q14_4 = str_trim(Q14_4)) %>%
  arrange(Q14_4) %>%
  mutate(
    
    # --- “All species” (multilenguaje) ---
    Q14_4 = if_else(Q14_4 == "all",                                "All species", Q14_4),
    Q14_4 = if_else(Q14_4 == "all of them",                        "All species", Q14_4),
    Q14_4 = if_else(Q14_4 == "all species",                        "All species", Q14_4),
    Q14_4 = if_else(Q14_4 == "all species. i have no preference.", "All species", Q14_4),
    Q14_4 = if_else(Q14_4 == "all sea life",                       "All species", Q14_4),
    Q14_4 = if_else(Q14_4 == "(all marine animals) sharks",        "All species", Q14_4),
    Q14_4 = if_else(Q14_4 == "any fishes",                         "All species", Q14_4),
    Q14_4 = if_else(Q14_4 == "any fish",                           "All species", Q14_4),
    Q14_4 = if_else(Q14_4 == "everything in tidepools",            "All species", Q14_4),
    Q14_4 = if_else(Q14_4 == "animals",                            "All species", Q14_4),
    Q14_4 = if_else(Q14_4 == "animales",                           "All species", Q14_4),
    Q14_4 = if_else(Q14_4 %in% c("todos","todas","todo","cualquiera"), "All species", Q14_4),
    Q14_4 = if_else(Q14_4 %in% c("所有","全部","都"),              "All species", Q14_4),
    Q14_4 = if_else(Q14_4 %in% c("animal","beach animals","animal that are there"),
                    "All species", Q14_4),
    
    # --- CLEANING NAMES ---
    
    # 1) MARINE MAMMALS
    Q14_4 = if_else(Q14_4 %in% c("mammals", "marine mammals"),
                    "General marine mammals", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("harbor porpoise","delfin","delfines","dolphin","dolphins","dolfins","dolphine",
                                 "dolohins","dophins","dolphines","dolphins!!","dolpjin",
                                 "bottlenose dolphin","bottlenose dolphins","common dolphin"),
                    "Dolphins", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("wales","porpoise","orcas","orca","killer whiles","killer whale",
                                 "hump whales","whale","whales","ballena","ballenas","whajes",
                                 "beluga whale","blue whale","cetaceans","tiburones ballenas arrecifes"),
                    "Whales", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("sea lion","sea lions","leon marino","leones marinos","lobos marinos",
                                 "sealions","seal lions","sea lion s","california sea lion","ca sea lion",
                                 "california sea lions","sea lian","california sea lions (and all other pinnipeds)"),
                    "Sea lions", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("phoca vitulina","northern elephant seal pup","lepard seals","foca","focas",
                                 "seal","seals","pinnipeds","harbor seals","harbor seal",
                                 "harbor seals (or any pinniped)","las focos","leopard seals","sealsls",
                                 "elephant seal","海豹,海狗","海豹"),
                    "Seals", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("manatees"),
                    "Manatees", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("sea oters","sea otter","sea otters","otter","otters",
                                 "north american river otters (not marine but they swim in the ocean/tidepools)",
                                 "marine otters"),
                    "Otters", Q14_4),
    
    # 2) FISH
    Q14_4 = if_else(Q14_4 %in% c("seafosh","pesces","peses","pescado","peces","small fish",
                                 "different types of fishes","fish","fish in the waves",
                                 "fish jumping out of the water if it's far enough out to see","fishes"),
                    "General fish", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("seaweedfish","sand bass","red snapper","puffer fish","perch","moray eel",
                                 "fish cat","sea bass","white sea bass","bass","calico bass","calicos","garibali",
                                 "girabaldi","steelhead","garibaldi fish","garibaldi","rock fish","rockfish",
                                 "sea horse","sea horses","seahorse","rock cod","rockcod","california sheephead",
                                 "cod","halibut","hallibut","helabut","cowfish","grunion","california grunion"),
                    "Coastal & reef fish", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("tuna","yellowtail","yellowtail tuna","mackarel","makarel","mackerel",
                                 "anchovy","anberjacks","amberjacks"),
                    "Pelagic fish", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("salmon","trucha"),
                    "Anadromous fish", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("manta","mantaraias","rays","bat ray","bat rays",
                                 "stingray","sting ray","stingrays","sting rays",
                                 "shovel nose shark","croaker stingrays"),
                    "Rays", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("sharks","shark species","shark","leopard shark",
                                 "leopard sharks","white shark","white sharks","white tip sharks"),
                    "Sharks", Q14_4),
    
    # 3) INVERTEBRATES
    Q14_4 = if_else(Q14_4 %in% c("shrimp","sand crabs","sand bugs that dig the holes","san crabs","mole crabs",
                                 "sand flea","snow crab","mini crabs","lobsters","lobster",
                                 "hermit crab","hermit crabs","barnacles",
                                 "sand fleas","sea crabs","crawdads","camaron","cangrejo",
                                 "dungeness","dungeness crab","canngrejos",
                                 "crab","crabs","craps","blue crab","crabs/hermit crabs","fiddler crabs"),
                    "Crustaceans", Q14_4),
    
    # (REMOVED) old collapsing-to-Mollusks line here on purpose ✅
    
    Q14_4 = if_else(Q14_4 %in% c("squids","pulpo","octopus","octopi","two spotted octopus"),
                    "Cephalopods", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("urchins","sea urchin","sea urchins","sand dollars","sand dollar",
                                 "pisaster ochraceus","sea cucumbers","estrella","sea star","sea stars",
                                 "star fish","starfish","seastars"),
                    "Echinoderms", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("see anemone","sea anemone","sea anemones","sea anenime","sea anenomes",
                                 "jelly fish","jellyfish","coral","coral reef",
                                 "anemone","anemones","anenomes","giant anemone","green anemone"),
                    "Cnidarians", Q14_4),
    
    # 4) MARINE FLORA & ALGAE
    Q14_4 = if_else(Q14_4 %in% c("kelp","giant kelp","giant marine kelp","macrosystis pyrifera","the kelp forest"),
                    "Kelp", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("seaweeds","kombu","seaweed","algas","algae","sea weed","海藻","海带"),
                    "Seaweed", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("sea grass","seagrass"),
                    "Seagrass", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("cordgrass","hairgrass"),
                    "Saltmarsh plants", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("plants flowers","plants","planta marina","palm tree","palm trees",
                                 "menzies wallflower","lupine","ice plants","american dune grass",
                                 "callifornia poppies","coastal sage scrub","flowering coastal succulents","dudleya"),
                    "Other coastal vegetation", Q14_4),
    
    # 5) BIRDS
    Q14_4 = if_else(Q14_4 %in% c("small birds","marine birds","birds, marine life","large birds","birds","birds/plovers","blrds"),
                    "General birds", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("海鸥","terns","seaguls","seabirds","sea gulls,","prlicans",
                                 "pelicans and other birds","sea birds","sea gulls","sea gull","seagull","seagulls",
                                 "see gulls","gaviotas","pelicans","pelican","pelicans!","brown pelican","brown pelicans",
                                 "brandts commorant","pelicans","cormorant","cormorants"),
                    "Seabirds", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("western snowy plovers","snowy plover","snowy plovers","shorebirds","shore birds",
                                 "sandpipper","sandpipers","sandpiper","sand piper","ridgeways rail","plovers","plover",
                                 "killdeer","avocet"),
                    "Shorebirds", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("peregrine falcon","storks,","ospreys","osprey","herons","hawks","barn owl",
                                 "blue heron","eagles","egrets","great blue heron"),
                    "Raptors", Q14_4),
    
    # 6) HABITAT / ELEMENTS
    Q14_4 = if_else(Q14_4 %in% c("贝壳","shells","seashells","conchas","sea shells"),
                    "Seashells", Q14_4),
    
    Q14_4 = if_else(Q14_4 %in% c("tadpoles","things to see in tide pools like kelp and crabs and corals and anemonies",
                                 "tide pool creatures","tide pool life","tide pools",
                                 "tidepool critters (crabs, mussels, etc)","tidepool crustations","tide pool creatures"),
                    "Tidepool habitat", Q14_4),
    
    # 8) MARINE REPTILES
    Q14_4 = if_else(Q14_4 %in% c("sea turtle","sea turtles","turtle","turtles","green sea turtle",
                                 "leatherback turtle","leatherback sea turtle","pacific green sea turtle",
                                 "green sea turtles","tortugas"),
                    "Turtles", Q14_4),
    
    # ----------------------------------------------------------------------
    # COMMON-NAME OVERRIDES ✅ (keep specifics; this is what we corrected)
    # ----------------------------------------------------------------------
    Q14_4 = if_else(Q14_4 %in% c("nudibranch","nudibranchs","nudibrachs","nudibranches",
                                 "opalescent nudibranch","spanish shawl"),
                    "Nudibranchs", Q14_4),
    Q14_4 = if_else(Q14_4 %in% c("sea snail","sea snails","snails"),
                    "Sea snails", Q14_4),
    Q14_4 = if_else(Q14_4 %in% c("clam","clams","bean clam","coquina clams"),
                    "Clams", Q14_4),
    Q14_4 = if_else(Q14_4 %in% c("mussel","mussels","mussles","mussells","mytilus californianus","muscles"),
                    "Mussels", Q14_4),
    Q14_4 = if_else(Q14_4 %in% c("oyster","oysters"),
                    "Oysters", Q14_4),
    Q14_4 = if_else(Q14_4 %in% c("abalone","abalone.","albane","haliotis rufescens","haliotis fulgens",
                                 "green abalone","black abalone"),
                    "Abalone", Q14_4),
    Q14_4 = if_else(Q14_4 %in% c("chiton","chitons","gumshoe chiton"),
                    "Chitons", Q14_4),
    Q14_4 = if_else(Q14_4 %in% c("sea hare","sea hares","sea hares!!!","black sea hare"),
                    "Sea hares", Q14_4),
    Q14_4 = if_else(Q14_4 %in% c("octopus","octopi","pulpo","squid","squids","humboldt squid","two spotted octopus"),
                    "Cephalopods", Q14_4)
  )

# Non-marine or unclear -> NA (lowercase list, since d2 is lowercased)
to_na_14_4 <- c(
  "huntington beach","axolotls","driftwood","i don't see many","idk",
  "koifish","n","n/a","none","ocean pupps","s","sea enemies","the kraken",
  "water","wildlife","na","oppossums/skunks/bear cubs/raccoons/",
  "nature bees - wool carder bee","lizards","lizzards","dog","ardillas","squirrel",
  "butterflies"
)

d4 <- d3 %>%
  mutate(Q14_4 = if_else(Q14_4 %in% to_na_14_4, NA_character_, Q14_4))

# =====================================================
# SLOT 2: Q14_5 (FULL BLOCK INCLUDED + FIXES)
# FIXES vs your old script:
#   - Removed the old "Mollusks" collapsing line
#   - Added COMMON-NAME OVERRIDES at the end
# =====================================================

d5 <- d4 %>%
  mutate(Q14_5 = str_trim(Q14_5)) %>%
  arrange(Q14_5) %>%
  mutate(
    
    # --- “All species” (multilenguaje) ---
    Q14_5 = if_else(Q14_5 == "all",                                "All species", Q14_5),
    Q14_5 = if_else(Q14_5 == "all of them",                        "All species", Q14_5),
    Q14_5 = if_else(Q14_5 == "all species",                        "All species", Q14_5),
    Q14_5 = if_else(Q14_5 == "all species. i have no preference.", "All species", Q14_5),
    Q14_5 = if_else(Q14_5 == "all sea life",                       "All species", Q14_5),
    Q14_5 = if_else(Q14_5 == "(all marine animals) sharks",        "All species", Q14_5),
    Q14_5 = if_else(Q14_5 == "any fishes",                         "All species", Q14_5),
    Q14_5 = if_else(Q14_5 == "any fish",                           "All species", Q14_5),
    Q14_5 = if_else(Q14_5 == "everything in tidepools",            "All species", Q14_5),
    Q14_5 = if_else(Q14_5 == "animals",                            "All species", Q14_5),
    Q14_5 = if_else(Q14_5 == "animales",                           "All species", Q14_5),
    Q14_5 = if_else(Q14_5 %in% c("todos","todas","todo","cualquiera"), "All species", Q14_5),
    Q14_5 = if_else(Q14_5 %in% c("所有","全部","都"),              "All species", Q14_5),
    Q14_5 = if_else(Q14_5 %in% c("marine life","animal","wildlife","beach animals",
                                 "anything in the water! they all amaze me",
                                 "animal that are there",
                                 "any fish, anything really, being around 30 tiny leopard sharks no problem is one of the coolest yet"),
                    "All species", Q14_5),
    
    # 1) MARINE MAMMALS
    Q14_5 = if_else(Q14_5 %in% c("mammals", "marine mammals"),
                    "General marine mammals", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("dophin","dolphin fish","harbor porpoise","delfin","delfines","dolphin","dolphins",
                                 "dolfins","dolphine","dolohins","dophins","dolphines","dolphins!!","dolpjin",
                                 "bottlenose dolphin","bottlenose dolphins","common dolphin"),
                    "Dolphins", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("whiles","whales when i can afford to go whale watching","wale","orca whales",
                                 "humpback whale","gray whale","blue whales","balena","wales","porpoise","orcas","orca",
                                 "killer whiles","killer whale","hump whales","whale","whales","ballena","ballenas","whajes",
                                 "beluga whale","blue whale","cetaceans","tiburones ballenas arrecifes"),
                    "Whales", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("zalophus californianus","sea lion's","sea lions","ca sea lions","sea lion","sea lions",
                                 "leon marino","leones marinos","lobos marinos","sealions","seal lions","sea lion s",
                                 "california sea lion","ca sea lion","california sea lions","sea lian",
                                 "california sea lions (and all other pinnipeds)"),
                    "Sea lions", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("sealss","seals at la jolla coves, but a fair distance away","elephant seals",
                                 "phoca vitulina","northern elephant seal pup","lepard seals","foca","focas",
                                 "seal","seals","pinnipeds","harbor seals","harbor seal",
                                 "harbor seals (or any pinniped)","las focos","leopard seals","sealsls",
                                 "sea elephants","elephant seal","海豹,海狗","海豹"),
                    "Seals", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("manatees"),
                    "Manatees", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("river otters","sea ottters","sea oters","sea otter","sea otters",
                                 "otter","southern sea otters","otters",
                                 "north american river otters (not marine but they swim in the ocean/tidepools)",
                                 "marine otters"),
                    "Otters", Q14_5),
    
    # 2) FISH
    Q14_5 = if_else(Q14_5 %in% c("various fish","random fish that the local fisherman gets at the redondo pier","pescados",
                                 "other fish in general","big fishes","big fish","seafosh","pesces","peses","pescado","peces",
                                 "small fish","different types of fishes","fish","fish in the waves",
                                 "fish jumping out of the water if it's far enough out to see","fishes"),
                    "General fish", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("surf perch","seabass","opaleye","ling cod","kelp bass","guitar fish","grunions",
                                 "giant sea bass","flat fishes","croaker","corbina","calico","blow fish","black seabass",
                                 "seaweedfish","sand bass","red snapper","puffer fish","perch","moray eel","fish cat","sea bass",
                                 "white sea bass","bass","calico bass","calicos","garibali","girabaldi","steelhead",
                                 "garibaldi fish","garibaldi","rock fish","rockfish","sea horse","sea horses","seahorse",
                                 "rock cod","rockcod","california sheephead","cod","halibut","hallibut","stone fish","helabut",
                                 "cowfish","grunion","california grunion"),
                    "Coastal & reef fish", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("swordfish","mola mola","king macral","common white fish/ sardines",
                                 "tuna","yellowtail","yellowtail tuna","mackarel","makarel","mackerel","anchovy",
                                 "anberjacks","amberjacks"),
                    "Pelagic fish", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("salmon","trucha","trout"),
                    "Anadromous fish", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("stingrayd","mantaray","manta ray","mana rays","manta","mantaraias","rays","bat ray","bat rays",
                                 "stingray","sting ray","stingrays","sting rays",
                                 "shovel nose shark","croaker stingrays"),
                    "Rays", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("whale sharks","tiburones","small sharks","nurf sharks","non-threatening sharks",
                                 "la jolla leopard sharks when i paddle board","horn sharks","great white shark","great whale shark",
                                 "sharks","shark species","shark","leopard shark","leopard sharks","thrashers",
                                 "white shark","white sharks","white tip sharks"),
                    "Sharks", Q14_5),
    
    # 3) INVERTEBRATES
    Q14_5 = if_else(Q14_5 %in% c("螃蟹","the little sand crabs","sea slaters","sand lice","sand crab","little ocean crustaceans",
                                 "little ocean crabs","crustaceans","crabs!","crabs & lobsters","beach crustaceans","shrimp","sand crabs",
                                 "sand bugs that dig the holes","san crabs","mole crabs","sand flea","snow crab","mini crabs","lobsters","lobster",
                                 "hermit crab","hermit crabs","barnacles","sand fleas","sea crabs","crawdads","camaron","cangrejo",
                                 "dungeness","dungeness crab","canngrejos","crab","amphipods","crabs","craps","blue crab",
                                 "crabs/hermit crabs","fiddler crabs"),
                    "Crustaceans", Q14_5),
    
    # (REMOVED) old collapsing-to-Mollusks line here on purpose ✅
    
    Q14_5 = if_else(Q14_5 %in% c("squids","pulpo","octopus","octopi","two spotted octopus"),
                    "Cephalopods", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("sunflower sea star","pacific sand dollars","estrellas marinas","estrellas de mar","estrella marina",
                                 "echinoderms","bat star (seastar)","urchins","sea urchin","sea urchins","sand dollars","sand dollar",
                                 "pisaster ochraceus","sea cucumbers","estrella","sea star","sea stars","star fish","starfish","seastars"),
                    "Echinoderms", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("珊瑚","sea animinies","moon jellyfish","enomones","corral","corals","coral reefs",
                                 "by-the-wind jellyfish","branch coral","anenome","see anemone","sea anemone","sea anemones",
                                 "sea anenime","sea anenomes","jelly fish","jellyfish","coral","coral reef","anemone","anemones",
                                 "anenomes","giant anemone","green anemone"),
                    "Cnidarians", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("planktain"),
                    "Plankton", Q14_5),
    
    # 4) MARINE FLORA & ALGAE
    Q14_5 = if_else(Q14_5 %in% c("underwater kelp","seaweed with the bulbs","laminariales","kelps","kelp forests","kelp beds",
                                 "bullwhip kelp","bull kelp","kelp","giant kelp","giant marine kelp","macrosystis pyrifera","the kelp forest",
                                 "there's giant sea weeds with huge pods that amaze me. i don't know their name yet"),
                    "Kelp", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("silvetia compressa","seawees","seawood","seeweed","sea weeds","nori","algie",
                                 "seaweeds","kombu","seaweed","algas","algae","sea weed","海藻","海带"),
                    "Seaweed", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("海草","eelgrass","sea grass","seagrass"),
                    "Seagrass", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("wet land plants","salt tolerant plant","cordgrass","hairgrass"),
                    "Saltmarsh plants", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("white sage","trees","sea palms","sea ice plant","plantas","palmas","native plants","lupens","grass",
                                 "flowers","flower","coastal plants (like plants in torrey pines reserve)","california coastal sage brush",
                                 "buckwheat","plants flowers","plants","planta marina","palm tree","palm trees","menzies wallflower","lupine",
                                 "ice plants","american dune grass","callifornia poppies","coastal sage scrub",
                                 "flowering coastal succulents","ice plant","native plants and insects!","dudleya"),
                    "Other coastal vegetation", Q14_5),
    
    # 5) BIRDS
    Q14_5 = if_else(Q14_5 %in% c("wild birds that belong to the coast","various birds","pigeons","water birds","patos","pajaros","native birds",
                                 "different kinds of birds (not seagulls)","general birds","bird","any type of bird","aves","avez",
                                 "small birds","marine birds","birds, marine life","large birds","birds","birds/plovers","blrds"),
                    "General birds", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("sea gals","sea gould","pelagic birds","seagulls/other birds","los gavillas","least tern","gulls",
                                 "caspian terns","california brown pelican","ca brown pelican","belted kingfishers","海鸥","terns","seaguls",
                                 "seabirds","sea gulls,","prlicans","pelicans and other birds","sea birds","sea gulls","sea gull",
                                 "seagull","seagulls","see gulls","gaviotas","pelicans","pelican","pelicans!","brown pelican",
                                 "brown pelicans","brandts commorant","pelicans","cormorant","cormorants"),
                    "Seabirds", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("western sandpiper","snowy plover bird","sandpiper and plover","sand pipers","piper birds","beach birds",
                                 "western snowy plovers","snowy plover","snowy plovers","shorebirds","shore birds","sandpipper","sandpipers",
                                 "sandpiper","sand piper","ridgeways rail","plovers","plover","killdeer","avocet"),
                    "Shorebirds", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("night-herons","egret","egrets/herons","burrowing owl","bald eagles",
                                 "peregrine falcon","storks,","ospreys","osprey","herons","hawks","barn owl",
                                 "blue heron","eagles","egrets","great blue heron"),
                    "Raptors", Q14_5),
    
    # 6) HABITAT / ELEMENTS
    Q14_5 = if_else(Q14_5 %in% c("washed up shells","贝壳","shells","seashells","conchas","sea shells"),
                    "Seashells", Q14_5),
    
    Q14_5 = if_else(Q14_5 %in% c("anemone","tidal life","tide pools in general)","tadpoles","tide pool stuff","tidepool animals",
                                 "tidepools","things to see in tide pools like kelp and crabs and corals and anemonies",
                                 "tide pool creatures","tide pool life","tide pools",
                                 "tidepool critters (crabs, mussels, etc)","tidepool crustations"),
                    "Tidepool habitat", Q14_5),
    
    # 8) MARINE REPTILES
    Q14_5 = if_else(Q14_5 %in% c("pacific green sea turtles","sea turtle","sea turtles","turtle","turtles","green sea turtle",
                                 "leatherback turtle","leatherback sea turtle","pacific green sea turtle",
                                 "green sea turtles","tortugas"),
                    "Turtles", Q14_5),
    
    # ----------------------------------------------------------------------
    # COMMON-NAME OVERRIDES ✅ (keep specifics; this is what we corrected)
    # ----------------------------------------------------------------------
    Q14_5 = if_else(Q14_5 %in% c("nudibranch","nudibranchs","nudibrachs","nudibranches",
                                 "opalescent nudibranch","spanish shawl"),
                    "Nudibranchs", Q14_5),
    Q14_5 = if_else(Q14_5 %in% c("sea snail","sea snails","snails"),
                    "Sea snails", Q14_5),
    Q14_5 = if_else(Q14_5 %in% c("clam","clams","bean clam","coquina clams"),
                    "Clams", Q14_5),
    Q14_5 = if_else(Q14_5 %in% c("mussel","mussels","mussles","mussells","mytilus californianus","muscles"),
                    "Mussels", Q14_5),
    Q14_5 = if_else(Q14_5 %in% c("oyster","oysters"),
                    "Oysters", Q14_5),
    Q14_5 = if_else(Q14_5 %in% c("abalone","abalone.","albane","haliotis rufescens","haliotis fulgens",
                                 "green abalone","black abalone"),
                    "Abalone", Q14_5),
    Q14_5 = if_else(Q14_5 %in% c("chiton","chitons","gumshoe chiton"),
                    "Chitons", Q14_5),
    Q14_5 = if_else(Q14_5 %in% c("sea hare","sea hares","sea hares!!!","black sea hare"),
                    "Sea hares", Q14_5),
    Q14_5 = if_else(Q14_5 %in% c("octopus","octopi","pulpo","squid","squids","humboldt squid","two spotted octopus"),
                    "Cephalopods", Q14_5)
  )

# Non-marine or unclear -> NA (lowercase list, since d2 is lowercased)
to_na_14_5 <- c(
  "unidentified creatures in transition between these species","the view","squirrels","t",
  "scuttle bugs","rabbits","flies","d","conejos","catfish","bees","bears","ardilla","huntington beach",
  "ardillas","butterflies","squirrel","dog","dogs","lizards","lizzards",
  "nature bees - wool carder bee","polar bears","bagre","beach","beauty of the waves, surfers",
  "cat","fishing","foreigners","it would be nice is there are sea plants in santa monica",
  "ocean","oyster catchers","penguins","raccoons","sand","snakes","swimmers","the ocean","torrey pine",
  "jelly moon","mushrooms","n/a","oxnard coast","none","water","invertebrates","sea mammals"
)

d6 <- d5 %>%
  mutate(Q14_5 = if_else(Q14_5 %in% to_na_14_5, NA_character_, Q14_5))

# ----------------------------------------------------------
# save “almost clean” WIDE
# ----------------------------------------------------------
dir.create("results", showWarnings = FALSE, recursive = TRUE)
write_csv(d6, "results/species_valued_almost_clean.csv")

# =====================================================
# STEP 4 — pairs logic (INCLUDED, adapted to Q14_4 + Q14_5 only)
# NOTE: one of your cases had 3 items; we use Q14_4_new + Q14_4_new2
# =====================================================

d7 <- d6 %>%
  mutate(
    # ---------------- SLOT 1 ----------------
    Q14_4_new  = NA_character_,
    Q14_4_new2 = NA_character_,
    
    Q14_4_new = if_else(Q14_4 %in% c("alege and crab"),
                        "Seaweed", Q14_4_new),
    Q14_4     = if_else(Q14_4 %in% c("alege and crab"),
                        "Crustaceans", Q14_4),
    
    # 3-item case: big fishes + dolphins + kelp
    Q14_4_new  = if_else(Q14_4 %in% c("big fishes dolphins kelp"),
                         "General fish", Q14_4_new),
    Q14_4_new2 = if_else(Q14_4 %in% c("big fishes dolphins kelp"),
                         "Dolphins", Q14_4_new2),
    Q14_4      = if_else(Q14_4 %in% c("big fishes dolphins kelp"),
                         "Kelp", Q14_4),
    
    Q14_4_new = if_else(Q14_4 %in% c("crabs and small fish"),
                        "Crustaceans", Q14_4_new),
    Q14_4     = if_else(Q14_4 %in% c("crabs and small fish"),
                        "General fish", Q14_4),
    
    Q14_4_new = if_else(Q14_4 %in% c("crabs dolfines"),
                        "Crustaceans", Q14_4_new),
    Q14_4     = if_else(Q14_4 %in% c("crabs dolfines"),
                        "Dolphins", Q14_4),
    
    Q14_4_new = if_else(Q14_4 %in% c("dolphin and sea turtles"),
                        "Dolphins", Q14_4_new),
    Q14_4     = if_else(Q14_4 %in% c("dolphin and sea turtles"),
                        "Turtles", Q14_4),
    
    Q14_4_new = if_else(Q14_4 %in% c("dolphins or whales"),
                        "Dolphins", Q14_4_new),
    Q14_4     = if_else(Q14_4 %in% c("dolphins or whales"),
                        "Whales", Q14_4),
    
    Q14_4_new = if_else(Q14_4 %in% c("dolphins, birds"),
                        "Dolphins", Q14_4_new),
    Q14_4     = if_else(Q14_4 %in% c("dolphins, birds"),
                        "General birds", Q14_4),
    
    Q14_4_new = if_else(Q14_4 %in% c("dolphins, whales"),
                        "Dolphins", Q14_4_new),
    Q14_4     = if_else(Q14_4 %in% c("dolphins, whales"),
                        "Whales", Q14_4),
    
    Q14_4_new = if_else(Q14_4 %in% c("fish and dolphin"),
                        "General fish", Q14_4_new),
    Q14_4     = if_else(Q14_4 %in% c("fish and dolphin"),
                        "Dolphins", Q14_4),
    
    Q14_4_new  = if_else(Q14_4 %in% c("pescado focas eetrellas"),
                         "General fish", Q14_4_new),
    Q14_4_new2 = if_else(Q14_4 %in% c("pescado focas eetrellas"),
                         "Seals", Q14_4_new2),
    Q14_4      = if_else(Q14_4 %in% c("pescado focas eetrellas"),
                         "Echinoderms", Q14_4),
    
    Q14_4_new = if_else(Q14_4 %in% c("seals or sea lions"),
                        "Seals", Q14_4_new),
    Q14_4     = if_else(Q14_4 %in% c("seals or sea lions"),
                        "Sea lions", Q14_4),
    
    Q14_4_new = if_else(Q14_4 %in% c("seals, dilphins"),
                        "Seals", Q14_4_new),
    Q14_4     = if_else(Q14_4 %in% c("seals, dilphins"),
                        "Dolphins", Q14_4),
    
    Q14_4     = if_else(Q14_4 %in% c("seeing fish and dogs that they are so cute"),
                        "General fish", Q14_4),
    
    Q14_4_new = if_else(Q14_4 %in% c("whales dolphins"),
                        "Whales", Q14_4_new),
    Q14_4     = if_else(Q14_4 %in% c("whales dolphins"),
                        "Dolphins", Q14_4),
    
    # ---------------- SLOT 2 ----------------
    Q14_5_new  = NA_character_,
    Q14_5_new2 = NA_character_,
    
    Q14_5_new = if_else(Q14_5 %in% c("ballenas, delfines"),
                        "Whales", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("ballenas, delfines"),
                        "Dolphins", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("coral or kelp"),
                        "Kelp", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("coral or kelp"),
                        "Echinoderms", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("fish (whale watching)"),
                        "General fish", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("fish (whale watching)"),
                        "Whales", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("i'll sea birds and hawks"),
                        "General birds", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("i'll sea birds and hawks"),
                        "Raptors", Q14_5),
    
    # NOTE: you had "nudibranchs, algae, ..." placeholder; keeping exactly as you had it
    Q14_5_new = if_else(Q14_5 %in% c("nudibranchs, algae, ..."),
                        "Mollusks", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("nudibranchs, algae, ..."),
                        "Seaweed", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("rays and sharks"),
                        "Rays", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("rays and sharks"),
                        "Sharks", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("sea lions + seals"),
                        "Sea lions", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("sea lions + seals"),
                        "Seals", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("sea lions and seals"),
                        "Sea lions", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("sea lions and seals"),
                        "Seals", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("sea lions, seal"),
                        "Sea lions", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("sea lions, seal"),
                        "Seals", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("sea lions/seals"),
                        "Sea lions", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("sea lions/seals"),
                        "Seals", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("sea lions sharks"),
                        "Sea lions", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("sea lions sharks"),
                        "Sharks", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("seals/sea lions"),
                        "Sea lions", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("seals/sea lions"),
                        "Seals", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("shells and sea anemones"),
                        "Seashells", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("shells and sea anemones"),
                        "Mollusks", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("shells, seaweed"),
                        "Seashells", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("shells, seaweed"),
                        "Seaweed", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("anemone (tide pools in general)"),
                        "Echinoderms", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("anemone (tide pools in general)"),
                        "Tidepool habitat", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("starfishes and dolphins"),
                        "Echinoderms", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("starfishes and dolphins"),
                        "Dolphins", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("crabs, sea mammals"),
                        "Crustaceans", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("crabs, sea mammals"),
                        "General marine mammals", Q14_5),
    
    Q14_5_new = if_else(Q14_5 %in% c("seals and sea lions"),
                        "Seals", Q14_5_new),
    Q14_5     = if_else(Q14_5 %in% c("seals and sea lions"),
                        "Sea lions", Q14_5)
  )

# =====================================================
# LONG FORMAT (includes pair columns)
# =====================================================

d_long <- d7 %>%
  pivot_longer(
    cols = c(Q14_4, Q14_4_new, Q14_4_new2, Q14_5, Q14_5_new, Q14_5_new2),
    names_to = "slot",
    values_to = "species"
  ) %>%
  filter(!is.na(species), species != "") %>%
  mutate(species = str_squish(species))

# =====================================================
# Add group WITHOUT changing species (your ask)
# =====================================================

d_long2 <- d_long %>%
  mutate(
    species_common = species,
    species_group = case_when(
      species_common %in% c("General marine mammals","Dolphins","Whales","Sea lions","Seals","Otters","Manatees") ~ "Marine mammals",
      species_common %in% c("General fish","Coastal & reef fish","Pelagic fish","Anadromous fish","Rays","Sharks") ~ "Fish",
      species_common %in% c("Crustaceans") ~ "Crustaceans",
      species_common %in% c("Echinoderms") ~ "Echinoderms",
      species_common %in% c("Cnidarians") ~ "Cnidarians",
      species_common %in% c("Plankton") ~ "Plankton",
      species_common %in% c("Mollusks","Nudibranchs","Sea snails","Clams","Mussels","Oysters","Abalone","Chitons","Sea hares","Cephalopods","Seashells") ~ "Mollusks",
      species_common %in% c("Kelp","Seaweed","Seagrass","Saltmarsh plants","Other coastal vegetation") ~ "Marine flora & algae",
      species_common %in% c("General birds","Seabirds","Shorebirds","Raptors") ~ "Birds",
      species_common %in% c("Tidepool habitat") ~ "Habitat elements",
      species_common %in% c("Turtles") ~ "Marine reptiles",
      species_common %in% c("All species") ~ "All species",
      TRUE ~ "Unclassified"
    ),
    # optional readability only
    species_common = case_when(
      species_common == "General fish" ~ "Fish (general)",
      species_common == "General birds" ~ "Birds (general)",
      species_common == "General marine mammals" ~ "Marine mammals (general)",
      TRUE ~ species_common
    )
  )

# ----------------------------------------------------------
# save outputs
# ----------------------------------------------------------
write_csv(d7, "results/species_valued_almost_clean.csv")
write_csv(d_long2, "results/clean_valued_species_long_with_common_and_group.csv")

cat("DONE ✅ Saved:\n",
    " - results/species_valued_almost_clean.csv\n",
    " - results/clean_valued_species_long_with_common_and_group.csv\n")

# Quick QC
d_long2 %>% count(species_group, species_common, sort = TRUE) %>% print(n = 50)
