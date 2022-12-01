#import libraries
library(tidyverse)
library(janitor)
library(readxl)
library(here)


#load data
raw_data_all <- read_xls(here("data/raw_data/seabirds.xls"))

check_for_sheets <- excel_sheets(here("data/raw_data/seabirds.xls"))
check_for_sheets

#[1] "Ship data by record ID" "Bird data by record ID" "Ship data codes" "Bird data codes"

## DATA for Seabirds
bird_data_raw <- read_excel(here("data/raw_data/seabirds.xls"), 2) %>% clean_names()
bird_codes_raw <- read_excel(here("data/raw_data/seabirds.xls"), 4) %>% clean_names()

names(bird_data_raw)


bird_clean_names <- bird_data_raw %>% 
  rename(bird_record = record,
         species = species_common_name_taxon_age_sex_plumage_phase,
         species_scientific = species_scientific_name_taxon_age_sex_plumage_phase)


#AGE COLUMN
#Nominal missing = blank
#AD= adult
#SUBAD = subadult
#IMM= immature
#JUV = juvenile

age_vals <- c("AD", "sUBAD", "IMM", "JUV")
age_pattern <- str_c("(?i)",age_vals, collapse = "|")


#PLPHASE COL
#Ordinal missing = blank
#DRK = dark
#INT = Intermediate
#LGHT = light
#WHITE = white

plphase_vals <- c("DRK", "INT", "LGHT", "WHITE")
plumage_pattern <- str_c("(?i)", plphase_vals, collapse = "|")


#WANPLUM [SOMETHING PL{NUM}]

#1 = all brown
#2 = (brown plumage) breaking
#3 = white patch on wing
# = wing patch breaking
#5 = white
wanplum_pattern <- " PL[0-9]"
#print(str_extract(c(" PL2abaawda", "abc PL9"), wanplum_pattern))

#big pattern doesn't work as :(
#big_pat <- str_c(plumage_pattern, age_pattern, wanplum_pattern, collapse = "|")
#remove test all patterns
#print(str_remove("Wandering albatross sensu lato IMM PL5 ", big_pat))

#grab relevent data
bird_relevent_data <- bird_clean_names %>%
  select(bird_record,
         record_id,
         species,
         species_scientific,
         species_abbreviation,
         count)

#remove unwanted strings from 3 different columns
clean_bird_data <- bird_relevent_data %>% 
          #remove unwanted age (ad / adsub etc..)
  mutate(species = str_remove(species, age_pattern),
         species_scientific = str_remove(species_scientific, age_pattern),
         species_abbreviation = str_remove(species_abbreviation, age_pattern),
         #remove unwanted plumage text
         species = str_remove(species, plumage_pattern),
         species_scientific = str_remove(species_scientific, plumage_pattern),
         species_abbreviation = str_remove(species_scientific, plumage_pattern),
         #remove wanplum
         species = str_remove(species, wanplum_pattern),
         species_scientific = str_remove(species_scientific, wanplum_pattern),
         species_abbreviation = str_remove(species_abbreviation, wanplum_pattern)
         )
      
##DATA for ships
ship_data_raw <- read_excel(here("data/raw_data/seabirds.xls"), 1)
ship_codes_raw <- read_excel(here("data/raw_data/seabirds.xls"), 3)