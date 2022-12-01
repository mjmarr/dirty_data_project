#import libraries
library(tidyverse)
library(janitor)
library(readxl)
library(here)


#VARIABLES
#AGE COLUMN
#Nominal missing = blank
#AD= adult
#SUBAD = subadult
#IMM= immature
#JUV = juvenile

# <- c("AD", "SUBAD", "IMM", "JUV")
#age_pattern <- str_c("(?i)",age_vals, collapse = "|")
#

#PLPHASE COL
#Ordinal missing = blank
#DRK = dark
#INT = Intermediate
#LGHT = light
#WHITE = white

#plphase_vals <- c("DRK", "INT", "LGHT", "WHITE", "LIGHT")
#plumage_pattern <- str_c("(?i) ", plphase_vals, collapse = "|")


#WANPLUM [SOMETHING PL{NUM}]

#1 = all brown
#2 = (brown plumage) breaking
#3 = white patch on wing
# = wing patch breaking
#5 = white
#wanplum_pattern <- " PL[0-9]"
#print(str_extract(c(" PL2abaawda", "abc PL9"), wanplum_pattern))

#big pattern doesn't work as :(


#FUNCTIONS
#bird data cleaning function
bird_cleaner <- function(df) {
  age_vals <- c("AD| SUBAD| IMM| JUV| M|")
  plphase_vals <- c("DRK| INT| LGHT| WHITE| LIGHT")
  wanplum_pattern <- " PL[0-9]"
  
  clean_df <- df %>%
    #alter some names of columns
    rename(
      bird_record = record,
      species = species_common_name_taxon_age_sex_plumage_phase,
      species_scientific = species_scientific_name_taxon_age_sex_plumage_phase
    ) %>%
    
    #grab relevant data
    select(bird_record,
           record_id,
           species,
           species_scientific,
           species_abbreviation,
           count) %>%
    
    #remove unwanted strings from 3 different columns
    mutate(
      # #remove unwanted age (ad / adsub etc..)
      species = str_remove(species, age_vals),
      species = str_remove(species, plphase_vals),
      species = str_remove(species, wanplum_pattern),
      #lowecase all vales in each column
      species = str_to_lower(species),
      #remove - from species name
      species = str_remove(species, "-"),
      
      species_abbreviation = str_remove(species_abbreviation, age_vals),
      species_abbreviation = str_remove(species_abbreviation, plphase_vals),
      species_abbreviation = str_remove(species_abbreviation, wanplum_pattern),
      
      species_scientific = str_remove(species_scientific, age_vals),
      species_scientific = str_remove(species_scientific, plphase_vals),
      species_scientific = str_remove(species_scientific, wanplum_pattern),
      
      #to lower
      species_scientific = str_to_lower(species_scientific),
      species_abbreviation = str_to_lower(species_abbreviation),
    )
  
  
  return(clean_df)
}

#clean ship data / pull relevent data for analysis tasks
ship_cleaner <- function(df) {
  clean_df <- df %>%
    #rename one column
    rename(ship_record = record) %>%
    #grab relevant data for analysis
    select(ship_record, record_id, lat, long, date)
  
  return(clean_df)
}

#function to merge data-sets (bird_df = clean bird data, ship_df = clean ship data)
merge_birds_and_ships <- function(bird_df, ship_df) {
  clean_data <- bird_df %>%
    inner_join(ship_df, by = "record_id")
  
  return(clean_data)
}

#load data
#raw_data_all <- read_xls(here("data/raw_data/seabirds.xls"))

#check_for_sheets <- excel_sheets(here("data/raw_data/seabirds.xls"))
#check_for_sheets

#[1] "Ship data by record ID" "Bird data by record ID" "Ship data codes" "Bird data codes"

## DATA for Seabirds
bird_data_raw <- read_excel(here("data/raw_data/seabirds.xls"), 2) %>% clean_names()
#bird_codes_raw <- read_excel(here("data/raw_data/seabirds.xls"), 4) %>% clean_names()

##DATA for ships
ship_data_raw <- read_excel(here("data/raw_data/seabirds.xls"), 1) %>% clean_names()
#ship_codes_raw <- read_excel(here("data/raw_data/seabirds.xls"), 3) %>% clean_names()


#clean each set individually
clean_ship_data <- ship_cleaner(ship_data_raw)
clean_bird_data <- bird_cleaner(bird_data_raw)


#merge data set
merged_clean_data <- merge_birds_and_ships(clean_bird_data, clean_ship_data)

#clean & join data sets
#merged_clean_data <- merge_birds_and_ships(bird_cleaner(bird_data_raw), ship_cleaner(ship_data_raw))

#save cleaned merged data set ready for analysis
write_csv(merged_clean_data ,"data/clean_data/seabirds_clean.csv")

#remove values from environment ready for analysis
rm(list=ls())