#import libraries
library(tidyverse)
library(janitor)
library(assertr)
library(readxl)
library(here)


#patterns / candly list
#patterns
us_strings <-
  c(
    "usa",
    "us",
    "united states of america",
    "united states",
    "ussa",
    "u.s.a.",
    "murica",
    "usa!",
    "usa (i think but it's an election year so who can really tell)",
    "u.s." ,
    "america" ,
    "units states" ,
    "usa usa usa" ,
    "the best one - usa",
    "usa! usa! usa!",
    "the yoo ess of aaayyyyyy",
    "usa!!!!!!" ,
    "usa! usa!",
    "united sates",
    "sub-canadian north america... 'merica",
    "trumpistan",
    "merica",
    "united stetes",
    "not the usa or canada",
    "usa usa usa usa" ,
    "united  states of america",
    "united state",
    "united staes",
    "usausausa",
    "unhinged states",
    "us of a",
    "unites states",
    "the united states",
    "north carolina",
    "unied states",
    "u s" ,
    "the united states of america" ,
    "unite states",
    "usa? hard to tell anymore..",
    "'merica" ,
    "usas" ,
    "pittsburgh"  ,
    "new york"  ,
    "california" ,
    "i pretend to be from canada, but i am really from the united states.",
    "united stated" ,
    "ahem....amerca" ,
    "new jersey",
    "united ststes",
    "united statss" ,
    "murrika",
    "usaa",
    "alaska",
    "n. america",
    "u s a",
    "united statea",
    "usa usa usa!!!!",
    "cascadia"
  )

uk_strings <-
  c("united kindom","u.k.","uk","scotland","endland","england","united kingdom")

canada_strings <- c(
  "can[a]*"
)

us_pattern <- paste0("(?i)", us_strings, collapse = "|")
uk_pattern <- paste0("(?i)", uk_strings, collapse = "|")
canada_pattern <- paste0("(?i)", canada_strings, collapse = "|") 

#not candy list
not_candy <- c(
  "bonkers_the_board_game",
  "broken_glow_stick",
  "cash_or_other_forms_of_legal_tender",
  "chardonnay",
  "creepy_religious_comics_chick_tracts",
  "dental_paraphenalia", 
  "generic_brand_acetaminophen",
  "healthy_fruit",
  "hugs_actual_physical_hugs",
  "kale_smoothie",
  "lapel_pins",
  "pencils",
  "person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes",
  "real_housewives_of_orange_county_season_9_blue_ray",
  "sandwich_sized_bags_filled_with_boo_berry_crunch",
  "peterson_brand_sidewalk_chalk",
  "vicodin",
  "white_bread",
  "whole_wheat_anything"
)


#functions
#rename specific column names in 2015/2016/2017
rename_cols <- function(df){
  df %>% 
    rename_with(
      ~ case_when(
        #2015
        . == "how_old_are_you" ~ "age",
        . == "are_you_going_actually_going_trick_or_treating_yourself" ~ "going_trick_or_treating",
        #2016
        . == "your_gender" ~ "gender",
        . == "which_state_province_county_do_you_live_in" ~ "state_province",
        . == "which_country_do_you_live_in" ~ "country",
        #2017
        . == "internal_id" ~ "entry_val",
        . == "q1_going_out" ~ "going_trick_or_treating",
        . == "q2_gender" ~ "gender",
        . == "q3_age" ~ "age",
        . == "q4_country" ~ "country",
        . == "q5_state_province_county_etc" ~ "state_province",
        #candy
        . == "x100_grand_bar" ~ "100_grand_bar",
        . == "anonymous_brown_globs_that_come_in_black_and_orange_wrappers" ~ "black_and_orange_wrappers",
        . == "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes" ~ "black_and_orange_wrappers",
        . == "brach_products_not_including_candy_corn" ~ "brach_products",
        . == "vials_of_pure_high_fructose_corn_syrup_for_main_lining_into_your_vein" ~ "vials_of_pure_hfcs",
        . == "candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants" ~ "candy_cheap",
        . == "tolberone_something_or_other" ~ "tolberone",
        . == "chick_o_sticks_we_don_t_know_what_that_is" ~ "chick_o_sticks",
        . == "those_odd_marshmallow_circus_peanut_things" ~ "circus_peanut",
        . == "bonkers_the_candy" ~ "bonkers",
        . == "sourpatch_kids_i_e_abominations_of_nature" ~ "sourpatch_kids",
        . == "sweetums_a_friend_to_diabetes" ~ "sweetums",
        TRUE ~ .))
}

#function to add id column (can use [rowid_to_column("id") instead])
add_id_column <- function(df, year){
  df %>% 
  mutate(
    id = paste(
      year,
      str_c("0", as.character(row_number())),
      sep = "_"),
    .before = 1)
}

#function to fix order of data
order_candy_data <- function(df){
  df %>% 
    select(id,
           age,
           gender,
           country,
           state_province,
           year,
           going_trick_or_treating,
           candy,
           rating)
}

#clean age column
clean_age <- function(df) {
  df$age <- as.numeric(df$age)
  
  df %>% mutate(age = if_else(age >= 2 &
                                age <= 100, age, as.numeric(NA)))
}

clean_gender <- function(df) {
  df %>%
    mutate(gender = case_when(
      gender == "I'd rather not say" ~ "Not_Specified",
      is.na(gender) ~ "Not_Specified",
      TRUE ~ gender
    ))
}

#clean country column
clean_country <- function(df) {
  df %>% mutate(
    country = case_when(
      is.na(country) ~ NA_character_,
      str_detect(country, "[[:digit:]]") ~ NA_character_,
      str_detect(country, us_pattern) ~ "USA",
      str_detect(country, uk_pattern) ~ "UK",
      str_detect(country, canada_pattern) ~ "CANADA",
      TRUE ~ "OTHER"
    )
  )
}

#cleaning specific year 
clean_2015 <- function(df){
  df %>% 
    #select relevant data
    select(2:96,115) %>%
    #remove non candies
    select(!contains(not_candy)) %>% 
    #add id column
    add_id_column(2015) %>% 
    #rename columns
    rename_cols() %>% 
    #add missing columns
    mutate(
      year = 2015,
      gender = NA,
      country = NA,
      state_province = NA, .before = 4) %>% 
    
    #pivot values to candy -> rating
    pivot_longer(
      # 8 - butterfinger : 101 necco_wafers
      cols = "butterfinger":"necco_wafers",
      names_to = "candy",
      values_to = "rating"
    )
}

clean_2016 <- function(df){
  
  df %>%
    #grab from tot:york_peppermint_patties
    select(2:106) %>% 
    #remove non candies
    select(!contains(not_candy)) %>% 
    #add id column
    add_id_column(2016) %>% 
    #rename columns
    rename_cols() %>%
    #add year column
    mutate(year = 2016) %>% 
    
    pivot_longer(
      # 8 - 100_grand_bar : 91 york_peppermint_patties
      cols = "100_grand_bar":"york_peppermint_patties",
      names_to = "candy",
      values_to = "rating"
    )
}


#2017
clean_2017 <- function(df){
  df %>% 
    #select relevant data all columns to q7
    select(1:109) %>% 
    add_id_column(2017) %>% 
    #remove q6 prefix from candy...
    rename_all(~ stringr::str_replace(., regex("^q6_", ignore_case = TRUE), "")) %>%
    #remove non candies
    select(!contains(not_candy)) %>% 
    rename_cols() %>% 
    mutate(year = 2017) %>% 
    
    #pivot
    pivot_longer(
      # 8 - x100_grand_bar : 91 york_peppermint_patties
      cols = "100_grand_bar":"york_peppermint_patties",
      names_to = "candy",
      values_to = "rating"
    )
}

#Load excel files
#import excel data
raw_data_2015 <- readxl::read_excel(here("data/raw_data/boing-boing-candy-2015.xlsx")) %>% 
  janitor::clean_names()
raw_data_2016 <- readxl::read_excel(here("data/raw_data/boing-boing-candy-2016.xlsx")) %>% 
  janitor::clean_names()
raw_data_2017 <- readxl::read_excel(here("data/raw_data/boing-boing-candy-2017.xlsx")) %>% 
  janitor::clean_names()


#clean 2015 / 2016 / 2017 raw
candy_2015 <- clean_2015(raw_data_2015)
candy_2016 <- clean_2016(raw_data_2016)
candy_2017 <- clean_2017(raw_data_2017)


# Merge all 3 data-sets together
merged_data_2015_to_2017 <- bind_rows(candy_2015, candy_2016, candy_2017)


#clean merged data (date/gender/country)
candy_clean_data <- merged_data_2015_to_2017 %>% 
  clean_age() %>% 
  clean_gender() %>% 
  clean_country()

#save data to csv ready for analysis
write_csv(candy_clean_data, "data/clean_data/candy_data_clean.csv")

#remove values from environment ready for analysis
rm(list=ls())