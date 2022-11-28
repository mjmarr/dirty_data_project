#import libraries
library(tidyverse)
library(janitor)
library(assertr)
library(readxl)


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
clean_age <- function(df){
  df$age <- as.numeric(df$age)
  
  df %>% mutate(age = if_else(age >= 2 & age <= 100, age, NA_integer_))
}


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


#Load excel files
#import excel data
raw_data_2015 <- readxl::read_excel("data/raw_data/boing-boing-candy-2015.xlsx") %>% 
  janitor::clean_names()
raw_data_2016 <- readxl::read_excel("data/raw_data/boing-boing-candy-2016.xlsx") %>% 
  janitor::clean_names()
raw_data_2017 <- readxl::read_excel("data/raw_data/boing-boing-candy-2017.xlsx") %>% 
  janitor::clean_names()


#CANDY 2015
#add missing common columns to 2015
candy_2015 <- raw_data_2015 %>% 
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
    state_province = NA, .before = 4)
  

candy_2015 <- candy_2015 %>% 
  pivot_longer(
    # 8 - butterfinger : 101 necco_wafers
    cols = "butterfinger":"necco_wafers",
    names_to = "candy",
    values_to = "rating"
  )

#CANDY 2016
names(raw_data_2016)

candy_2016 <- raw_data_2016 %>%
  #grab from tot:york_peppermint_patties
  select(2:106) %>% 
  #remove non candies
  select(!contains(not_candy)) %>% 
  #add id column
  add_id_column(2016) %>% 
  #rename columns
  rename_cols() %>%
  #add year column
  mutate(year = 2016)

names(candy_2016)

candy_2016 <- candy_2016 %>% 
  pivot_longer(
    # 8 - 100_grand_bar : 91 york_peppermint_patties
    cols = "100_grand_bar":"york_peppermint_patties",
    names_to = "candy",
    values_to = "rating"
  )


names(raw_data_2017)
#CANDY 2017
candy_2017 <- raw_data_2017 %>% 
  #select relevant data all columns to q7
  select(1:109) %>% 
  add_id_column(2017) %>% 
  #remove q6 prefix from candy...
  rename_all(~ stringr::str_replace(., regex("^q6_", ignore_case = TRUE), "")) %>%
  #remove non candies
  select(!contains(not_candy)) %>% 
  rename_cols() %>% 
  mutate(year = 2017)

names(candy_2017)

candy_2017 <- candy_2017 %>% 
  pivot_longer(
    # 8 - x100_grand_bar : 91 york_peppermint_patties
    cols = "100_grand_bar":"york_peppermint_patties",
    names_to = "candy",
    values_to = "rating"
  )

# Merge all 3 data-sets together
merged_data_2015_to_2017 <- bind_rows(candy_2015, candy_2016, candy_2017)

#check all types
map(merged_data_2015_to_2017, class)

#fix age entries
merged_data_2015_to_2017 <- clean_age(merged_data_2015_to_2017)




