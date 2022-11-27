#import libraries
library(tidyverse)
library(janitor)
library(assertr)
library(readxl)


#import excel data
raw_data_2015 <- readxl::read_excel("data/raw_data/boing-boing-candy-2015.xlsx") %>% 
  janitor::clean_names()
raw_data_2016 <- readxl::read_excel("data/raw_data/boing-boing-candy-2016.xlsx") %>% 
  janitor::clean_names()
raw_data_2017 <- readxl::read_excel("data/raw_data/boing-boing-candy-2017.xlsx") %>% 
  janitor::clean_names()


names(raw_data_2015)
names(raw_data_2016)
names(raw_data_2017)

#add missing common columns to 2015
test <- raw_data_2015 %>% 
  add_column(gender = NA,
             country = NA,
             state = NA)



#rename specific column names in 2016 & 2017
rename_cols <- function(df){
  df %>% 
    rename_with(
      ~ case_when(
        #2015
        . == "how_old_are_you" ~ "age",
        . == "are_you_going_actually_going_trick_or_treating_yourself" ~ "going_trick_or_treating",
        #2016
        . == "your_gender" ~ "gender",
        . == "which_state_province_county_do_you_live_in" ~ "state",
        . == "which_country_do_you_live_in" ~ "country",
        #2017
        . == "q1_going_out" ~ "going_trick_or_treating",
        . == "q2_gender" ~ "gender",
        . == "q3_age" ~ "age",
        . == "q4_country" ~ "country",
        . == "q5_state_province_county_etc" ~ "state",
        TRUE ~ .))
}

test2 <- rename_cols(raw_data_2016)

#function to add id column
add_id_column <- function(df, year){
  df %>% 
  mutate(
    id = paste(
      year,
      str_c("0", as.character(row_number())),
      sep = "_"),
    .before = 1)
}

test <- raw_data_2015 %>% 
    addadd_id_column(2015) %>% 
    rename_cols() %>%
    add_column(gender = NA, country = NA, state = NA)

test2 <- raw_data_2016 %>% 
  add_id_column(2016) %>% 
  rename_cols()

test3 <- raw_data_2017 %>% 
  add_id_column(2016) %>% 
  rename_cols() %>% 
  #remove q6 prefix from candy...
  rename_all(~ stringr::str_replace(., regex("^q6_", ignore_case = TRUE), ""))


#select columns if they contain JOY, MEH, DISPAIR 
values <- c("JOY", "MEH", "DISPAIR")

test4 <-  test3 %>% select(id,age,gender,going_trick_or_treating,country,state,(where(~any(. == values, na.rm = TRUE))))
#merge all three data sets



#pivot data longer?
