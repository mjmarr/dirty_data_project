#import libraries
library(tidyverse)
library(janitor)
library(assertr)
library(testthat)
library(here)


#functions
#clean_data
clean_data <- function(dirty_data){
  print("...cleaning raw data")
  
  stopifnot(
    nrow(dirty_data) > 0,
    ncol(dirty_data) == 13
  )
  
  cleaned_data <- dirty_data %>% 
  #fix column names
    janitor::clean_names() %>% 
    rownames_to_column("name") %>%
    mutate(name = str_to_upper(name)) %>% 
    rename(javelin = javeline) %>% #spelling mistake *!
    
    #put all events under a specific type column
    #rank not needed due to points highest = winner? ...
    select(!rank) %>% 
    pivot_longer(!c("name","points","competition"),
                 names_to = "event",
                 values_to = "distance_or_time")
  
  print("cleaned data...")
  return(cleaned_data)
}


#validate_clean_data
validate_cleaned_data <- function(data){
  
  data %>% 
    verify(nrow(data) > 0) %>% 
    verify(ncol(data) == 5) %>% 
    verify(c("name", "points", "competition", "event", "distance_or_time") 
           %in% names(data)) %>% 
    verify(!is.na(distance_or_time)) %>% 
    verify(is.numeric(distance_or_time)) %>% 
    verify(is.numeric(points))
  
  #if valid save csv
  print('passed validaton')
  
}


#load raw data
raw_decathlon_data <- read_rds(here("data/raw_data/decathlon.rds"))

#clean raw data
clean_decathlon_data <- clean_data(raw_decathlon_data)

#validate cleaned data (check if its ready to save to a csv)
validate_cleaned_data(clean_decathlon_data)

#save data to csv ready for analysis
write_csv(clean_decathlon_data, "data/clean_data/decathlon_clean.csv")

#remove values from environment ready for analysis
rm(list=ls())