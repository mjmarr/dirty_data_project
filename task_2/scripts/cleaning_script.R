#import libraries
library(tidyverse)
library(here)

#functions

#' cake_data_cleaner
#' 
#' @description
#' `cake_data_cleaner` cleans & merges two datasets
#'
#' @param
#' `df` cake dataset without the titles for codes
#' `df_code` cake dataset with code
#' @returns merged cleaned dataset ready for analysis
cake_data_cleaner <- function(df, df_code)
{
  cleaned_data <- df %>% 
    rename(cake = Cake) %>% 
    pivot_longer(!cake,
                 names_to = "code",
                 values_to = "quantity"
    ) %>% 
    #join df_code
    left_join(df_code,
             by = c("code" = "code")
    ) %>% 
    #remove na values from quantity
    filter(!is.na(quantity)) %>% 
    #fix ingredient / measurement columns
    mutate(measure = case_when(
      ingredient == "Sour cream cup" ~ "cup",
      TRUE ~ measure
    ),
    ingredient = case_when(
      ingredient == "Sour cream cup" ~ "Sour cream",
      TRUE ~ ingredient
    ),
    ingredient = str_to_lower(ingredient))
  
  return(cleaned_data)
}


#import raw_data
raw_cake_ingredients <- read_csv(here("data/raw_data/cake-ingredients-1961.csv"))
cake_ingredient_code <- read_csv(here("data/raw_data/cake_ingredient_code.csv"))

#clean data
cleaned_cake_data <- cake_data_cleaner(raw_cake_ingredients, cake_ingredient_code)


#save cleaned dataset
write_csv(cleaned_cake_data, "data/clean_data/cake_data_clean.csv")

#1 cup = 8 fluid ounces.
#1 cup = 16 tablespoons.
#1 cup = 48 teaspoons.
#1 cup = 0.5 lbs
#1 cup = 0.25 quart

