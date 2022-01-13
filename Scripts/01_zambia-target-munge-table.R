# PROJECT: target-practice
# AUTHOR:  T. Essam | N. Petrovic | K. Srikanth
# PURPOSE: Zambia IMs' COP21 and COP22 Target Summary Tool
# LICENSE:  MIT
# DATE:     2022-01-12
# UPDATED:  
# NOTE:

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(tidytext)
library(ggtext)
library(glue)
library(janitor)
library(lubridate)
library(googlesheets4)

# GLOBAL VARIABLES --------------------------------------------------------

load_secrets() #glamr function to load credentials

# IMPORT ------------------------------------------------------------------

df <-  si_path() %>% # routes R to my data folder (use glamr::set_paths() to set up)
  return_latest("Zambia") %>% # returns the latest file in directory based on the pattern
  read_msd() #reads the MSD
        
# MUNGE -------------------------------------------------------------------

df_target <- df %>% 
  filter(fiscal_year %in% c(2021, 2022),
         fundingagency %in% c("HHS/CDC", "USAID"),
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(mech_code, primepartner, fundingagency, indicator, fiscal_year) %>% 
  summarise(across(c(targets), sum, na.rm = TRUE)) %>% 
  pivot_wider(names_from = fiscal_year, values_from = targets) %>% 
  rename(COP21_target = `2021`,
         COP22_target = `2022`) %>% 
  arrange(mech_code)

#Check Pano Data Table - Data Validation Step
test <- df_target %>% 
  filter(indicator == "CXCA_SCRN",
         mech_code == 10227)

sum(test$COP21_target, na.rm = TRUE)

# TABLE ------------------------------------------------------------------

# test <- df %>% 
#   filter(fiscal_year %in% c(2021, 2022)) %>% 
#          #fundingagency %in% c("HHS/CDC", "USAID"),
#         # standardizeddisaggregate == "Total Numerator") %>% 
#   group_by(mech_code, primepartner, fundingagency, indicator, fiscal_year) %>% 
#   summarise(across(c(targets), sum, na.rm = TRUE)) %>% 
#   pivot_wider(names_from = fiscal_year, values_from = targets) %>% 
#   rename(COP21_target = `2021`,
#          COP22_target = `2022`) %>% 
#   filter(indicator == "CXCA_SCRN")

#Question for Tim - is it Total Numerator or totals with no disagg?



  
