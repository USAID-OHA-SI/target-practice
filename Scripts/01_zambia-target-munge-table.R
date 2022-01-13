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
library(openxlsx)

# GLOBAL VARIABLES --------------------------------------------------------

load_secrets() #glamr function to load credentials for google drive

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
  ungroup() %>% 
  pivot_wider(names_from = fiscal_year, values_from = targets) %>% 
  rename(COP21_target = `2021`,
         COP22_target = `2022`) %>% 
  arrange(desc(fundingagency)) # remove/keep indicators with NA's/zeroes for FY21/FY22?


#Check Pano Data Table - Data Validation Step

    # test <- df_target %>% 
    #   filter(indicator == "CXCA_SCRN",
    #          mech_code == 10227)
    # 
    # sum(test$COP21_target, na.rm = TRUE)

# TABLE ------------------------------------------------------------------

#USING OPEN_XLSX()

#create the excel workbook
wb <- createWorkbook()

#add the blank sheet
addWorksheet(wb, "Sheet 1")

#define style for headers
hs <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                  halign = "center", valign = "center", textDecoration = "Bold",
                  border = "TopBottomLeftRight")

#write data to the sheet and add style
writeData(wb, 1, df_target, borders = "rows", headerStyle = hs)

#adding filters to the columns mech_code, prime_partner, funding_agency, and indicator
addFilter(wb, 1, row = 1, cols = 1:4)

#save workbook and write to dataout folder
saveWorkbook(wb, file = "Dataout/zambia-targets-fy21-fy22.xlsx", overwrite = TRUE)

