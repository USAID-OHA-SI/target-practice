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
  return_latest(pattern = "PSNU_IM_FY19-22.*\\Zambia.zip") %>% # returns the latest file in directory based on the pattern
  read_msd() #reads the MSD
        
# MUNGE -------------------------------------------------------------------

df_target <- df %>% 
  clean_agency() %>% #Tidy up agency names so we don't have to carry along HHS
  filter(fiscal_year %in% c(2021, 2022),
         fundingagency %in% c("CDC", "USAID"),
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(mech_code, mech_name, primepartner, fundingagency, indicator, fiscal_year) %>% 
  summarise(across(c(targets), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = fiscal_year, values_from = targets) %>% 
  rename(COP21_target = `2021`,
         COP22_target = `2022`) %>% 
  arrange(desc(fundingagency)) # remove/keep indicators with NA's/zeroes for FY21/FY22?

# Clean up mechanism names to shorten text
df_target <-
  df_target %>%
  mutate(mech_name = case_when(
    str_detect(mech_name, "DISCOVER-H") ~ "DISCOVER-H",
    str_detect(mech_name, "Stop GBV") ~ "Stop GBV",
    str_detect(mech_name, "Z-CHPP") ~ "Z-CHPP",
    TRUE ~ mech_name), #Clean up the OPU names below so openxlxs writes ok
    mech_name = str_remove_all(mech_name, "\\[Placeholder|\\]|-|[[:digit:]]") %>% trimws(.)
  ) %>%
  relocate(mech_name, .before = mech_code)


#Check Pano Data Table - Data Validation Step

    # test <- df_target %>% 
    #   filter(indicator == "CXCA_SCRN",
    #          mech_code == 10227)
    # 
    # sum(test$COP21_target, na.rm = TRUE)

# TABLE ------------------------------------------------------------------

#USING OPEN_XLSX()

#+1 extra rows for header
tot_rows <- nrow(df_target) + 1
tot_cols <- ncol(df_target)

#create the excel workbook
wb <- createWorkbook()

#add the blank sheet
addWorksheet(wb, "Zambia COP Targets by IM")

#define style for headers
hs <- createStyle(fgFill = "#EAEAEA", halign = "CENTER", textDecoration = "Bold",
                  border = "Bottom", fontColour = "#414042")

#Header row height = 32
setRowHeights(wb, 1, rows = 1, heights = 32)

#set column widths
setColWidths(wb, 1, cols = 1:tot_cols, widths = "auto", hidden = rep(FALSE, length(cols)))

#write data to the sheet and add style
writeData(wb, 1, df_target, headerStyle = hs, borders = "rows")

#adding filters to the columns mech_code, prime_partner, funding_agency, and indicator
addFilter(wb, 1, row = 1, cols = 1:5)

#adding commas for targets column
nbr <- createStyle(numFmt = "#,##0")
addStyle(wb, 1, style = nbr, rows = 2:tot_rows, cols = 6:7, gridExpand = T, stack = T)

#save workbook and write to dataout folder
saveWorkbook(wb, file = "Dataout/zambia-targets-fy21-fy22.xlsx", overwrite = TRUE)

