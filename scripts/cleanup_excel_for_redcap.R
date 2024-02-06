#' #################################
#' load libraries and set seed
#' #################################
library(tidyverse)
library(readxl)
library(janitor)
library(gt)

set.seed(16)


#' ################################
#' load data and clean data
#' ################################

readxl::read_xlsx("demo_data/Positive_ND_Isolates_Sent_to_MI_comb_sensi_etest_20240125.xlsx", sheet = 1) |> 
  identity() -> merlin_excel

merlin_excel

View(merlin_excel)

merlin_excel |> 
  mutate_all(.funs = ~ replace(.x, grepl(.x, "^NA$|^na$"), NA)) |> 
  View()  

merlin_excel |> 
  mutate_all(.funs = ~ replace(.x, stringr::str_trim(.x) == "NA" | stringr::str_trim(.x) == "na", NA)) |> 
  View()  

merlin_excel |> 
  janitor::clean_names() |> 
  View()  

merlin_excel |> 
  # clean NAs
  mutate_all(.funs = ~ replace(.x, stringr::str_trim(.x) == "NA" | stringr::str_trim(.x) == "na", NA)) |> 
  # clean variable names
  janitor::clean_names() |> 
  # add record_id variable (needs to be unique in REDCap)
  mutate(record_id = dplyr::row_number()) |> 
  select(record_id, everything()) |> 
  identity() -> merlin_clean

merlin_clean

View(merlin_clean)

merlin_clean |> 
  write_csv("demo_data/MERLINDemo_clean_data.csv", na = "")



#' ################################
#' load and expand data dictionary
#' - start a new REDCap project
#' - use online designer to start a data entry form
#' - given the data entry form a name
#' - add one variable called test_variable to the form, with the type "Text Box"
#' - export the Data Dictionary (a csv file)
#' ################################

read_csv("demo_data/MERLINDemo_DataDictionary_2024-02-06.csv") |> 
  identity() -> data_dictionary
data_dictionary

data_dictionary |> 
  filter(`Variable / Field Name` == "test_variable") |> 
  full_join(tibble(`Variable / Field Name` = names(merlin_clean))) |> fill(everything(), .direction = "down") |> 
  mutate(`Field Label` = stringr::str_to_title(gsub("_"," ", `Variable / Field Name`))) |> 
  filter(`Variable / Field Name` != "test_variable") |> 
  identity() -> new_data_dictionary
new_data_dictionary

new_data_dictionary |> 
  write_csv("demo_data/MERLINDemo_DataDictionary_Update.csv")

new_data_dictionary |> 
  write_csv("demo_data/MERLINDemo_DataDictionary_Update.csv", na = "")


#' ################################
#' in REDCap:
#' - check the data entry form to ensure that variables have been added
#' - use the data Import tool to import the clean data (csv file) created above
#' ################################



