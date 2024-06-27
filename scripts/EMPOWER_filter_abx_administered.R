######################################################
######################################################
# EMPOWER CAP
# Filter medications from data extraction
#
# Author: Leigh Cressman (crel@pennmedicine.upenn.edu)
#
# Restrict inpatient meds to systemically 
# administered abx using abx list 
# created by Lauren
#
# Remove topical, external, inhaled, otic, and
# ophthalmic antibiotics
######################################################
######################################################

################
#  notes
################
# historical meds can be removed if requested: 
# ZC_ORDER_CLASS.NAME != "Historical Med"

# filtered in SQL query:
#   restricted to administered meds (based on list of medication 
#   administration record (MAR) codes validated by Lauren

# abx name not filtered in SQL query due to concern with
# increasing execution time of query


##################
# load library
##################
library(tidyverse)

# for datasets which are too large to read into memory:
# use arrow::read_csv_arrow in place of readr::read_csv
# this is interface to Apache Arrow C++ library
# https://arrow.apache.org/docs/r/


#################################
# function to filter medications
#################################

# added improvement from BJK so that function does not
# access global variable implicitly
abx_include_tibble <- readr::read_csv("abx_names_ld.csv") |> 
    mutate(abx_name = tolower(abx_name))

filter_meds <- function(df, abx_include = abx_include_tibble) {
  
  # This function takes a dataframe and a list
  # of abx names stored as a dataframe as input,
  # filters each row on medication name,
  # medication form, and medication route,
  # and returns a dataframe containing only
  # systemically administered antibiotics.
  
  # abx to include argument is called 'abx_include'
  # 'abx_include' is expected to be a tibble, and by default is the tibble called 'abx_include_tibble'
  # Converts from dataframe column to string for filtering
  abx_names <- abx_include |> select(abx_name) |> pull() |>  
    paste(collapse = "|") 
  abx_form <- "cream|gel|lotion|ointment|nebulization|powder|enema" 
  abx_route <- "inhalation|ophthalmic|otic|nebulizer"
  
  # Retains med if name from abx list is contained in med name
  # Does not retain med if med form contains non-systemic keyword
  # Does not retain med if med route contains non-systemic keyword
  # Does not retain med if med name contains keyword from med route
  df <- df |> 
    filter(grepl(abx_names, name, ignore.case = T) & 
             !grepl(abx_form, form, ignore.case = T) &
             !grepl(abx_route, route, ignore.case = T) &
             !grepl(abx_route, name, ignore.case = T) 
           ) 
}


################
#  main
################
# import test data: inpatient medications 
inpt_meds <- readr::read_csv("./example_inpt_meds.csv")

# apply function to test data
inpt_abx <- filter_meds(inpt_meds)

# check unique med/form/route names 
# make sure they only contain systemic abx
abx_check <- inpt_abx |> distinct(name, form, route) |> 
  arrange(name)

# run above check with each new dataset
# to ensure that function works on new datasets