###########################################################
### IMPALA-2 isolates selected for long read sequencing ###
###########################################################

# purpose: to combine datasets in order to come up with isolate list
# along with their locations 

setwd("Y:/R files")

# required libraries
library(tidyverse)
library(readxl) #read_excel function
library(dplyr) # rename function

# load list of isolates needed for long read sequencing
# provided by Dr. Evan Snitkin; isolates are in order of priority

longread <- read_excel("impala_longreadiso_selection.xlsx")

# load excel file with locations for these isolates listed

locations <- read_excel("IMPALA Master List 4.7.21.xlsx")

# to combine datasets, I will first pick my "key." I will be using the IMPALA
# isolate identifier as my key, but this variable is called different things 
# in longread and locations, so I will change one to match the other.
colnames(locations) #ISOLATE NUMBERS
colnames(longread) #Sample ID

locations <- locations %>%
  rename(
    "Sample ID" = "ISOLATE NUMBERS")

# both datasets now have the isolate identifiers in the column "Sample ID."
# I will now use a join function to combine these datasets. Left_join(X,y) 
# keeps all values in X, where X will contain our Sample ID

impala_longread_locations <- left_join(longread,locations, by = "Sample ID")

# write this dataframe to an .csv file so that others can use it who may 
# not use R

write.csv(impala_longread_locations,"Y:/R files/impala_longread_locations.csv")

# the .csv file was written into the folder specified



