######################################
##### R TUTORIAL PART 1 ##############
######################################

# INTEGRATE DATA CLEAN UP & WRANGLING #

# 1. set working directory
setwd("Y:/R files")

# 2. install and/or load required packages
library(readxl)
library(tidyverse)
library(dplyr)

#3. load data and store as object in the global environment
integrate <- read_excel("INTEGRATE Isolate Master List_LC.xlsx")

# note that it has defaulted to the first tab; if you wish to
# open another tab, you have to specify:
?readxl
integrate <- read_excel("INTEGRATE Isolate Master List_LC.xlsx", sheet = "Isolate MICs")

# this sheet needs tidied. There are columns with data that aren't actually part 
# of the dataset. Drop columns you don't care about using subset() function

integrate_clean <- subset(integrate, select = -c(16,19,20,21,22))

# now we are left with only relevant information

# for the sake of consistency, let's change the data such that it only has MIC
# values under each drug. eg. instead of "R" under C/T, let's put "8"

# I like to use base R for this
integrate_clean$'C/T'[integrate_clean$`C/T` == "R"] <- 8
integrate_clean$'C/T'[integrate_clean$`C/T` == "S"] <- 0.06
integrate_clean$MEV[integrate_clean$MEV == "R"] <- 16
integrate_clean$MEV[integrate_clean$MEV == "S"] <- 0.008
integrate_clean$OMC[integrate_clean$OMC == "R"] <- 8
integrate_clean$OMC[integrate_clean$OMC == "S"] <- 0.12
integrate_clean$COL[integrate_clean$COL == "R"] <- 4
integrate_clean$COL[integrate_clean$COL == "S"] <- 0.25
integrate_clean$PLZ[integrate_clean$PLZ == "R"] <- 4
integrate_clean$PLZ[integrate_clean$PLZ == "S"] <- 0.12
integrate_clean$IMI[integrate_clean$IMI == "R"] <- 2
integrate_clean$IMI[integrate_clean$IMI == "S"] <- 16
# integrate_clean$'FOS+'[integrate_clean$'FOS+' == "R"] <- 128
# integrate_clean$'FOS+'[integrate_clean$'FOS+' == "S"] <- 64
integrate_clean$DLX[integrate_clean$DLX == "R"] <- 0.12
integrate_clean$DLX[integrate_clean$DLX == "S"] <- 1
integrate_clean$CZA[integrate_clean$CZA == "R"] <- 32
integrate_clean$CZA[integrate_clean$CZA == "S"] <- 0.12
integrate_clean$MERO[integrate_clean$MERO == "R"] <- 8
integrate_clean$MERO[integrate_clean$MERO == "S"] <- 0.25
integrate_clean$ERV[integrate_clean$ERV == "R"] <- 8
integrate_clean$ERV[integrate_clean$ERV == "S"] <- 0.03
integrate_clean$IMR[integrate_clean$IMR == "R"] <- 16
integrate_clean$IMR[integrate_clean$IMR == "S"] <- 0.03

# how many isolates are resistant to colistin?
# dplyr package count() function is easiest to remember:
count(integrate_clean$COL == 4)

# error message! If you want to check if your variable is numeric ahead of time:
is.numeric(integrate_clean$COL) #FALSE

#integrate_clean2 <- integrate_clean %>%
 # mutate_at(('COL'), as.numeric)

# for entire dataset:
# integrate_clean %>%
 # mutate_if(is.character, as.numeric)

# try base R without having to change variable type
length(which(integrate_clean$COL >= 4))

# 139 isolates are resistant to colistin
# what percentage of our isolates is that?
length(na.omit(integrate_clean$COL))
139/636 # 21.85%


#test
