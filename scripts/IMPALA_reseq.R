## IMPALA Data ##

setwd("H:/ARES/IMPALA")
library(readxl)
library(dplyr)
library(tidyverse)
install.packages("xlsx")
library("xlsx")
install.packages("openxlsx")
library(openxlsx)

# load in data

IMPALA_key <- read_excel("impala master list 4.7.21.xlsx", sheet = "KEY")
IMPALA_list <- read_excel("isolates to Re-sequence_LC.xlsx")

# combine the key data with the list of isolates to be regrown in order to match 
# isolate numbers with freezer locations

# rename column in one dataset to match the other for joining purposes

colnames(IMPALA_key)
colnames(IMPALA_list)

IMPALA_key2 <- IMPALA_key %>%
  rename("Isolate ID" = "ISOLATE NUMBERS")

# merge data 

IMPALA_merged <- IMPALA_list %>%
  left_join(IMPALA_key2, by="Isolate ID")

# how many boxes contain needed isolates?

length(unique(IMPALA_merged$BOX))
table(IMPALA_merged$BOX)  # 15

### COLLATE EXPERIMENTAL DATA WITH BOTSWANA IDS ###

IMPALA_11.19.22 <- read_excel("IMPALA_RESEQ_11.19.22.xlsx", sheet = "Data")
IMPALA_Master <- read_excel("impala master list 4.7.21.xlsx", sheet = "RESULTS")

IMPALA_Master2 <- IMPALA_Master %>%
  rename("Isolate ID" = "ISOLATE NUMBERS")

IMPALA_Master_merged <- IMPALA_Master2 %>%
  left_join(IMPALA_merged, by="Isolate ID")

# write to excel file
write.xlsx(IMPALA_Master_merged, file = "IMPALA_Master_merged.xlsx", sheetName = "Sheet1", 
           colNames = TRUE, rowNames = TRUE, append = FALSE)

###############################################################
###############################################################
###############################################################

# IMPALA-2 Re-sequencing Efforts II #

# a LV report was pulled on 5-17-2023; need to collate that 
# list with the list provided by Evan of the isolates that 
# need worked up in order to get their freezer locations

# load LV data
impala_report <- read.csv("IMPALA_LVreportpull_5172023.csv")

# load list of isolates needing worked up
impala_seq <- read_excel("2023-04-19_IMPALA_fail_QC_genomes.xlsx", sheet = "LabVantage")

# rename one of the columns containing IDs for merge purposes
impala_report <- impala_report %>%
  rename("lab_id" = "External.Participant.ID")

# remove spaces in lab_id from impala_seq data
impala_seq <- as.data.frame(
  apply(impala_seq,2, function(x) gsub("\\s+", "", x)))

# merge based on lab_id

impala_merged2 <- impala_report %>%
  inner_join(impala_seq, by="lab_id")


# get rid of cell lysates (keep only bact cell iso)

impala_merged2 <- subset(impala_merged2, Sample.Type != "Cell Lysate")

# write to an excel file 
write.xlsx(impala_merged2, file = "IMPALA_Master_merged2.xlsx", sheetName = "Sheet1", 
           colNames = TRUE, rowNames = TRUE, append = FALSE)

# lab_ids seem to be missing from the newly merged data set. which are missing?
setdiff(impala_seq$lab_id, impala_merged2$lab_id)

# BOS113-01A
# ERROR -- fine
# PMH144-01A

# load in last year's re-seq masterfile (a list of everything we worked up)

masterlist2022 <- read_excel("IMPALA_RESEQ_masterfile.xlsx", sheet = "Data")

# find missing isolates in masterlist

# where are these in LV? 
colnames(masterlist2022)

masterlist2022[masterlist2022$`Isolate ID` == "PMH024-02", ]
# this isolate was worked up, where is it in LV?

impala_report[impala_report$lab_id == "PMH024-02"]
# not listed

# check original masterlist
impala_masterlist <- read_excel("IMPALA Master List 4.7.21.xlsx", sheet = "RESULTS")
# is this isolate in the original masterlist?

impala_report[impala_report$lab_id == "PMG144-01A", ]

##########################################
# are any specific plates problematic? 
##########################################
# load plate data
plate1 <- read_excel("IMPALA2 Plate Maps_2023.xlsx", sheet = "genome_plate_1", col_names = FALSE)
plate2 <- read_excel("IMPALA2 Plate Maps_2023.xlsx", sheet = "genome_plate_2", col_names = FALSE)
plate3 <- read_excel("IMPALA2 Plate Maps_2023.xlsx", sheet = "genome_plate_3", col_names = FALSE)
plate4 <- read_excel("IMPALA2 Plate Maps_2023.xlsx", sheet = "genome_plate_4", col_names = FALSE)
plate5 <- read_excel("IMPALA2 Plate Maps_2023.xlsx", sheet = "genome_plate_5", col_names = FALSE)

# elongate data to form list rather than table
plate1<-data.frame(lab_id=unlist(plate1))
plate2<-data.frame(lab_id=unlist(plate2))
plate3<-data.frame(lab_id=unlist(plate3))
plate4<-data.frame(lab_id=unlist(plate4))
plate5<-data.frame(lab_id=unlist(plate5))

# add column with plate number assignment
plate1<-plate1 %>%
  mutate(plate_num = 1)

plate2<-plate2 %>%
  mutate(plate_num = 2)

plate3<-plate3 %>%
  mutate(plate_num = 3)

plate4<-plate4 %>%
  mutate(plate_num = 4)

plate5<-plate5 %>%
  mutate(plate_num = 5)

# combine all data frames vertically
platemaps <- rbind(plate1, plate2, plate3, plate4, plate5)

# remove spaces in lab_id
platemaps<- as.data.frame(
  apply(platemaps,2, function(x) gsub("\\s+", "", x)))

# merge dataframes
platemaps2 <- platemaps %>%
  inner_join(impala_seq, by="lab_id")

## no particular plates with problems; issue is distributed pretty evenly
## across all 5 plates


