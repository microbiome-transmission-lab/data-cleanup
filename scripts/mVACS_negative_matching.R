library(tidyverse)
library(janitor)
library(dplyr) 
library(readr)
library(readxl)
library(stringr)
library(formattable)
library(meltr)


#Using Dummy Data to use as the basis for the headers for data 
df_lines <- read_lines("test_cerner_report.txt")
df_lines = df_lines[-(1:9)]
df_lines = df_lines[c(1,3,5,7)]

#Reading in the cerner file using the dummy data for the distance of the columns and specifiying column names
cerner_df <- read_fwf("negative_reports/Cerner_Neg_DATE.txt", fwf_empty(df_lines, col_names= c("name", "mrn_from_label", "location", "receive_date",  "procedure", "accession_number_scanned" )), skip= 10, skip_empty_rows = TRUE)

#loading in our mVACS file then converting box mrn to character and padding 0s
box_data <- read_excel("C:/Users/dyltap/Box/mVACS Data Entry/mVACS_data_entry.xlsx")
box_data$mrn_from_label<- as.character(box_data$mrn_from_label)
box_data$mrn_from_label <- str_pad(box_data$mrn_from_label, 9, pad= "0")

#Removing NAs from negative data file(even though skip_empty_rows should be doing that)
cerner_df <- na.omit(cerner_df)

#joining by mrn as it maintains through date
matching_negatives <- inner_join(cerner_df, box_data, join_by(mrn_from_label) )

#remove duplicates of same person and if the sample was not found
matching_negatives <- distinct(matching_negatives, mrn_from_label, .keep_all = TRUE)
matching_negatives <- matching_negatives[which(matching_negatives$specimen_found_truefalse %in% "TRUE"),]


#cleaning up columns and specify accession numbers
matching_negatives <- subset(matching_negatives,select= c(1,2,4,6,10,11, 12, 13, 15, 18 ))
matching_negatives <- rename(matching_negatives, previous_accession_number = accession_number_scanned.y)
matching_negatives <- rename(matching_negatives, new_accession_number = accession_number_scanned.x)
matching_negatives <- relocate(matching_negatives, previous_accession_number, .after= new_accession_number)

#writing new excel file to save results
write.csv(matching_negatives, "repeat_negatives/matching_DATE.csv")
