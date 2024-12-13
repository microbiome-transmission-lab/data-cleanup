library(tidyverse)
library(janitor)
library(dplyr)
library(readr)
library(readxl)
library(stringr)

#loading in file, specifying width range and assigning column names, using condensed line list from Cerner
cerner_df <- read_fwf("negative_reports/Cerner_Neg_Date.txt", fwf_cols(name= c(1,26), mrn_from_label= c(27,36), recieve_date= c(49,63), procedure= c(64,69), accession_number_scanned= c(76,88)), skip=10, skip_empty_rows = TRUE)

#loading in our mVACS file then converting box mrn to character and padding 0s
box_data <- read_excel("C:/Users/dyltap/Box/mVACS Data Entry/mVACS_data_entry.xlsx")
box_data$mrn_from_label<- as.character(box_data$mrn_from_label)
box_data$mrn_from_label <- str_pad(box_data$mrn_from_label, 9, pad= "0")

#Removing NAs from negative data file
cerner_df <- na.omit(cerner_df)

#joining by accession number, inner join so that results are only from cerner file that have matching mrns to the box file.
matching_negatives <- inner_join(cerner_df, box_data, join_by(mrn_from_label) )

#cleaning up columns
matching_negatives <- matching_negatives |>
  subset(select= -c(4,10,12,13,14,15)) |>
  relocate(accession_number_scanned.y, .after= accession_number_scanned.x)
  

#writing csv
write.csv(matching_negatives, "repeat_negatives/matching_date.csv")

