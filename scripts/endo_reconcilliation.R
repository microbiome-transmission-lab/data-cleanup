library(tidyverse)
library(janitor)
library(dplyr)
library(lubridate)
library(readr)

#import data, LV and CHOP Data
CHOP_endo <- read.csv("data/CHOP_endo.csv")
LV <- read.csv("data/MSPACMAN_LV_Final.csv") 

#clean names
CHOP_clean_endo <- clean_names(CHOP_endo)
LV_clean <- clean_names(LV)

#Clean df and only get days where endo samples were collected
CHOP_clean_endo <- CHOP_clean_endo[-which(CHOP_clean_endo$event_name %in% "Baseline" ), ]
CHOP_clean_endo <- CHOP_clean_endo[-which(CHOP_clean_endo$event_name %in% "Outcome" ), ]
CHOP_clean_endo <- CHOP_clean_endo[which(CHOP_clean_endo$was_a_sample_collected %in% "Yes"), ]
CHOP_clean_endo <- subset(CHOP_clean_endo, select= c(1,2,4,5,6))

#reformat into date type and create column that has count of each collecction event per subject
CHOP_clean_endo$date <- as.Date(CHOP_clean_endo$date, format= "%m/%d/%y")
CHOP_clean_endo <- rename(CHOP_clean_endo, participant= subject_id)
CHOP_clean_endo <- mutate(CHOP_clean_endo, count= consecutive_id(date), .by = participant)


#cleaning LV data to only show endo samples in circulation
LV_clean <- subset(LV_clean, select= c(1,4,6,7, 10,16,17))
LV_clean <- LV_clean[which(LV_clean$original_sample_type %in% "EndotrachealAspirate"), ]
LV_clean <- LV_clean[which(LV_clean$disposal_status %in% "null"), ]

#isolating subject number and date and using that on a sorted df to get a running count
LV_clean$external_participant_id <- (parse_number(LV_clean$external_participant_id))
LV_clean$collection_date <- as.Date(LV_clean$collection_date, format= "%m/%d/%y")
LV_clean <-rename(LV_clean, participant= external_participant_id, date= collection_date)
LV_clean <- arrange(LV_clean, LV_clean$participant, LV_clean$date )
LV_clean <- mutate(LV_clean, count= consecutive_id(date), .by = participant)

#merge data on participant and count, check for date difference and what that difference is
CHOP_LV_Endo <- full_join(CHOP_clean_endo, LV_clean, by= c("participant", "count"))
CHOP_LV_Endo<- rename(CHOP_LV_Endo, CHOP_date = date.x, LV_date = date.y)
CHOP_LV_Endo <- CHOP_LV_Endo|>
  relocate(CHOP_date, .after= participant) |>
  relocate(LV_date, .after = CHOP_date) |>
  relocate(count, .after= participant)
CHOP_LV_Endo<- CHOP_LV_Endo |> mutate(dates_aligned= ifelse(CHOP_date == LV_date, "Yes", "No")) 
CHOP_LV_Endo <- relocate(CHOP_LV_Endo, dates_aligned, .after=LV_date)

#getting difference in dates
CHOP_LV_Endo$date_difference <- with(CHOP_LV_Endo,difftime(CHOP_date, LV_date, units= "days") )
CHOP_LV_Endo$date_difference <- as.numeric(CHOP_LV_Endo$date_difference)
CHOP_LV_Endo <- relocate(CHOP_LV_Endo, date_difference, .after = dates_aligned)
CHOP_LV_Endo <- CHOP_LV_Endo[-which(CHOP_LV_Endo$dates_aligned %in% "Yes"), ]

#export
write.csv(CHOP_LV_Endo, "tabs/CHOP_LV_Endo_Revised.csv")
