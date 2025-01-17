library(tidyverse)
library(janitor)
library(dplyr)
library(lubridate)
library(readr)

#import data, LV and CHOP Data
CHOP <- read_csv("MSPACMAN_CHOP.csv")
LV <- read.csv("data/MSPACMAN_LV_Total.csv") 

#clean names
CHOP_clean <- clean_names(CHOP)
LV_clean <- clean_names(LV)

#removing uneeded columns and isolating for samples that came from stool swabs 
LV_clean <-subset(LV_clean, select = c(1,4,6,7, 10,16,17))
LV_clean <- LV_clean[which(LV_clean$original_sample_type %in% "Stool Swab"), ]
LV_clean <- LV_clean[which(LV_clean$disposal_status %in% "null"), ]

#isolating subject number and date and using that on a sorted df to get a running count
LV_clean$external_participant_id <- (parse_number(LV_clean$external_participant_id))
LV_clean$collection_date <- as.Date(LV_clean$collection_date, format= "%m/%d/%y")
LV_clean <-rename(LV_clean, participant= external_participant_id, date= collection_date)
LV_clean <- arrange(LV_clean, LV_clean$participant, LV_clean$date )
LV_clean <- mutate(LV_clean, count= consecutive_id(date), .by = participant)

#Clean up CHOP data to remove uneeded rows and have only stool samples that were collected
CHOP_clean <- subset(CHOP_clean, select = c(1,2,3,4)) 
CHOP_clean <- CHOP_clean[which(CHOP_clean$was_a_stool_specimen_collected_this_week %in% "Yes"),]

#reformat into date type and create column that has count of each collecction event per subject
CHOP_clean$date <- as.Date(CHOP_clean$date, format= "%m/%d/%Y")
CHOP_clean <- rename(CHOP_clean, participant= x1)
CHOP_clean <- mutate(CHOP_clean, count= consecutive_id(date), .by = participant)


#merge files based on particpant number and collection event
CHOP_LV_Data <- full_join(CHOP_clean, LV_clean, by= c("participant", "count"))

#clean up- move date columns next to each other, make TRUE/FALSE column to compare dates and remove uneeded columns
CHOP_LV_Data<- rename(CHOP_LV_Data, CHOP_date = date.x, LV_date = date.y)
CHOP_LV_Data <- CHOP_LV_Data|>
  relocate(LV_date, .after = CHOP_date) |>
  relocate(count, .after= participant)
CHOP_LV_Data<- CHOP_LV_Data |> mutate(dates_aligned= ifelse(CHOP_date == LV_date, "Yes", "No")) 
CHOP_LV_Data <- relocate(CHOP_LV_Data, dates_aligned, .after=LV_date)

#creating new column that has the time difference in days between the two dates, should be 0 if dates are the same
CHOP_LV_Data$date_difference <- with(CHOP_LV_Data,difftime(CHOP_date, LV_date, units= "days") )
CHOP_LV_Data$date_difference <- as.numeric(CHOP_LV_Data$date_difference)
CHOP_LV_Data <- relocate(CHOP_LV_Data, date_difference, .after = LV_date)
CHOP_LV_Data <- CHOP_LV_Data[-which(CHOP_LV_Data$dates_aligned %in% "Yes"), ]


write.csv(CHOP_LV_Data, "tabs/CHOP_LV_Stool_Revised.csv")
