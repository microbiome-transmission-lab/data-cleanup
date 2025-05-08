library(tidyverse)
library(janitor)
library(dplyr)
library(lubridate)
library(readr)
library(readxl)

#import data, LV and CHOP Data
CHOP_endo <- read.csv("data/resp_CHOP.csv")
LV <- read.csv("data/MSPACMAN_LV_April.csv") 

#clean names
CHOP_clean_endo <- clean_names(CHOP_endo)
LV_clean <- clean_names(LV)
LV_clean <- LV_clean[-(1:2),]

#Clean df and only get days where endo samples were collected
CHOP_clean_endo <- CHOP_clean_endo[which(CHOP_clean_endo$was_a_sample_collected %in% "Yes"), ]
CHOP_clean_endo <- subset(CHOP_clean_endo, select= c(1,2,4,5,6))
CHOP_clean_endo$date <- as.Date(CHOP_clean_endo$date, format= "%m/%d/%y")
CHOP_clean_endo <- rename(CHOP_clean_endo, participant= subject_id)

#cleaning LV data to only show endo samples in circulation
LV_clean <- subset(LV_clean, select= c(1,4,6,7, 10,16,17))
LV_clean <- LV_clean[which(LV_clean$original_sample_type %in% "EndotrachealAspirate"), ]
LV_clean <- LV_clean[which(LV_clean$disposal_status %in% "null"), ]

#isolating subject number and date and using that on a sorted df to get a running count
LV_clean$external_participant_id <- (parse_number(LV_clean$external_participant_id))
LV_clean$collection_date <- as.Date(LV_clean$collection_date, format= "%m/%d/%Y")
LV_clean <-rename(LV_clean, participant= external_participant_id, date= collection_date)
LV_clean <- arrange(LV_clean, LV_clean$participant, LV_clean$date )

#Code to make the week and year dates columns, used for the first match
LV_clean$week <- epiweek(LV_clean$date)
LV_clean$year <- year(LV_clean$date)
CHOP_clean_endo$week <- epiweek(CHOP_clean_endo$date)
CHOP_clean_endo$year <-year(CHOP_clean_endo$date)

#Creating a "count" variable that counts what number of the week the sample is, resets after every week and when the participant changes
#Data will then be joined based on the participant, the week and year information as well as the newly created count
CHOP_clean_endo <- mutate(CHOP_clean_endo, count= consecutive_id(date), .by= c(week, participant)) 
LV_clean <- mutate(LV_clean, count= consecutive_id(date), .by= c(year, week, participant)) 
LV_CHOP_endo <- full_join(CHOP_clean_endo, LV_clean, by= c("participant", "week", "year", "count"))

#Cleaning up the created dataframe  
LV_CHOP_endo<- rename(LV_CHOP_endo, CHOP_date = date.x, LV_date = date.y)
LV_CHOP_endo <- LV_CHOP_endo|>
  relocate(LV_date, .after = CHOP_date)

#Making a date aligned column: shows whether the dates are matching or not
LV_CHOP_endo<- LV_CHOP_endo |> mutate(dates_aligned= ifelse(CHOP_date == LV_date, "Yes", "No")) 
LV_CHOP_endo <- relocate(LV_CHOP_endo, dates_aligned, .after=LV_date)

#Making date difference column: shows how far apart the difference is. Useful for spotting for serious errors
LV_CHOP_endo$date_difference <- with(LV_CHOP_endo,difftime(CHOP_date, LV_date, units= "days") )
LV_CHOP_endo$date_difference <- as.numeric(LV_CHOP_endo$date_difference)
LV_CHOP_endo <- relocate(LV_CHOP_endo, date_difference, .after = dates_aligned)

############################################################################################################################################################################################################################
#THESE NEXT LINES OF CODE ONLY NEED TO DO BE DONE IF THE ALIGMENT OF THE DATES FROM THE WEEK AND COUNT ARE INCORRECT. SEVERAL DIFFERENT SCENARIOS ARE SHOWN WITH THE UNIQUE SOLUTION PRESENTED CODE IS IN QUOTES ON DEFAUTLT
############################################################################################################################################################################################################################


#Filtering data to only include the dates that are not aligned, then splitting the dataframe again for further analysis
"  LV_CHOP_endo <- LV_CHOP_endo[-which(LV_CHOP_endo$dates_aligned %in% "Yes"), ]
LV_endo_redux <- subset(LV_CHOP_endo, select= c(1,5,12,13,14,15,16))
CHOP_endo_redux <- subset(LV_CHOP_endo, select= c(1,2,3,4,8))

#Cleaning this redux CHOP data, mainly involves ensuring that only one row of each date exists
CHOP_endo_redux <- arrange(CHOP_endo_redux, CHOP_endo_redux$participant, CHOP_endo_redux$LV_date)
CHOP_endo_redux <- na.omit(CHOP_endo_redux)
CHOP_endo_redux <- distinct(CHOP_endo_redux)
CHOP_endo_redux <- mutate(CHOP_endo_redux, count= consecutive_id(CHOP_date), .by= c(participant)) 

#Now is time to fix the remaining issues with the LV dates broken into 3 different categories:

#The first is for missing dates. This can be fixed by using replace NA and inputting any date we want or a placeholder date that can be used during the count.
#Any NA that still exists in another column will have the row fully removed
LV_endo_redux$LV_date <- replace_na(LV_endo_redux$LV_date, as.Date("2024-04-09"))
LV_endo_redux <- LV_endo_redux[complete.cases(LV_endo_redux[, c('sample')]), ]

#The second category is for ssamples that have an incorrect date that overlaps with another collection date. 
#In this situation we have to search for an identifier and change the date.
LV_endo_redux$LV_date <- case_when(
  LV_endo_redux$sample == "S-230703-00785" ~ "2023-07-04",
  LV_endo_redux$sample == "S-230703-00786" ~ "2023-07-04",
  .default = as.character(LV_endo_redux$LV_date)
)

#The third category is for samples that have an incorrect date that does not overlap with another collection date.
#Now that all other cases have been fixed we can organize and create the count, except the organization will be done by sample ID instead of date
#This will put the samples in the order they were created which should align the count of the incorrect dates with the CHOP count which has the correct date
LV_endo_redux <- arrange(LV_endo_redux, participant, sample)
LV_endo_redux <- mutate(LV_endo_redux, count= consecutive_id(LV_date), .by= c(participant)) 

#Now we match the samples again, clean a little and make the dates characters for some extra work
LV_CHOP_endo_redux <- full_join(CHOP_endo_redux, LV_endo_redux, by= c("participant", "count"))
  LV_CHOP_endo_redux <- LV_CHOP_endo_redux|>
  relocate(LV_date, .after = CHOP_date)
LV_CHOP_endo_redux$LV_date <- as.character(LV_CHOP_endo_redux$LV_date)
LV_CHOP_endo_redux$CHOP_date <- as.character(LV_CHOP_endo_redux$CHOP_date)

#The one side effect of this different form of aligment is that children from samples are not grouped properly.
#This can be fixed by assigning a LV date the proper CHOP date
LV_CHOP_endo_redux$CHOP_date <- case_when(
  LV_CHOP_endo_redux$LV_date == "2023-07-05" ~ "2023-07-05",
  LV_CHOP_endo_redux$LV_date == "2024-04-09" ~ "2024-04-10",
  .default = as.character(LV_CHOP_endo_redux$CHOP_date)
)

#Now we can revert the LV dates to what they actually show on the LabVantage site. All the dates can be converted to date types as well
LV_CHOP_endo_redux$LV_date <- case_when(
  LV_CHOP_endo_redux$LV_date == "2023-07-04" ~ "2023-07-05",
  LV_CHOP_endo_redux$LV_date == "2024-04-09" ~ NA,
  .default = as.character(LV_CHOP_endo_redux$LV_date)
)
LV_CHOP_endo_redux$LV_date <- as.Date(LV_CHOP_endo_redux$LV_date)
LV_CHOP_endo_redux$CHOP_date <- as.Date(LV_CHOP_endo_redux$CHOP_date)

#The same date information columns can be made as before. With the addition of the corrected_date column.
#These will be dates sent to the LIMS team
LV_CHOP_endo_redux<- LV_CHOP_endo_redux |> mutate(dates_aligned= ifelse(CHOP_date == LV_date, "Yes", "No")) 
LV_CHOP_endo_redux <- relocate(LV_CHOP_endo_redux, dates_aligned, .after=LV_date)
LV_CHOP_endo_redux$date_difference <- with(LV_CHOP_endo_redux,difftime(CHOP_date, LV_date, units= "days") )
LV_CHOP_endo_redux$date_difference <- as.numeric(LV_CHOP_endo_redux$date_difference)
LV_CHOP_endo_redux <- relocate(LV_CHOP_endo_redux, date_difference, .after = dates_aligned)
LV_CHOP_endo_redux <- mutate(LV_CHOP_endo_redux, corrected_date = LV_CHOP_endo_redux$CHOP_date)
LV_CHOP_endo_redux <- relocate(LV_CHOP_endo_redux, corrected_date, .after=LV_date)
LV_CHOP_endo_redux <- LV_CHOP_endo_redux[complete.cases(LV_CHOP_endo_redux[, c('corrected_date')]), ]
LV_CHOP_endo_redux <- arrange(LV_CHOP_endo_redux, participant, corrected_date)

#As a final step before export, the non-aligned samples will be selected for
LV_CHOP_endo_redux <- LV_CHOP_endo_redux[-which(LV_CHOP_endo_redux$dates_aligned %in% "Yes"), ]

LV_list <- read_excel("C:/Users/dyltap/Downloads/LabVantage_Date_Changes_TICK606721.xlsx", 2)
LV_list <- rename(LV_list,  "corrected_date" = "SAMPLEFAMILYCOLLECTIONDT", "sample" = "S_SAMPLEID")
join_check <- full_join(LV_CHOP_endo_redux, LV_list, join_by(corrected_date, sample))
#export data out
#write.csv(LV_CHOP_endo_redux, "tabs/CHOP_LV_Endo_Epiweek.csv")    "
