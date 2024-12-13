###################################
###################################
# EMPOWER IN
# extract med duration from sig
# for outpatient meds ordered at
# discharge

# Author: Jim Harrigan
###################################
###################################

# df outpt_filt was already filtered to include only systemic abx

raw_data <- as_tibble(outpt_filt) %>%
  mutate(sig = tolower(sig),
         name = tolower(name)) 
  
# for meds containing sig
raw_data %>%
  filter(sig != "null" 
  ) %>%
  mutate(
    stated_duration = as.numeric(str_match(sig, "(for\\W)(\\d*)(\\Wday)")[, 3]) ,
    stated_doses = as.numeric(str_match(sig, "(for\\W)(\\d*)(\\Wdose)")[, 3])
  ) %>% 
  mutate( 
    qty_per_dose = case_when(
      str_detect(sig,"^take one") ~ "1", 
      str_detect(sig,"^take\\W*\\d*\\.\\d*") ~ str_match(sig,"(?:\\W*take\\W*)(\\b\\d*\\.\\d*\\b)\\s(?:tablets|tablet|tab|pill|capsule|capsules|ml|mls)\\b")[,2],
      str_detect(sig,"^take") ~ str_match(sig,"(?:\\W*take\\W*)(\\b\\d*\\b)\\s(?:tablets|tab|pill|tablet|capsule|capsules|ml|mls)\\b")[,2],
      str_detect(sig,"^[:digit:]") ~ str_match(sig, "^(\\d*)(?:\\W(tablet|capsule|tablets|ml|mls|tabs|tab))")[,2],
      TRUE ~ 'other'
    )) %>%
  mutate(
    freq_per_day = case_when(
      str_detect(sig,"(mon.*)(wed.*)(fri.*)") ~ "0.428", #' run this first for instances of daily on MWF
      str_detect(sig,"(?<!twice)(?<!two times)(?<!three)(?<!four) daily") ~ "1",
      str_detect(sig, "\\d\\Wtimes a day") ~ str_match(sig,"(\\d)\\Wtimes a day")[,2],
      str_detect(sig, "every other day") ~ "0.5",
      str_detect(sig, "2 times a week") ~ "0.285",
      str_detect(sig, "3 times a week") ~ "0.428",
      TRUE ~ 'OTHER'
    )) %>%
  mutate(
    freq_by_hours = 
      case_when(
        str_detect(sig, "(every)\\W\\d*\\W(hours)") ~ str_match(sig,"(every\\W)(\\d*)(\\Whours)")[,3],
        TRUE ~ 'other'
      )
  ) %>%
  mutate(
    qty_per_dose = as.numeric(qty_per_dose),
    freq_per_day = as.numeric(freq_per_day),
    freq_by_hours = as.numeric(freq_by_hours)
  )%>%
  mutate(
    freq_final = if_else(
      !is.na(freq_per_day), (1)/(freq_per_day)
      ,(freq_by_hours)/(24)
    )
  )  %>%
  mutate(
    duration = case_when(
      !is.na(stated_duration) ~ stated_duration, # First, if it states the duration then we use that
      stated_doses == 1 ~ 1, # Then, if there is only 1 dose we will set duration to 1
      !is.na(stated_doses) ~ stated_doses * freq_final, # If order states for x doses, multiply doses * frequency = days
      TRUE ~ round(((quant_num/qty_per_dose) * freq_final),digits =1 # if unevaluated, then will calculate based on qty / qty per dose * frequency 
      )
    ))  -> coded_sig_data

# If sig was missing, use order start date and end date to calculate duration