import pandas as pd

# Import all the raw data as csv files #
pd.options.display.max_rows = 999
SCDI_data_raw= pd.read_csv('C:\\Users\\dyltap\\Box\\Raw Coding Data\\Brown_Lab\\Data\\brown_samples_raw.csv')
FMT_data_raw= pd.read_csv("C:\\Users\\dyltap\\Box\\Raw Coding Data\\Brown_Lab\\Data\\FMT_samples_raw.csv")

# cleaning date and times for SCDI data #
for column in SCDI_data_raw.loc[:, 'received_date' : 'freezing_time']:
    if column == "received_date":
        SCDI_data_raw[column]= pd.to_datetime(SCDI_data_raw[column])
    else:
        SCDI_data_raw[column]= pd.to_datetime(SCDI_data_raw[column], errors= 'coerce').dt.time
   
SCDI_data_raw['scdi_sample_id']= 'Brown_SCDI_' + SCDI_data_raw['scdi_sample_id'].str.replace('SCDI-', '', regex= True).str.zfill(4)
SCDI_data_clean= SCDI_data_raw


# adding record ID header to index, setting index such that records do not overlap, and making CSV file #
SCDI_data_clean.index.names= ['record_id']
SCDI_data_clean.index+= 1
SCDI_data_clean.to_csv('Data/SCDI_clean_data.csv')

#cleaning date on FMT stool and urine data #
FMT_data_raw['recieved_date']= pd.to_datetime(FMT_data_raw['recieved_date'])
FMT_data_raw['prefix']= FMT_data_raw['fmt_sample_id'].str.extract( r'(R.)')
FMT_data_raw['fmt_sample_id']= "Brown_" + FMT_data_raw['prefix'] + "_" + FMT_data_raw['fmt_sample_id'].str.replace( r'R.' , '', regex= True).str.zfill(4)
FMT_data_raw= FMT_data_raw.drop(columns= ['prefix'])


FMT_data_clean= FMT_data_raw

#adding heading record id header for index and converting to new CSV #
FMT_data_clean.index.names= ['record_id']
FMT_data_clean.index= FMT_data_clean.index + SCDI_data_clean.index[-1] + 1
FMT_data_clean.to_csv('Data/FMT_data_clean.csv')