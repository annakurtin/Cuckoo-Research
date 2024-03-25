##### Create Functions for Cleaning Playback Data ####

# Created 3/25/2024

##### Code #####

# Create a new column for BBCU detections and YBCU detections
create_binary_cuckoo <- function(dataframe){
  updated_dataframe <- dataframe %>%
    mutate(
      bbcu_detection = ifelse(species == "BBCU", 1,
                    ifelse(species %in% c("NOBI", "YBCU"), 0, NA))) %>%
    mutate(
      ybcu_detection = ifelse(species == "YBCU", 1,
                    ifelse(species %in% c("NOBI", "BBCU"), 0, NA)))
  # make bbcu and ybcu a factor????
  return(updated_dataframe)
}


# Clean interval column
# Change 1-5 in interval column to M1 etc
# Logic behind this is to make it the same data type as PB1 etc
clean_intervals <- function(dataframe){
  updated_dataframe <- dataframe %>% mutate(interval = case_when(
    interval == "1" ~ "M1",
    interval == "2" ~ "M2",
    interval == "3" ~ "M3",
    interval == "4" ~ "M4",
    interval == "5" ~ "M5",
    interval == "M1" ~ "M1",
    interval == "M2" ~ "M2",
    interval == "M3" ~ "M3",
    interval == "M4" ~ "M4",
    interval == "M5" ~ "M5",
    interval == "PB1" ~ "PB1",
    interval == "PB2" ~ "PB2",
    interval == "PB3" ~ "PB3",
    interval == "PB4" ~ "PB4",
    interval == "PB5" ~ "PB5"))
  return(updated_dataframe)
}

# clean the ARU ID in the region 6 data
clean_aru_r622 <- function(r6_22_data){
  r6_data_new <- r6_22_data %>% mutate(aru_id = case_when(
    aru_point == "05169_1" ~ "SMM05169",
    aru_point == "05169_2" ~ "SMM05222",
    aru_point == "05169_3" ~ "SMM05075",
    aru_point == "FWP_R6_001_C4" ~ "FWPR6001",
    aru_point == "FWP_R6_002_C5" ~ "FWPR6002",
    aru_point == "FWP_R6_003_C6" ~ "FWPR6003",
    aru_point == "FWP_R6_004_C7" ~ "FWPR6004",
    aru_point == "FWP_R6_005_C8" ~ "FWPR6005",
    aru_point == "FWP_R6_006_C9" ~ "FWPR6006")) 
  r6_data_new <- r6_data_new %>% select(-aru_point)
  return(r6_data_new)
}

# Clean ARUs r5 or R7 2022
clean_arus <- function(dataframe){
  dataframe$aru_id <- str_replace(dataframe$aru_id,"AMU", "AM")
  dataframe$aru_id <- str_replace(dataframe$aru_id," ", "")
  return(dataframe)
}

# Edit the time column to remove the colon
## Also potentially in the future change it to 24 hour time?
clean_time <- function(dataframe){
  dataframe$time <- str_replace(dataframe$time, ":", "")
  return(dataframe)
}
clean_time_wcorrectcolname <- function(dataframe){
  dataframe$survey_site_start_time <- str_replace(dataframe$survey_site_start_time, ":", "")
  return(dataframe)
}

# convert the date column into a date format
make_date_format <- function(dataframe){
  dataframe$date <- as.Date(dataframe$date, format = "%m/%d/%Y")
  # may need to edit this if different dataframes have different formats
  return(dataframe)
}

# rename the long and lat columns in the metadata and select only the columns needed for cleaning playback data
rename_cols_and_select <- function(metadata){
  if("latitude" %in% colnames(metadata)){
    metadata <- metadata %>% rename(lat = latitude)
  }
  if("longitude" %in% colnames(metadata)){
    metadata <- metadata %>% rename(long = longitude)
  }
  metadata_foruse <- metadata %>% select(point_id, aru_id, lat, long)
  return(metadata_foruse)
}

# make a function to join the metadata to the playback data
join_playback_metadata <- function(playback, metadata){
  newdat <- left_join(playback, metadata, by = "aru_id")
  # maybe in the future select/reorder certain columns
}


# make a function to join the metadata to the playback data
join_playback_metadata_bypoint <- function(playback, metadata){
  newdat <- left_join(playback, metadata, by = "point_id")
  # maybe in the future select/reorder certain columns
}

# Create a survey ID column that is just the site, a #, then 1 (since in 2022 only one survey was conducted) 
separate_site_make_survey_id <- function(dataframe){
  dataframe <- dataframe %>% 
    separate(point_id, into = c("site_id", "point"), sep = "-" , remove = FALSE) %>%
    mutate(survey_id = paste(site_id,"#1"))
  dataframe$survey_id <- str_replace(dataframe$survey_id," ", "")
  return(dataframe)
}


# Make a function to create a new column that wasn't entered into the dataset and fill it with "no_data" values
make_unentered_columns <- function(dataframe,column_name){
  dataframe <- dataframe %>% mutate(!!column_name := "no_data")
  return(dataframe)
}


# select the columns in the correct order
reorder_final_cols <- function(dataframe){
  final_dataframe <- dataframe %>% select(obs,
                                          date,
                                          time,
                                          survey_id, 
                                          site_id, 
                                          point_id,
                                          lat,
                                          long,
                                          time, 
                                          interval, 
                                          species, 
                                          bbcu, 
                                          ybcu, 
                                          distance, 
                                          bearing, 
                                          how, 
                                          visual, 
                                          call, 
                                          sex, 
                                          cluster, 
                                          notes)
}

# select the columns in the correct order
reorder_cols <- function(dataframe){
  final_dataframe <- dataframe %>% select(obs,
                                          num_obs,
                                          date,
                                          time,
                                          survey_id, 
                                          site_id, 
                                          point_id,
                                          lat,
                                          long,
                                          time, 
                                          interval, 
                                          species, 
                                          bbcu, 
                                          ybcu, 
                                          distance, 
                                          bearing, 
                                          how, 
                                          visual, 
                                          call, 
                                          sex, 
                                          cluster, 
                                          notes)
}


select_final_metad_cols <- function(dataframe){
  dataframe <- dataframe %>% select(survey_id, 
                                    observer, num_obs, num_points,
                                    date, survey_site_start_time,
                                    lat_avg,long_avg,
                                    sky_start,sky_end,
                                    wind_start,wind_end,
                                    temp_start, temp_end,
                                    detection_hist_bbcu, detection_hist_ybcu,
                                    notes)
}

select_final_pbdata_cols <- function(dataframe){
  dataframe <- dataframe %>% select(observer,
                                    date,
                                    survey_id, 
                                    site_id, 
                                    point_id, 
                                    start_time, 
                                    interval, 
                                    species,                                     
                                    bbcu_detection, 
                                    ybcu_detection,
                                    distance, 
                                    bearing, 
                                    visual, 
                                    call, 
                                    cluster, 
                                    notes 
)
}