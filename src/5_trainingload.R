# TRAIL Training Load Data 

## Read in Previous Data
...
fusion_id <- read_csv("data/processed/fusion_ids.csv")

# retrieve last date 
last_date <- data$start_date %>% tail(1) %>% as.Date
last_date <- as.Date("2021-01-01")

# Create sequence of months needed to download
# minus one month to go to last complete month
# do we need to go back further incase people haven't synced their watches and new data is added???
start_dates <- seq(from = last_date, to = Sys.Date() - months(1), by = "month")


########
### Garmin Daily Summary ###
########

# Create empty lists to populate with loop
csvdatalist <- list()

# Loop to download garmin daily summary 
for  (i in start_dates) {
  
  # format dates properly
  i <- as.Date(i, origin = "1970-01-01")
  end_date <- ceiling_date(ymd(i), "month") - days(1) # get end date of month

  start <- as.character(format(as.Date(i), "%d/%m/%Y"))
  end <- as.character(format(as.Date(end_date), "%d/%m/%Y"))
  
  filename = paste0("garmin_daily_", i) # create file name based on start date
  
  # pull data through API
  garmindaily <- pull_smartabase(
    form = "Garmin Daily Summary",
    start_date = start,
    end_date = end) %>%
    inner_join(., fusion_id, by = "user_id") %>% # join trail ids
    select(id, everything(), -about) # remove name column

  
  # remove nested HR sample columnn and write to csv
  csvdata <- garmindaily %>%
    select(-`Heart Rate Samples`)
  csvdatalist[[start]] <- csvdata # add each download as a list element
  
  # keep HR sample data, write to JSON
  garmindaily %>%
    mutate(hr_samples = map(`Heart Rate Samples`, ~read.table(text = ., sep = ",", header = TRUE))) %>% # map each comma separated sample to read as a table and nest dataframe
    select(-'Heart Rate Samples') %>%
    toJSON(.) %>%
    write(., file = paste0("data/processed/training_load/", filename, ".json"))
  
  gc()
}

# bind together lists into one data frame
csvdatalist <- csvdatalist %>% bind_rows() 

newcsv <- bind_rows(originaldata, csvdatalist)

write.csv(csvdatalist, "data/processed/training_load/Garmin Daily Summary.csv")



########
## Garmin ###
########

# Create empty list
csvdatalist <- list()

# Loop to download garmin daily summary 
for  (i in start_dates) {
  
  # format dates properly
  i <- as.Date(i, origin = "1970-01-01")
  end_date <- ceiling_date(ymd(i), "month") - days(1) # get end date of month

  start <- as.character(format(as.Date(i), "%d/%m/%Y"))
  end <- as.character(format(as.Date(end_date), "%d/%m/%Y"))
  
  # pull data through API
  garminactivity <- pull_smartabase(
    form = "Garmin Activity Summary",
    start_date = start,
    end_date = end) %>%
    inner_join(., fusion_id, by = "user_id") %>% # join trail ids
    select(id, everything(), -c(about, UUID)) # remove name column and duplicated UUID column
  
  # Writes each individual piece of data to a row 
  garminactivity %>%
    filter(!is.na(Samples)) %>% # only take the data with the Samples
    mutate(path = paste0(id, "_", event_id,".csv")) %>% # combine for a temp new id (person id + activity id)
    select(path, Samples) %>%
    pwalk(~write(x = .y, file = paste0("data/processed/training_load/event_data/", .x))) # write each comma delimitted data entry to file
  
  # remove nested granular data sample column and write to csv
  csvdata <- garminactivity %>%
    select(-Samples)
  csvdatalist[[start]] <- csvdata # add each download as a list element
  
}

# bind together lists into one data frame
csvdatalist <- csvdatalist %>% bind_rows() 

newcsv <- bind_rows(originaldata, csvdatalist)

write.csv(csvdatalist2, "data/processed/training_load/garmin_activity.csv")



########
### Apple Health Kit Daily Summary ####
########

csvdatalist <- list()

# Loop to download garmin daily summary 
for  (i in start_dates) {
  
  # format dates properly
  i <- as.Date(i, origin = "1970-01-01")
  end_date <- ceiling_date(ymd(i), "month") - days(1) # get end date of month
  start <- as.character(format(as.Date(i), "%d/%m/%Y"))
  end <- as.character(format(as.Date(end_date), "%d/%m/%Y"))
  
  # pull data through API
  healthkitdaily <- pull_smartabase(
    form = "HealthKit Daily Summary",
    start_date = start,
    end_date = end) %>%
    inner_join(., fusion_id, by = "user_id") %>% # join trail ids
    select(id, everything(), -about) # remove name column
  
  # add each download as a list element
  csvdatalist[[start]] <- healthkitdaily
}

# bind together lists into one data frame
csvdatalist <- csvdatalist %>% bind_rows() 

newcsv <- bind_rows(originaldata, csvdatalist)

write.csv(csvdatalist, "data/processed/training_load/healthkit_daily_summary.csv")


#######
## Apple HealthKit
#####


csvdatalist <- list()

# Loop to download garmin daily summary 
for  (i in start_dates) {
  
  # format dates properly
  i <- as.Date(i, origin = "1970-01-01")
  end_date <- ceiling_date(ymd(i), "month") - days(1) # get end date of month
  start <- as.character(format(as.Date(i), "%d/%m/%Y"))
  end <- as.character(format(as.Date(end_date), "%d/%m/%Y"))
  
  # pull data through API
  healthkit <- pull_smartabase(
    form = "HealthKit Activity",
    start_date = start,
    end_date = end) %>%
    inner_join(., fusion_id, by = "user_id") %>% # join trail ids
    select(id, everything(), -c(about, UUID)) # remove name column and duplicated UUID column
  
  # add each download as a list element
  csvdatalist[[start]] <- healthkit %>% select(-CSV)
  
  # write granular HR data to csv file per participant and event
  healthkit %>%
    filter(!is.na(CSV)) %>%
    mutate(path = paste0(id, "_", event_id,".csv")) %>%
    select(path, CSV) %>%
    pwalk(~write(x = .y, file = paste0("data/processed/training_load/event_data/", .x)))
  
}

# bind together lists into one data frame
csvdatalist <- csvdatalist %>% bind_rows() 

newcsv <- bind_rows(originaldata, csvdatalist)

write_csv(csvdatalist, "data/processed/training_load/healthkit_activity.csv")


