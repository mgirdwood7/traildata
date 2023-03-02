# this will just write the summary without the full Sample there
garmin %>%
  select(-Samples) %>%
  write_csv("test.csv")


headgarmin %>%
  mutate(new = map(Samples, ~read.table(text = ., sep = ",", header = TRUE))) %>%
  select(-Samples) %>%
  pwalk(~write_csv(x = .y, path = paste0("test/", .x, ".csv")))

         
test3 <- garmin %>%
  filter(!is.na(Samples)) %>% # only take the data with the Samples
  mutate(id = paste0(UUID, "_", event_id)) %>% # combine for a temp new id (person id + activity id)
  mutate(new = map(Samples, ~read.table(text = ., sep = ",", header = TRUE))) %>% # map each comma separated sample to read as a table and nest dataframe
  select(id, new) %>% # select the 2 objects for pwalk
  pwalk(~write_csv(x = .y, path = paste0("test/",.x, ".csv"))) # write each to csv, naming by id
         


test4 <- garmin %>%
  head() %>%
  filter(!is.na(Samples)) %>% # only take the data with the Samples
  mutate(id = paste0(UUID, "_", event_id)) %>% # combine for a temp new id (person id + activity id)
  mutate(new = map(Samples, ~read.table(text = ., sep = ",", header = TRUE))) %>% # map each comma separated sample to read as a table and nest dataframe
  mutate(path = paste0(UUID, "/", event_id, ".csv")) %>%
  select(path, new) %>%
  pwalk(~write_csv(x = .y, path = paste0("test/",.x)))

# Get data
# Get Ids of people with data
# Create folders for people (if not existing, don't overwrite)
# get data and nest as above
# write to folder

test4 <- garmin %>%
  filter(!is.na(Samples)) %>% # only take the data with the Samples
  mutate(id = paste0(UUID, "_", event_id)) %>% # combine for a temp new id (person id + activity id)
  mutate(new = map(Samples, ~read.table(text = ., sep = ",", header = TRUE))) %>% # map each comma separated sample to read as a table and nest dataframe
  mutate(path = paste0(UUID, "/", event_id, ".csv")) %>%
  select(path, new) %>%
  toJSON(.) %>%
  write(., "myJSON.json")


# keep 'Samples' and daily summaries separate.
# download Garmin data month by month and exclude the sample data. 

# Get dates to download
start_date <- as.Date("2020-07-01")
end_date <- Sys.Date()

# Create a sequence of months between the start and end dates
start_dates <- seq(from = start_date, to = end_date, by = "month")
end_dates <- ceiling_date(ymd(start_dates), "month") - days(1) # round up to nearest month, -1 day to get the end of the month


## Download daily Garmin Data Summary
## Function to loop through dates and downlaod data
for  (i in start_dates) {
  
  # format dates properly
  i <- as.Date(i, origin = "1970-01-01")
  end_date <- ceiling_date(ymd(i), "month") - days(1) # get end date of month
  filename = paste0("Garmin Daily_", i) # create file name based on start date
  
  start <- as.character(format(as.Date(i), "%d/%m/%Y"))
  end <- as.character(format(as.Date(end_date), "%d/%m/%Y"))
  
# pull data through API
garmindaily <- pull_smartabase(
  form = "Garmin Daily Summary",
  start_date = start,
  end_date = end)

# remove nested HR sample columnn and write to csv
garmindaily %>%
  select(-`Heart Rate Samples`) %>% 
  write_csv(paste0(filename, ".csv"))

# keep HR sample data, write to JSON
garmindaily %>%
  mutate(hr_samples = map(`Heart Rate Samples`, ~read.table(text = ., sep = ",", header = TRUE))) %>% # map each comma separated sample to read as a table and nest dataframe
  select(-'Heart Rate Samples') %>%
  toJSON(.) %>%
  write(., paste0(filename, ".json"))
}




## Read in Previous Data
...

# retrieve last date 
last_date <- data$start_date %>% tail(1) %>% as.Date
last_date <- as.Date("2020-01-01")

# Create sequence of months needed to download
# minus one month to go to last complete month
# do we need to go back further incase people haven't synced their watches and new data is added???
start_dates <- seq(from = last_date, to = Sys.Date() - months(1), by = "month")


# Create empty lists to populate with loop
csvdatalist <- list()
jsondatalist <- list()

# Loop to download garmin daily summary 
for  (i in start_dates) {
  
  # format dates properly
  i <- as.Date(i, origin = "1970-01-01")
  end_date <- ceiling_date(ymd(i), "month") - days(1) # get end date of month
  filename = paste0("Garmin Daily_", i) # create file name based on start date
  
  start <- as.character(format(as.Date(i), "%d/%m/%Y"))
  end <- as.character(format(as.Date(end_date), "%d/%m/%Y"))
  
  # pull data through API
  garmindaily <- pull_smartabase(
    form = "Garmin Daily Summary",
    start_date = start,
    end_date = end)
  
  # remove nested HR sample columnn and write to csv
  csvdata <- garmindaily %>%
    select(-`Heart Rate Samples`)
  csvdatalist[[start]] <- csvdata # add each download as a list element
  
  # keep HR sample data, write to JSON
  jsondata <- garmindaily %>%
    mutate(hr_samples = map(`Heart Rate Samples`, ~read.table(text = ., sep = ",", header = TRUE))) %>% # map each comma separated sample to read as a table and nest dataframe
    select(-'Heart Rate Samples')
  jsondatalist[[start]] <- jsondata
}

# bind together lists into one data frame
csvdatalist <- csvdatalist %>% bind_rows() 
jsondatalist <- jsondatalist %>% bind_rows()

newcsv <- bind_rows(originaldata, csvdatalist)
newjson <- bind_rows(originaldata, jsondatalist) %>% toJSON(.)

write.csv(csvdatalist, "data/processed/training_load/Garmin Daily Summary.csv")
write(jsondatalist, "data/processed/training_load/Garmin Daily Summary.json")



###########
########
## Garmin ###
########


# Loop to download garmin daily summary 
for  (i in start_dates) {
  
  # format dates properly
  i <- as.Date(i, origin = "1970-01-01")
  end_date <- ceiling_date(ymd(i), "month") - days(1) # get end date of month
  filename = paste0("Garmin Daily_", i) # create file name based on start date
  
  start <- as.character(format(as.Date(i), "%d/%m/%Y"))
  end <- as.character(format(as.Date(end_date), "%d/%m/%Y"))
  
  # pull data through API
  garminactivity <- pull_smartabase(
    form = "Garmin Activity Summary",
    start_date = start,
    end_date = end)
  
  # Writes each individual piece of data to a row 
  garminactivity %>%
    filter(!is.na('Summary Id')) %>% # only take the data with the Samples
    mutate(id = paste0(UUID, "_", event_id)) %>% # combine for a temp new id (person id + activity id)
    mutate(new = map(Samples, ~read.table(text = ., sep = ",", header = TRUE))) %>% # map each comma separated sample to read as a table and nest dataframe
    mutate(path = paste0(UUID, "/", event_id, ".csv")) %>%
    select(path, new) %>%
    pwalk(~write_csv(x = .y, path = paste0("data/processed/event_data/",.x)))
  
  
  # remove nested granular data sample column and write to csv
  csvdata <- garmindaily %>%
    select(-Sample)
  csvdatalist[[start]] <- csvdata # add each download as a list element
  
}

# bind together lists into one data frame
csvdatalist <- csvdatalist %>% bind_rows() 

newcsv <- bind_rows(originaldata, csvdatalist)

write.csv(csvdatalist, "data/processed/training_load/Garmin Activity.csv")




###########
########
## Apple Health Kit Daily Summary ###
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
    end_date = end)
  
  # add each download as a list element
  csvdatalist[[start]] <- healthkitdaily
}

# bind together lists into one data frame
csvdatalist <- csvdatalist %>% bind_rows() 

newcsv <- bind_rows(originaldata, csvdatalist)

write.csv(csvdatalist, "data/processed/training_load/HealthKit Daily Summary.csv")



## Apple 


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
    pwalk(~write(x = .y, file = paste0("data/processed/training_load/event_data/", path)))
          
}

# bind together lists into one data frame
csvdatalist <- csvdatalist %>% bind_rows() 

newcsv <- bind_rows(originaldata, csvdatalist)

write_csv(csvdatalist, "data/processed/training_load/healthkit_activity.csv")




# For now downlaod the nested dataframe as a JSON object
# Future, could consider writing each individual activity/event to a csv file per participant
# Then can organise each file into participant folders
# This would allow for some easier querying perhaps?


# run a loop through the months of the year since trail has started to:
## download data
## format Sample columns etc
## write to JSON
## write summary csv file without samples



healthkit <- pull_smartabase(
  form = "HealthKit Activity",
  start_date = "01/09/2020",
  end_date = "08/09/2020")

apple <- healthkit[7,] %>% select(CSV) %>% mutate(new = map(CSV, ~read.csv(text = ., sep = ",", header = TRUE)))


apple <- healthkit[4:5,] %>% select(event_id, CSV) 

apple %>%
  pwalk(~write(x = .y, file = paste0(.x, ".csv")))


pwalk(~write_csv(x = .y, path = paste0("data/processed/event_data/",.x)))

apple %>%
healthkit[2,] %>% select(CSV) %>% purrr::map( ~write("test2.csv"))


mutate(new = map(Samples, ~read.table(text = ., sep = ",", header = TRUE)))
