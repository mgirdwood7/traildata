# Laod

library(tidyverse)
library(readxl)
library(openxlsx)

# Data
names <- list.files(path = "data/raw/", pattern = "^\\w+.+.xlsx", recursive = TRUE) # find .xlsx files in 

files <- names %>%
  paste0("data/raw/",.) %>% # paste directory title at start of name
  map(~readxl::read_xlsx(., sheet = 1)) # read in each file to a list

names(files) <- names %>% str_extract("(?<=/)\\w+(?=\\.)") # name each list by name of the file.
list2env(files, envir = .GlobalEnv) # add each element of the list to the environment as its own df.

# Variable Names 
path <- "data/raw/varnames.xlsx" # set path for read in later

varnames <- path %>%
  excel_sheets() %>% # read in all sheets
  set_names() %>% # set names based on name of sheet
  map(read_xlsx, path = path) # read in each sheet to a list

names(varnames) <- paste0(names(varnames), "names") # paste 'names' to end of each variable name 
list2env(varnames, envir = .GlobalEnv) # add each element of the list to the environment as its own df.