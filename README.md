# Trail Data Cleaning README

This script is to extract data from Fusion/Smartabase into a database for the Trail Project

Requires the following inputs:
  - data exported from fusion. Using 'library(neon)' to access API. 
  - note no data stored in this repo. API Log in details also required with 'save_credentials()' from 'neon' 
  - 'data/raw/varnames.xlsx' - this is a file of what to change variable names to from fusion export (which is messy)

Src folder contains all scripts.

`traildata` folder contains an rshiny app for extracting data for use in data analysis. Currently published at [https://mgirdwood.shinyapps.io/traildata/](https://mgirdwood.shinyapps.io/traildata/)
