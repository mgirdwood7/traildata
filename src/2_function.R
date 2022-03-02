# Function to rename variables based on list of same name - e.g. rename koos with koosnames list

renamefunction <- function(data) {
  title <- paste0(data, "names") # create string with variable name + "names" 
  name <- get(title) # get relevant name list
  data <- get(data) # get relevant data

data %>% # use relevant data
  rename_at(vars(-c(1:4)), 
            ~str_replace_all(., setNames(name$codename, name$question))) %>% # replace all var names
  rename_at(vars(ends_with("?")), # remove any question marks not matched
            ~str_replace(., "\\?", "")) %>%
  select(-HIDDEN, -Index) # remove unneeded variables
}  
