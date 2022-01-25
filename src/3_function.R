renamefunction <- function(data){
  title <- paste0(data, "names") # create string with varname + "names"
  name <- get(name) # get relevant name list
  data <- get(data) # get relevant data

data %>%
  rename_at(vars(-c(1:4)),
            ~str_replace_all(., setNames(test$codename, test$question))) %>% # replace all var names
  rename_at(vars(ends_with("?")), # remove any question marks not matched
            ~str_replace(., "\\?", "")) %>%
  select(-HIDDEN, -Index) # remove unneeded variables
}