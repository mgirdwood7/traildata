# Clean File

# Variable renaming

# KOOS

koos <- koos %>%
  rename_at(vars(-c(1:4)),
            ~ str_replace_all(., setNames(koosnames$codename, koosnames$question))) %>%
  rename_at(vars(ends_with("?")), 
            ~str_replace(., "\\?", "")) %>%
  select(-HIDDEN, -Index)


renamefunction('koos')

## may need to redo totals for KOOS
## also need to recode koos factors

# SPEX

# ? write as a function

# ASSQ

# Tampa

## Code factor
## Need to recalculate total? and check inverse, also calculate tsk_11 total

# Pass

# Visa-a

# Trail baseline
## Shoes question double up? 
## and need to remove '.' column


# phone Screening
## need to remove days since surgery column as auto calculated
## remove postcode due to double up in baseline questionnaire
## remove sex, height, weight, group
## remove finalmessage
## remove contains('llsurgery')

# lab testing and biodex
