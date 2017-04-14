# get / set workdirectory to read in the datafile
################################################################################################################
setwd(dir = "c:/Data_thesis/")
getwd()

# load packages/libraries used in the analysis
################################################################################################################
pkg <- c("tidyverse", "igraph", "data.table", "dtplyr", "broom", "purrr", "ggplot2")
sapply(pkg, require, character.only = TRUE )


# read sample dataset (for now make sample set)
################################################################################################################
# read test CDR dataset
get_all_data <- function() {
  load_data <- function(path_dataset) {
    fread(input = path_dataset, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  }
  # Load separate files
  cdr5 <- load_data("PREPAID_CDR_ANONYM_201005.txt")
  cdr6 <- load_data("PREPAID_CDR_ANONYM_201006.txt")
  cdr7 <- load_data("PREPAID_CDR_ANONYM_201007.txt")
  cdr8 <- load_data("PREPAID_CDR_ANONYM_201008.txt")
  # cdr9 <- load_data("PREPAID_CDR_ANONYM_201009.txt")
  # Bind all rows together
  cdr1 <- bind_rows(cdr5, cdr6, cdr7, cdr8)
  # adjust first columns to char
  cdr1$A_NUMBER <- as.character(cdr1$A_NUMBER)
  cdr1$B_NUMBER <- as.character(cdr1$B_NUMBER)
  # Return final large data.table
  return(cdr1)
  
}

# execute 
cdr1 <- get_all_data()


# drop calls under 5 seconds -> this implies wrong calls or perhaps voicemail calls
################################################################################################################
cdr1 <-
  filter(cdr1, CALL_ACTUAL_DURATION > 5)
# exclude all customers that have either incoming-outgoings calls above 1400 calls a month.
cdr1 <- 
  filter(cdr1, 
         !B_NUMBER %in% c(filter(incoming_calls, nr > 2000)$B_NUMBER, filter(outgoing_call, nr > 2000)$A_NUMBER), 
         !A_NUMBER %in% c(filter(incoming_calls, nr > 2000)$B_NUMBER, filter(outgoing_call, nr > 2000)$A_NUMBER)
  )





