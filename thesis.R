# get / set workdirectory to read in the datafile
################################################################################################################
setwd(dir = "/Users/oliver.belmans/Data/R_workdirectory/Thesis")
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
  cdr9 <- load_data("PREPAID_CDR_ANONYM_201009.txt")
  
  cdr1 <- bind_rows(cdr5, cdr6, cdr7, cdr8, cdr9)
  return(cdr1)
  
}



# 
dim(cdr1)
cdr1$A_NUMBER <- as.character(cdr1$A_NUMBER)
cdr1$B_NUMBER <- as.character(cdr1$B_NUMBER)
