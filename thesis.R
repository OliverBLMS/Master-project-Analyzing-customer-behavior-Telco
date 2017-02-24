########################################################
# get / set workdirectory
########################################################
getwd()
setwd(dir = "/Users/oliver.belmans/Data/R_workdirectory/Thesis")
getwd()

########################################################
# load packages
########################################################
pkg <- c("tidyverse", "sna")
lapply(pkg, require, character.only = TRUE)

########################################################
# read sample dataset
########################################################
sample_cdr <- read.table("sample_cdr.txt", 
                         header = TRUE, 
                         sep = ",", 
                         stringsAsFactors = FALSE)


########################################################
# Basic Checks
########################################################
# Check if caller appear as callee
table(sample_cdr$A_NUMBER %in% sample_cdr$B_NUMBER)

########################################################
# Variable manipulations
########################################################
## CALL_START_DT to date format
## Create extra date/time variables like week-, daynumber, houres
sample_cdr$CALL_START_DT_TM <- strptime(paste(sample_cdr$CALL_START_DT, sample_cdr$CALL_START_TM),"%d%b%Y %H:%M:%S")
sample_cdr$WEEK_NR <- format(sample_cdr$CALL_START_DT_TM, "%U")
sample_cdr$DAY_NR <- format(sample_cdr$CALL_START_DT_TM, "%w")
sample_cdr$DAY <- format(sample_cdr$CALL_START_DT_TM, "%a")
sample_cdr$HOUR <- as.integer(format(sample_cdr$CALL_START_DT_TM, "%H"))
sample_cdr$PEAK_HOUR <- ifelse(sample_cdr$HOUR >= 8 & sample_cdr$HOUR <= 18, "PEAK", "NON-PEAK")

# Filter outliers or non relevant items
## based on duration
# todo




