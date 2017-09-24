###########################################################

###            DATA ALGOITHMS AND MEANING
###           ASSESSMENT 2: Classification 
###              LOAN DEFAULT CHALLENGE

###              Corinna Maher Mittmann
###                    SEP 2017

###   DOCUMENT NAME: DAM_data_understanding_corinna
###########################################################
## Work-space management
Original_Data_Location <- "/Users/corinnamm/Documents/GitHub/DAM_Assignment2_CM/03_DAM_Spring2017_Assignment_2/"

working_file_path <- "/Users/corinnamm/Documents/GitHub/DAM_Assignment2_CM/02_Working_data_folder/"

code_file_path <- "/Users/corinnamm/Documents/GitHub/DAM_Assignment2_CM/01_Code/"

wd_path <- "/Users/corinnamm/Documents/GitHub/DAM_Assignment2_CM/"

getwd()
### Packages
library(randomForest)
library(glmnet)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(stats)
require(caret)
require(readr)
require(e1071)

library(gbm)
library(caret)
library(ISLR)
library(corrplot)
library(nnet)

source(paste0(code_file_path,"99_general_functions_DAM_Corinna.R"))
# h2o package for neural nets

### 
default <- read.csv(paste0(Original_Data_Location,"training.csv"), header = T)
data_dict <- read.csv(paste0(Original_Data_Location,"data_dictionary.csv"), header = T)
data_dict

check_for_missing(default)

# for (i in 1:(length(names(default)))){
#   print(i)
#   s <- qplot(x = default[,i],y=default$loan_amnt, geom = "point", col = default$loan_status) 
#   plot(s)
# }


descriptive_info(default, "loan_status")


str(default)

# remove columns with 1 level

mylist_levels <- default %>%
  summarise_each(funs(n_distinct)) %>%
  tidyr::gather("LoanStatNew", "level") %>% filter(level != 1) %>% select(LoanStatNew)
cols_to_keep <- as_vector(mylist_levels, .type = "character")

typeof(cols_to_keep)
typeof(names(default))

default2 = default[,cols_to_keep]
# rewrite working data
write.csv(default2, paste0(working_file_path,"clean_data_v1.csv"),row.names = F)

data<- read.csv(paste0(working_file_path,"clean_data_v1.csv"))
# Refomat the date column

# make interest rate numeric
unique(data$int_rate)
data$int_rate <- gsub("X", "", data$int_rate)
# check that "." is the last character for every level
table((substr(data$int_rate, nchar(data$int_rate),nchar(data$int_rate))=="."))[[1]]==nrow(data)
n_distinct((substr(data$int_rate, 1,nchar(data$int_rate)-1))) == n_distinct(data$int_rate)


data$int_rate <- substr(data$int_rate, 1,nchar(data$int_rate)-1)
write.csv(data, paste0(working_file_path,"clean_data_v2.csv"),row.names = F)
data<- read.csv(paste0(working_file_path,"clean_data_v2.csv"))
str(data)

# Make emp_length consistent
# change missing format
data$emp_length <- gsub("\\.","",data$emp_length)
head(data$emp_length)
# deal with na
table(data$emp_length == "na")
data$emp_length <- ifelse(data$emp_length == "na","XUK",data$emp_length)
table(data$emp_length == "XUK")
# keep only the value and remove "years" year, ..years etc units
data$emp_length <- gsub("year","",data$emp_length)
data$emp_length <- gsub("s","",data$emp_length)

write.csv(data, paste0(working_file_path,"clean_data_v3.csv"),row.names = F)
data<- read.csv(paste0(working_file_path,"clean_data_v3.csv"))



# Remove ID to rownames before moedelling
data$member_id