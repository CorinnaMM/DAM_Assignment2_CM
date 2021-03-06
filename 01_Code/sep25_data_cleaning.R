###########################################################

###            DATA ALGOITHMS AND MEANING
###           ASSESSMENT 2: Classification 
###              LOAN DEFAULT CHALLENGE

###              Corinna Maher Mittmann
###                    SEP 2017

###   DOCUMENT NAME: DAM_data_understanding_corinna
###########################################################

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

#### ==== Pre-clean exploration

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

## === START CLEAN

# remove columns with 1 level

mylist_levels <- default %>%
  summarise_each(funs(n_distinct)) %>%
  tidyr::gather("LoanStatNew", "level") %>% filter(level != 1) %>% select(LoanStatNew)
cols_to_keep <- as_vector(mylist_levels, .type = "character")

typeof(cols_to_keep)
typeof(names(default))

default2 = default[,cols_to_keep]

### ======== may need to review this decision to remove as 0 and NA values only - ? does distinction btw 0 and NA have value?
#collections # n.a
unique(default2$collections_12_mths_ex_med)
table(default2$collections_12_mths_ex_med)
default2 <- select(default2, -collections_12_mths_ex_med)
# remove tax liens
unique(default2$tax_liens)
table(default2$tax_liens)
default2 <- select(default2, -tax_liens)

# rewrite working data
# default = default[,cols_to_keep]
# write.csv(default, paste0(working_file_path,"clean_data_v1.csv"),row.names = F)
# 
# write.csv(default2, paste0(working_file_path,"clean_data_31var.csv"),row.names = F)
# data<- read.csv(paste0(working_file_path,"clean_data_31var.csv"))


# make interest rate numeric
unique(data$int_rate)
data$int_rate <- gsub("X", "", data$int_rate)
# check that "." is the last character for every level
table((substr(data$int_rate, nchar(data$int_rate),nchar(data$int_rate))=="."))[[1]]==nrow(data)
n_distinct((substr(data$int_rate, 1,nchar(data$int_rate)-1))) == n_distinct(data$int_rate)


data$int_rate <- substr(data$int_rate, 1,nchar(data$int_rate)-1)
# write.csv(data, paste0(working_file_path,"clean_data_v2.csv"),row.names = F)
data<- read.csv(paste0(working_file_path,"clean_data_v2.csv"))
str(data)

# Make emp_length consistent
# change missing format
data$emp_length <- gsub("\\.","",data$emp_length)
head(data$emp_length)
# deal with na
table(data$emp_length == "na")
data$emp_length <- ifelse(data$emp_length == "na","unknown",data$emp_length) #unknown for unknown values we can change this to "unknown"
table(data$emp_length == "unknown")
# keep only the value and remove "years" year, ..years etc units
data$emp_length <- gsub("year","",data$emp_length)
data$emp_length <- gsub("s","",data$emp_length)
unique(data$emp_length)
qplot(data$emp_length, fill = "identity")
# write.csv(data, paste0(working_file_path,"clean_data_v3.csv"),row.names = F)
# data<- read.csv(paste0(working_file_path,"clean_data_v3.csv"))

# Make term consistent
# change missing format
data$term <- gsub("\\.","",data$term)
head(data$term)
# deal with na
table(data$term == "na")
# no na values established
# data$term <- ifelse(data$term == "na","unknown",data$term)
# table(data$term == "unknown")
# keep only the value and remove "years" year, ..years etc units
data$term <- gsub("month","",data$term)
data$term <- gsub("s","",data$term)
head(data$term)
unique(data$term)
qplot(data$term, fill = "identity")
# write.csv(data, paste0(working_file_path,"clean_data_v4.csv"),row.names = F)
# data<- read.csv(paste0(working_file_path,"clean_data_v4.csv"))

# Make verification_status consistent
# change missing format
unique(data$verification_status)
data$verification_status <- gsub("\\.","_",data$verification_status)
qplot(data$verification_status, fill = "identity")
# pub rec bankruptcies # NA
unique(data$pub_rec_bankruptcies)
# deal with na
table(is.na(data$pub_rec_bankruptcies))
data$pub_rec_bankruptcies <- ifelse(is.na(data$pub_rec_bankruptcies),"unknown",data$pub_rec_bankruptcies)
table(data$pub_rec_bankruptcies == "unknown")
as.factor(data$pub_rec_bankruptcies)
qplot(data$pub_rec_bankruptcies, fill = "identity")
table(data$pub_rec_bankruptcies)

## ====== Refomat the date column

# earliest cred line convert to floor date
data$earliest_cr_line = gsub("\\.","-",data$earliest_cr_line)
data <- mutate(data, earliest_cr_line = as.Date(paste0("01-",earliest_cr_line), format = "%d-%b-%Y"))
# test <- dplyr::sample_n(as.tibble(data$earliest_cr_line), 10)
unique(data$earliest_cr_line)
# qplot(data$earliest_cr_line, data$total_rec_int, colour = data$inq_last_6mths)
qplot(data$earliest_cr_line, fill = "identity") ## + facet_wrap(data$loan_status)
## we may or may not want to convert the date into continuous time series
## it may be advantageous to keep the time format as is

###########################################

# Remove ID to rownames before moedelling
# head(data$member_id)
# row.names(data) <- data$member_id
# data <- select(data, -member_id)
# head(data)
# tail(data)
# tail(default)
## note the above transofrmation is only available whilst member id is unique

# write.csv(data, paste0(working_file_path,"clean_data_v5.csv"),row.names = T)
write_csv(data, paste0(working_file_path,"clean_data_30var.csv"))
data <- read.csv(paste0(working_file_path,"clean_data_30var.csv"), row.names = T)

### - Remove columns with collinearity

data <- select(data, -funded_amnt_inv, -installment, -sub_grade, -total_pymnt_inv, -total_rec_prncp) 
write_csv(data, paste0(working_file_path,"clean_justified_data_25var.csv"))
### === END CLEAN

str(data)
table(data$loan_status)
## make target binary
data <- mutate(data, loan_status = if_else(loan_status == "Charged.Off",1,0))
table(data$loan_status)

write_csv(data, paste0(working_file_path,"modellingdata1.csv"))
