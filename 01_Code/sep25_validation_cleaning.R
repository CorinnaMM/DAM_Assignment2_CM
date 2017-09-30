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
default <- read.csv(paste0(Original_Data_Location,"validation.csv"), header = T)
new_data_dict <- read.csv(paste0(Original_Data_Location,"new_data_dictionary.csv"), header = T)
old_data <- read.csv(paste0(working_file_path,"clean_justified_data_25var.csv"))

# remove columns not in model data
targetcolnum <- grep("loan_status",names(old_data))
# is_there_a_target <- grep("loan_status",names(default)) #no
cols_to_keep <- names(old_data[,-targetcolnum]) # 10 is target (loan_status)
typeof(cols_to_keep)
names(default)
default2 = select(default, paste0(cols_to_keep))
names(default2)

check_for_missing(default2)

# for (i in 1:(length(names(default)))){
#   print(i)
#   s <- qplot(x = default[,i],y=default$loan_amnt, geom = "point", col = default$loan_status) 
#   plot(s)
# }


# descriptive_info(default, "loan_status")


# str(default)

## === START CLEAN


### ======== may need to review this decision to remove as 0 and NA values only - ? does distinction btw 0 and NA have value?
#collections # n.a
unique(default2$collections_12_mths_ex_med)
# removed
# table(default2$collections_12_mths_ex_med)
# default2 <- select(default2, -collections_12_mths_ex_med)
# remove tax liens
unique(default2$tax_liens)
#removed
# table(default2$tax_liens)
# default2 <- select(default2, -tax_liens)

# rewrite working new_data
# default = default[,cols_to_keep]
# write.csv(default, paste0(working_file_path,"clean_new_data_v1.csv"),row.names = F)
# 
# write.csv(default2, paste0(working_file_path,"clean_new_data_31var.csv"),row.names = F)
# new_data<- read.csv(paste0(working_file_path,"clean_new_data_31var.csv"))

new_data <- default2

# make interest rate numeric
unique(new_data$int_rate)
new_data$int_rate <- gsub("X", "", new_data$int_rate)
# check that "." is the last character for every level
table((substr(new_data$int_rate, nchar(new_data$int_rate),nchar(new_data$int_rate))=="."))[[1]]==nrow(new_data)
n_distinct((substr(new_data$int_rate, 1,nchar(new_data$int_rate)-1))) == n_distinct(new_data$int_rate)


new_data$int_rate <- substr(new_data$int_rate, 1,nchar(new_data$int_rate)-1)
# write.csv(new_data, paste0(working_file_path,"clean_new_data_v2.csv"),row.names = F)
# new_data<- read.csv(paste0(working_file_path,"clean_new_data_v2.csv"))
str(new_data)

# Make emp_length consistent
# change missing format
new_data$emp_length <- gsub("\\.","",new_data$emp_length)
head(new_data$emp_length)
# deal with na
table(new_data$emp_length == "na")
new_data$emp_length <- ifelse(new_data$emp_length == "na","unknown",new_data$emp_length) #unknown for unknown values we can change this to "unknown"
table(new_data$emp_length == "unknown")
# keep only the value and remove "years" year, ..years etc units
new_data$emp_length <- gsub("year","",new_data$emp_length)
new_data$emp_length <- gsub("s","",new_data$emp_length)
unique(new_data$emp_length)
qplot(new_data$emp_length, fill = "identity")
# write.csv(new_data, paste0(working_file_path,"clean_new_data_v3.csv"),row.names = F)
# new_data<- read.csv(paste0(working_file_path,"clean_new_data_v3.csv"))

# Make term consistent
# change missing format
new_data$term <- gsub("\\.","",new_data$term)
head(new_data$term)
# deal with na
table(new_data$term == "na")
# no na values established
# new_data$term <- ifelse(new_data$term == "na","unknown",new_data$term)
# table(new_data$term == "unknown")
# keep only the value and remove "years" year, ..years etc units
new_data$term <- gsub("month","",new_data$term)
new_data$term <- gsub("s","",new_data$term)
head(new_data$term)
unique(new_data$term)
qplot(new_data$term, fill = "identity")
# write.csv(new_data, paste0(working_file_path,"clean_new_data_v4.csv"),row.names = F)
# new_data<- read.csv(paste0(working_file_path,"clean_new_data_v4.csv"))

# Make verification_status consistent
# change missing format
unique(new_data$verification_status)
new_data$verification_status <- gsub("\\.","_",new_data$verification_status)
qplot(new_data$verification_status, fill = "identity")
# pub rec bankruptcies # NA
unique(new_data$pub_rec_bankruptcies)
# deal with na
table(is.na(new_data$pub_rec_bankruptcies))
new_data$pub_rec_bankruptcies <- ifelse(is.na(new_data$pub_rec_bankruptcies),"unknown",new_data$pub_rec_bankruptcies)
table(new_data$pub_rec_bankruptcies == "unknown")
as.factor(new_data$pub_rec_bankruptcies)
qplot(new_data$pub_rec_bankruptcies, fill = "identity")
table(new_data$pub_rec_bankruptcies)

## ====== Refomat the date column

# earliest cred line convert to floor date
new_data$earliest_cr_line = gsub("\\.","-",new_data$earliest_cr_line)
new_data <- mutate(new_data, earliest_cr_line = as.Date(paste0("01-",earliest_cr_line), format = "%d-%b-%Y"))
# test <- dplyr::sample_n(as.tibble(new_data$earliest_cr_line), 10)
unique(new_data$earliest_cr_line)
# qplot(new_data$earliest_cr_line, new_data$total_rec_int, colour = new_data$inq_last_6mths)
qplot(new_data$earliest_cr_line, fill = "identity") ## + facet_wrap(new_data$loan_status)
## we may or may not want to convert the date into continuous time series
## it may be advantageous to keep the time format as is

###########################################

# Remove ID to rownames before moedelling
# head(new_data$member_id)
# row.names(new_data) <- new_data$member_id
# new_data <- select(new_data, -member_id)
# head(new_data)
# tail(new_data)
# tail(default)
## note the above transofrmation is only available whilst member id is unique

# write.csv(new_data, paste0(working_file_path,"clean_new_data_v5.csv"),row.names = T)
write_csv(new_data, paste0(working_file_path,"cleaned_validation.csv"))
new_data <- read.csv(paste0(working_file_path,"cleaned_validation.csv"), row.names = T)

# ### - Remove columns with collinearity
# 
# new_data <- select(new_data, -funded_amnt_inv, -installment, -sub_grade, -total_pymnt_inv, -total_rec_prncp) 
# write_csv(new_data, paste0(working_file_path,"clean_justified_new_data_25var.csv"))
# ### === END CLEAN
# 
# str(new_data)
# table(new_data$loan_status)
# ## make target binary
# new_data <- mutate(new_data, loan_status = if_else(loan_status == "Charged.Off",1,0))
# table(new_data$loan_status)
# 
# write_csv(new_data, paste0(working_file_path,"modellingnew_data1.csv"))

## new extra columns with missing values 
## total_acc
## pub_rec
## open_acc
## earliest_cr_line
## delinq_2yrs
## annual_inc

check_for_missing(new_data)
unique(new_data$emp_length)
## total_acc
table(is.na(new_data$total_acc))
class(new_data$total_acc)
## pub_rec
unique(new_data$pub_rec_bankruptcies)
unique(new_data$pub_rec)
table(new_data$pub_rec)
## open_acc
## earliest_cr_line
## delinq_2yrs
## annual_inc
