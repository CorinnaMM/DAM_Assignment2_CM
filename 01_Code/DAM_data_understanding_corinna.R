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

# #### ==== Pre-clean exploration
# 
# ### 
# default <- read.csv(paste0(Original_Data_Location,"training.csv"), header = T)
# data_dict <- read.csv(paste0(Original_Data_Location,"data_dictionary.csv"), header = T)
# data_dict
# 
# check_for_missing(default)
# 
# # for (i in 1:(length(names(default)))){
# #   print(i)
# #   s <- qplot(x = default[,i],y=default$loan_amnt, geom = "point", col = default$loan_status) 
# #   plot(s)
# # }
# 
# 
# descriptive_info(default, "loan_status")
# 
# 
# str(default)
# 
# ## === START CLEAN
# 
# # remove columns with 1 level
# 
# mylist_levels <- default %>%
#   summarise_each(funs(n_distinct)) %>%
#   tidyr::gather("LoanStatNew", "level") %>% filter(level != 1) %>% select(LoanStatNew)
# cols_to_keep <- as_vector(mylist_levels, .type = "character")
# 
# typeof(cols_to_keep)
# typeof(names(default))
# 
# default2 = default[,cols_to_keep]
# 
# ### ======== may need to review this decision to remove as 0 and NA values only - ? does distinction btw 0 and NA have value?
# #collections # n.a
# unique(default2$collections_12_mths_ex_med)
# table(default2$collections_12_mths_ex_med)
# default2 <- select(default2, -collections_12_mths_ex_med)
# # remove tax liens
# unique(default2$tax_liens)
# table(default2$tax_liens)
# default2 <- select(default2, -tax_liens)
# 
# # rewrite working data
# # default = default[,cols_to_keep]
# # write.csv(default, paste0(working_file_path,"clean_data_v1.csv"),row.names = F)
# # 
# # write.csv(default2, paste0(working_file_path,"clean_data_31var.csv"),row.names = F)
# # data<- read.csv(paste0(working_file_path,"clean_data_31var.csv"))
# 
# 
# # make interest rate numeric
# unique(data$int_rate)
# data$int_rate <- gsub("X", "", data$int_rate)
# # check that "." is the last character for every level
# table((substr(data$int_rate, nchar(data$int_rate),nchar(data$int_rate))=="."))[[1]]==nrow(data)
# n_distinct((substr(data$int_rate, 1,nchar(data$int_rate)-1))) == n_distinct(data$int_rate)
# 
# 
# data$int_rate <- substr(data$int_rate, 1,nchar(data$int_rate)-1)
# # write.csv(data, paste0(working_file_path,"clean_data_v2.csv"),row.names = F)
# data<- read.csv(paste0(working_file_path,"clean_data_v2.csv"))
# str(data)
# 
# # Make emp_length consistent
# # change missing format
# data$emp_length <- gsub("\\.","",data$emp_length)
# head(data$emp_length)
# # deal with na
# table(data$emp_length == "na")
# data$emp_length <- ifelse(data$emp_length == "na","XUK",data$emp_length) #XUK for unknown values we can change this to "unknown"
# table(data$emp_length == "XUK")
# # keep only the value and remove "years" year, ..years etc units
# data$emp_length <- gsub("year","",data$emp_length)
# data$emp_length <- gsub("s","",data$emp_length)
# unique(data$emp_length)
# qplot(data$emp_length, fill = "identity")
# # write.csv(data, paste0(working_file_path,"clean_data_v3.csv"),row.names = F)
# # data<- read.csv(paste0(working_file_path,"clean_data_v3.csv"))
# 
# # Make term consistent
# # change missing format
# data$term <- gsub("\\.","",data$term)
# head(data$term)
# # deal with na
# table(data$term == "na")
# # no na values established
# # data$term <- ifelse(data$term == "na","XUK",data$term)
# # table(data$term == "XUK")
# # keep only the value and remove "years" year, ..years etc units
# data$term <- gsub("month","",data$term)
# data$term <- gsub("s","",data$term)
# head(data$term)
# unique(data$term)
# qplot(data$term, fill = "identity")
# # write.csv(data, paste0(working_file_path,"clean_data_v4.csv"),row.names = F)
# # data<- read.csv(paste0(working_file_path,"clean_data_v4.csv"))
# 
# # Make verification_status consistent
# # change missing format
# unique(data$verification_status)
# data$verification_status <- gsub("\\.","_",data$verification_status)
# qplot(data$verification_status, fill = "identity")
# # pub rec bankruptcies # NA
# unique(data$pub_rec_bankruptcies)
# # deal with na
# table(is.na(data$pub_rec_bankruptcies))
# data$pub_rec_bankruptcies <- ifelse(is.na(data$pub_rec_bankruptcies),"XUK",data$pub_rec_bankruptcies)
# table(data$pub_rec_bankruptcies == "XUK")
# as.factor(data$pub_rec_bankruptcies)
# qplot(data$pub_rec_bankruptcies, fill = "identity")
# table(data$pub_rec_bankruptcies)
# 
# ## ====== Refomat the date column
# 
# # earliest cred line convert to floor date
# data$earliest_cr_line = gsub("\\.","-",data$earliest_cr_line)
# data <- mutate(data, earliest_cr_line = as.Date(paste0("01-",earliest_cr_line), format = "%d-%b-%Y"))
# # test <- dplyr::sample_n(as.tibble(data$earliest_cr_line), 10)
# unique(data$earliest_cr_line)
# # qplot(data$earliest_cr_line, data$total_rec_int, colour = data$inq_last_6mths)
# qplot(data$earliest_cr_line, fill = "identity") ## + facet_wrap(data$loan_status)
# ## we may or may not want to convert the date into continuous time series
# ## it may be advantageous to keep the time format as is
# 
# ###########################################
# 
# # Remove ID to rownames before moedelling
# head(data$member_id)
# row.names(data) <- data$member_id
# data <- select(data, -member_id)
# head(data)
# tail(data)
# tail(default)
# ## note the above transofrmation is only available whilst member id is unique
# 
# # write.csv(data, paste0(working_file_path,"clean_data_v5.csv"),row.names = T)
# # write.csv(data, paste0(working_file_path,"clean_data_30var.csv"),row.names = T)
# data <- read.csv(paste0(working_file_path,"clean_data_30var.csv"), row.names = T)
# 
# ### === END CLEAN

## Data Understanding part 2 ## load data
data <- read.csv(paste0(working_file_path,"clean_data_30var.csv"))
str(data)
table(data$loan_status)
## make target binary
data <- mutate(data, loan_status = if_else(loan_status == "Charged.Off",1,0))
table(data$loan_status)
## remove ID
data <- select(data, -X)

data = as.tibble(data)
int_data <- select(data, -term, -grade, -sub_grade, -home_ownership, 
                   -verification_status, -purpose, -addr_state,
                   -pub_rec_bankruptcies, -earliest_cr_line) # loan_status
int_data$emp_length = gsub("X","", int_data$emp_length)
int_data$emp_length = ifelse(int_data$emp_length == "UK", 0, int_data$emp_length)
int_data$emp_length = as.numeric(int_data$emp_length)
# corrplot::corrplot(int_data, method = "shade")
str(int_data)
default_corplot <- int_data[sapply(int_data, function(x) is_integer(x) || is_double(x))]                                               
par(mfrow = c(1,1))
corrplot::corrplot(cor(default_corplot), method = "shade") 


##
descriptive_info(data, "loan_status")

#####################################################
#####################################################

### Below is work in progress code - don't run

#####################################################
#####################################################



## make loan staus a binary
# table(data$loan_status)
# data$target = 0
# training[training$loan_status == "Fully.Paid", "target"] = 1

# recall that for this challenge loan status is our target
# create data partition row list
set.seed(42)
train = createDataPartition(y = data$loan_status, p = 0.7, list = F)
# partition default data - remove the variable Store7
training = data[train, ]
testing = data[-train, ]

# training$Purchase_binary = 0
# training[training$Purchase == "Charged.Off", "target"] = 1
# 
# testing$Purchase_binary = 0
# testing[testing$Purchase == "Charged.Off", "target"] = 1



## have to decide on tolerance for the correlations
gvdata <- select(data, -term, -grade, -sub_grade, -home_ownership, 
                 -verification_status, -purpose, -addr_state,
                 -pub_rec_bankruptcies, -emp_length)
gvlma::gvlma(lm(loan_status ~., data = gvdata, timeseq = gvdata$earliest_cr_line, family = "binomial"))
?gvlma


##
### up and down sample required?
table()

## visualise data
# test <- mutate(data, check_sameness = ifelse(loan_amnt == funded_amnt, 1,0))
# barplot(table(tmp1$check_sameness), color = "blue")
# tmp1 <- filter(test, loan_status == 1)
# table(tmp1$check_sameness)
# tmp2 <- filter(test, loan_status == 0)
# 
# test <- mutate(data, test = (total_pymnt/loan_amnt)*100)
# n_distinct(test$test)
# summary(test$test)
# sd(test$test)
# qplot(data$installment, data$loan_amnt, geom ="point", colour = as.factor(data$grade)) +facet_wrap(data$grade)
# qplot(test$test, test$loan_amnt, geom ="point", colour = as.factor(test$grade))

## PCA

