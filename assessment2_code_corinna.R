###########################################################
###            DATA ALGOITHMS AND MEANING
###           ASSESSMENT 2: Classification 
###              LOAN DEFAULT CHALLENGE
###              Corinna Maher Mittmann
###                    SEP 2017
###   DOCUMENT Purpose: Clean Data + Model CART tree & Logistic Regression
###########################################################
Original_Data_Location <- "/Users/corinnamm/Documents/GitHub/DAM_Assignment2_CM/03_DAM_Spring2017_Assignment_2/"
working_file_path <- "/Users/corinnamm/Dropbox/02_Working_data_folder/"
code_file_path <- "/Users/corinnamm/Documents/GitHub/DAM_Assignment2_CM/01_Code/"
wd_path <- "/Users/corinnamm/Documents/GitHub/DAM_Assignment2_CM/"
###
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
library(glmnet)
##
source(paste0(code_file_path,"/99_general_functions_DAM_Corinna.R"))

require(dplyr)
## === START CLEAN
default <- read.csv("/Users/corinnamm/Documents/GitHub/DAM_Assignment2_CM/03_DAM_Spring2017_Assignment_2/training.csv", header = T)
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
# # use custom function from function script to look at densities and descriptive stats
# descriptive_info(default, "loan_status")

review_data_func(default,"Charged.Off", "Fully.Paid")

# remove columns with 1 level
mylist_levels <- default %>%
  summarise_each(funs(n_distinct)) %>%
  tidyr::gather("LoanStatNew", "level") %>% filter(level != 1) %>% select(LoanStatNew)
cols_to_keep <- purrr::as_vector(mylist_levels, .type = "character")
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
data <- default2
# make interest rate numeric
head(unique(data$int_rate))
data$int_rate <- gsub("X", "", data$int_rate)
# check that "." is the last character for every level
table((substr(data$int_rate, nchar(data$int_rate),nchar(data$int_rate))=="."))[[1]]==nrow(data)
n_distinct((substr(data$int_rate, 1,nchar(data$int_rate)-1))) == n_distinct(data$int_rate)
data$int_rate <- substr(data$int_rate, 1,nchar(data$int_rate)-1)
# write.csv(data, paste0(working_file_path,"clean_data_v2.csv"),row.names = F)
# data<- read.csv(paste0(working_file_path,"clean_data_v2.csv"))
str(data)
# Make emp_length consistent
# change missing format
data$emp_length <- gsub("\\.","",data$emp_length)
data$emp_length <- gsub("X", "", data$emp_length)
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
### --------------------- Make verification_status consistent
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
data$pub_rec_bankruptcies <- as.factor(data$pub_rec_bankruptcies)
qplot(data$pub_rec_bankruptcies, fill = "identity")

table(data$pub_rec_bankruptcies)
### --------------------- Refomat the date column
# earliest cred line convert to floor date
data$earliest_cr_line = gsub("\\.","-",data$earliest_cr_line)
data <- mutate(data, earliest_cr_line = as.Date(paste0("01-",earliest_cr_line), format = "%d-%b-%Y"))
# test <- dplyr::sample_n(as.tibble(data$earliest_cr_line), 10)
unique(data$earliest_cr_line)
# qplot(data$earliest_cr_line, data$total_rec_int, colour = data$inq_last_6mths)
qplot(data$earliest_cr_line, fill = "identity") ## + facet_wrap(data$loan_status)
## we may or may not want to convert the date into continuous time series
## it may be advantageous to keep the time format as is
####
## ==== Please note that I am changing the format of this column to days since the publish date of the data.
# These files contain complete loan data for all loans issued through the time period stated, 
# including the current loan status (Current, Late, Fully Paid, etc.) and latest payment information. 
# The file containing loan data through the "present" (note the data was uploaded a year ago to Kaggle by Wendy Kan)
# contains complete loan data for all loans issued through the previous completed calendar quarter.
row.names(data) <- data$member_id
# max(as.Date(new_data$earliest_cr_line))
max(as.Date(old_data$earliest_cr_line))
max(as.Date(data$earliest_cr_line))
## ==== earliest credit line was not seen to be important for the tree model
####
backup_data <- data
data <- mutate(data, earliest_cr_line = as.Date("2017-09-25") - as.Date(earliest_cr_line))
head(data$earliest_cr_line)
data$earliest_cr_line = as.double(data$earliest_cr_line)
names(data)
data <- select(data, -total_pymnt_inv, -installment, -funded_amnt_inv, -grade, -total_rec_prncp)
# write.csv(data, paste0(Original_Data_Location,"version2_training_26_col"),row.names = F)
# data2 <- data %>% mutate(grade = substr(sub_grade, 1,1)) %>% select(-sub_grade)  ### to convert subgrade to grade
data <- read_csv(paste0(Original_Data_Location,"version2_training_26col.csv"))

#####################
# Validation set
#####################
default <- read.csv("/Users/corinnamm/Documents/GitHub/DAM_Assignment2_CM/03_DAM_Spring2017_Assignment_2/validation.csv", header = T)
str(default)
## === START CLEAN
# remove columns not in model data
targetcolnum <- grep("loan_status",names(old_data))
# is_there_a_target <- grep("loan_status",names(default)) #no
cols_to_keep <- names(data[,-targetcolnum]) # 10 is target (loan_status)
typeof(cols_to_keep)
names(default)
default2 = select(default, paste0(cols_to_keep))
names(default2)
### ======== may need to review this decision to remove as 0 and NA values only - ? does distinction btw 0 and NA have value?
#collections # n.a
unique(default2$collections_12_mths_ex_med)
table(default2$collections_12_mths_ex_med)
default2 <- select(default2, -collections_12_mths_ex_med)
# remove tax liens
unique(default2$tax_liens)
table(default2$tax_liens)
default2 <- select(default2, -tax_liens)
data <- default2
# make interest rate numeric
head(unique(data$int_rate))
data$int_rate <- gsub("X", "", data$int_rate)
# check that "." is the last character for every level
table((substr(data$int_rate, nchar(data$int_rate),nchar(data$int_rate))=="."))[[1]]==nrow(data)
n_distinct((substr(data$int_rate, 1,nchar(data$int_rate)-1))) == n_distinct(data$int_rate)
data$int_rate <- substr(data$int_rate, 1,nchar(data$int_rate)-1)
# write.csv(data, paste0(working_file_path,"clean_data_v2.csv"),row.names = F)
# data<- read.csv(paste0(working_file_path,"clean_data_v2.csv"))
str(data)
# Make emp_length consistent
# change missing format
data$emp_length <- gsub("\\.","",data$emp_length)
data$emp_length <- gsub("X", "", data$emp_length)
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
### --------------------- Make verification_status consistent
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
data$pub_rec_bankruptcies <- as.factor(data$pub_rec_bankruptcies)
qplot(data$pub_rec_bankruptcies, fill = "identity")
table(data$pub_rec_bankruptcies)
### --------------------- Refomat the date column
# earliest cred line convert to floor date
data$earliest_cr_line = gsub("\\.","-",data$earliest_cr_line)
data <- mutate(data, earliest_cr_line = as.Date(paste0("01-",earliest_cr_line), format = "%d-%b-%Y"))
# test <- dplyr::sample_n(as.tibble(data$earliest_cr_line), 10)
unique(data$earliest_cr_line)
# qplot(data$earliest_cr_line, data$total_rec_int, colour = data$inq_last_6mths)
qplot(data$earliest_cr_line, fill = "identity") ## + facet_wrap(data$loan_status)
## we may or may not want to convert the date into continuous time series
## it may be advantageous to keep the time format as is
####
## ==== Please note that I am changing the format of this column to days since the publish date of the data.
# These files contain complete loan data for all loans issued through the time period stated, 
# including the current loan status (Current, Late, Fully Paid, etc.) and latest payment information. 
# The file containing loan data through the "present" (note the data was uploaded a year ago to Kaggle by Wendy Kan)
# contains complete loan data for all loans issued through the previous completed calendar quarter.
row.names(data) <- data$member_id
# max(as.Date(new_data$earliest_cr_line))
max(as.Date(old_data$earliest_cr_line))
max(as.Date(data$earliest_cr_line))
## == note == earliest credit line was not seen to be important for the tree model - as a time series consider the value of use
####
backup_data <- data

### PARTITION OUT PROBLEM ROWS
problem_rows <- data.frame(member_id = NA)
tbl_df(data)
problem_rows_func(problem_rows = problem_rows, new_data = tbl_df(data))
class(problem_rows)
problem_row_id <- distinct(tbl_df(problem_rows))
problem_rows <- tbl_df(problem_rows)
problem_rows <- distinct(problem_rows)
problem_rows <- left_join(problem_rows,data)
data2 <- mutate(data, problem = if_else(member_id %in% problem_row_id[[1]], 1,0))
table(data2$problem)
# problem_rows_complete
back_up <- data2
## remove problem rows from data

data <- data2 %>% filter(problem == 0 ) %>% select(-problem)

data <- mutate(data, earliest_cr_line = as.Date("2017-09-25") - as.Date(earliest_cr_line))
head(data$earliest_cr_line)
data$earliest_cr_line = as.double(data$earliest_cr_line)
names(data)
# write_csv(data, paste0(Original_Data_Location, "version2_validation_noNA.csv"))
# data <- select(data, -total_pymnt_inv, -installment, -funded_amnt_inv, -grade, -total_rec_prncp)
# write_csv(data, paste0(Original_Data_Location, "version2_validation_25col_noNA.csv"))
# data2 <- data %>% mutate(grade = substr(sub_grade, 1,1)) %>% select(-sub_grade)  ### to convert subgrade to grade

NAdata <- data2 %>% filter(problem == 1 ) %>% select(-problem)
# write_csv(NAdata, paste0(Original_Data_Location, "version2_validation_all_columns_NA.csv"))
NAdata <- select(NAdata, -total_pymnt_inv, -installment, -funded_amnt_inv, -grade, -total_rec_prncp)
# write_csv(NAdata, paste0(Original_Data_Location, "version2_validation_25col_NA.csv"))

data <- bind_rows(data,NAdata)
# write_csv(data, paste0(Original_Data_Location, "version2_validation_full_25col.csv"))

# write_csv(data, paste0(Original_Data_Location, "version2_validation_full.csv"))

## ===== END CLEANING

## Data Understanding part 2 ## load data
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

## Clean up workspace
data <- backup_data
rm(int_data, default, default2, default_corplot)
##################
## MODELLING
##################

# old_data <- read.csv(paste0(working_file_path,"clean_justified_data_25var.csv"))
# default <- read.csv(paste0(Original_Data_Location,".csv"), header = T)
# write_csv(data, paste0(working_file_path,"v2_clean_justified_data_25var"))
# data <- read_csv(paste0(working_file_path,"v2_clean_justified_data_25var.csv"))
data <- read_csv(paste0(Original_Data_Location,"version2_training_26col.csv"))
# Renove ID
row.names(data) <- data$member_id
data <- select(data, -member_id)
str(data)
data$loan_status <- as.factor(data$loan_status)
data<- data %>% mutate(                 loan_amnt = as.numeric(loan_amnt),
                                        funded_amnt = as.numeric(funded_amnt),
                                        term = as.factor(term),
                                        int_rate = as.numeric(int_rate),
                                        sub_grade = as.factor(sub_grade), # = substr(sub_grade, 1,1),
                                        emp_length = as.factor(emp_length),
                                        home_ownership = as.factor(home_ownership),
                                        annual_inc = as.numeric(annual_inc),
                                        verification_status = as.factor(verification_status),
                                        loan_status = as.factor(loan_status),
                                        purpose = as.factor(purpose),
                                        addr_state = as.factor(addr_state),
                                        dti = as.numeric(dti),
                                        delinq_2yrs = as.numeric(delinq_2yrs),
                                        earliest_cr_line = as.numeric(earliest_cr_line),
                                        inq_last_6mths = as.numeric(inq_last_6mths),
                                        open_acc = as.numeric(open_acc),
                                        pub_rec = as.numeric(pub_rec),
                                        revol_bal = as.numeric(revol_bal),
                                        total_acc = as.numeric(total_acc),
                                        total_pymnt = as.numeric(total_pymnt),
                                        total_rec_int = as.numeric(total_rec_int),
                                        total_rec_late_fee = as.numeric(total_rec_late_fee),
                                        last_pymnt_amnt = as.numeric(last_pymnt_amnt),
                                        pub_rec_bankruptcies = as.factor(pub_rec_bankruptcies))
str(data)
data <- tbl_df(data)
class(data)
###############
## SUB SAMPLING FOR UNBALANCED CLASS
#to improve the model subsampling is also performed on an alternate dataset
###############
## Partitioning to train the glmnet model
#dummy factors
data_backup <- data
#######################################
set.seed(42)
train = createDataPartition(y = data$loan_status, p = 0.7, list = F)
# partition default data
training = data[train, ]
training <- mutate(training, loan_status = if_else(loan_status == "Charged.Off",1,0))
dmy <- dummyVars(" ~ .", data = training, fullRank = F)
glmDF <- as.data.frame(predict(dmy,training))
glmDF <- mutate(glmDF, loan_status = if_else(loan_status == 1,"Charged.Off","Fully.Paid"))
training <- mutate(training, loan_status = if_else(loan_status == 1,"Charged.Off","Fully.Paid"))
testing = data[-train, ]
testing <- mutate(testing, loan_status = if_else(loan_status == "Charged.Off",1,0))
dmy <- dummyVars(" ~ .", data = testing, fullRank = F)
testDF <- as.data.frame(predict(dmy,testing))
testDF <- mutate(testDF, loan_status = if_else(loan_status == 1,"Charged.Off","Fully.Paid"))
testing <- mutate(testing, loan_status = if_else(loan_status == 1,"Charged.Off","Fully.Paid"))
table(testing$loan_status)
table(training$loan_status)
## upsampled dataframe
# imbal_training$ID <- rownames(imbal_training)
set.seed(42)
x <- select(glmDF, -loan_status)
set.seed(42)
up_glmDF <- upSample(x = x,
                     y = as.factor(glmDF$loan_status), list = F, yname = "loan_status")
set.seed(42)
down_training <- downSample(x = x,
                            y = training$loan_status, list = F, yname = "loan_status")
# Even class balance will present problems for interpretability
#
up_training <- up_glmDF
table(up_training$loan_status)
# head(up_training$x)
class(up_training)
str(up_training)
backup_up_train_up <- up_training
backup_up_train_dn <- down_training
targetcol <- grep("loan_status", names(up_training))
set.seed(42)

## BAGGED CART WITH SMALLER DATA SET With either GRADE/SUBGRADE used
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

set.seed(42)
upmod_inside <- train(x = up_training[,-ncol(up_training)],y=up_training[,ncol(up_training)], 
                      method = "treebag",
                      nbagg = 50,
                      metric = "ROC",
                      trControl = ctrl)

## cannot have binary level labels for the caret packet will throw error as 0 and 1 levels are not valid format

## Evaluation

str(upmod_inside)
upmod_inside$results
## ===== confusion matrix
# table(repurchase.rf$test$predicted,testing$Target)
te <- grep("loan_status",names((testDF)))
predictions_upmod <- predict(upmod_inside$finalModel,testDF[,-te],type="class")
probability_upmod <- predict(upmod_inside$finalModel,testing[,-te],type="prob")
# cbind(testing, predictions_upmod)
# cbind(testing,probability_upmod)
# test_validationfile <- testing
wow <- confusionMatrix(predictions_upmod,testDF[,te])
wow
wow$byClass

## ==== ROCR
typeof(predictions_upmod)
tesi<- testDF

tesi$pred <- predictions_upmod
binary<- mutate(tesi, pred = ifelse(pred == "Charged.Off", 1,0), loan_status = ifelse(loan_status == "Charged.Off", 1,0)) 
testing_prediction = prediction(binary$pred, binary$loan_status)
data.test.auc <- ROCR::performance(testing_prediction, "auc")
auc = unlist(slot(data.test.auc, "y.values"))
#

# model_evaluation(testDF,training, upmod_inside$finalModel, target = "loan_status")

impmod<- varImp(upmod_inside$finalModel)
plot(impmod)
### =========

##############################
## predict the validation set
new_data <- read_csv(paste0(Original_Data_Location,"version2_validation_full_25col.csv"))

## ====== prep submission prediction data
new_data<- new_data %>% mutate(                 loan_amnt = as.numeric(loan_amnt),
                                                funded_amnt = as.numeric(funded_amnt),
                                                term = as.factor(term),
                                                int_rate = as.numeric(int_rate),
                                                grade = as.factor(grade),
                                                emp_length = as.factor(emp_length),
                                                home_ownership = as.factor(home_ownership),
                                                annual_inc = as.numeric(annual_inc),
                                                verification_status = as.factor(verification_status),
                                                purpose = as.factor(purpose),
                                                addr_state = as.factor(addr_state),
                                                dti = as.numeric(dti),
                                                delinq_2yrs = as.numeric(delinq_2yrs),
                                                earliest_cr_line = ymd(earliest_cr_line),
                                                inq_last_6mths = as.numeric(inq_last_6mths),
                                                open_acc = as.numeric(open_acc),
                                                pub_rec = as.numeric(pub_rec),
                                                revol_bal = as.numeric(revol_bal),
                                                total_acc = as.numeric(total_acc),
                                                total_pymnt = as.numeric(total_pymnt),
                                                total_rec_int = as.numeric(total_rec_int),
                                                total_rec_late_fee = as.numeric(total_rec_late_fee),
                                                last_pymnt_amnt = as.numeric(last_pymnt_amnt),
                                                pub_rec_bankruptcies = as.factor(pub_rec_bankruptcies))

str(new_data)
row.names(new_data) <- new_data$member_id
id <- grep("member_id",names((new_data)))
### If the dummy variables names are not the same - to insert the . for the dummy names below
# colnames(new_data)[4] <- paste0(colnames(new_data)[4],".")
# colnames(new_data)[6] <- paste0(colnames(new_data)[6],".")
# colnames(new_data)[7] <- paste0(colnames(new_data)[7],".")
# colnames(new_data)[8] <- paste0(colnames(new_data)[8],".")
# colnames(new_data)[10] <- paste0(colnames(new_data)[10],".")
# colnames(new_data)[11] <- paste0(colnames(new_data)[11],".")
# colnames(new_data)[12] <- paste0(colnames(new_data)[12],".")
# colnames(new_data)[25] <- paste0(colnames(new_data)[25],".")
## ====================
# testing2 <- mutate(new_data, loan_status = if_else(loan_status == "Charged.Off",1,0))
dmy <- dummyVars(" ~ .", data = new_data[,-id], fullRank = F)
testDF2 <- as.data.frame(predict(dmy,new_data[,-id]))
# reorder names(testDF)
testDF3 <- testDF2
testDF3$addr_state.ME <- 0
# names(testDF3) <- (names(testDF))

# testDF2 <- mutate(testDF2, loan_status = if_else(loan_status == 1,"Charged.Off","Fully.Paid"))
# testing2 <- mutate(testing2, loan_status = if_else(loan_status == 1,"Charged.Off","Fully.Paid"))

### ============== New Submission

loan_status_pred <- predict(upmod_inside$finalModel,testDF2[,-id],type="class")
prob_upmod <- predict(upmod_inside$finalModel,testDF3,type="prob")

new_pred <- cbind(new_data,prob_upmod)
names(new_pred)
new_pred <- select(new_pred, member_id, Charged.Off)
names(new_pred) <- c("member_id",   "probability")
write_csv(new_pred, paste0(Original_Data_Location, "bag_cart_upsamp_5rptcv_50nbagg_submission2.csv"))

##############################
## predict the validation set
new_data <- read_csv(paste0(Original_Data_Location,"version2_validation_full_25col.csv"))

## ====== prep submission prediction data for tree
new_data<- new_data %>% mutate(                 loan_amnt = as.numeric(loan_amnt),
                                                funded_amnt = as.numeric(funded_amnt),
                                                term = as.factor(term),
                                                int_rate = as.numeric(int_rate),
                                                grade = as.factor(grade),
                                                emp_length = as.factor(emp_length),
                                                home_ownership = as.factor(home_ownership),
                                                annual_inc = as.numeric(annual_inc),
                                                verification_status = as.factor(verification_status),
                                                purpose = as.factor(purpose),
                                                addr_state = as.factor(addr_state),
                                                dti = as.numeric(dti),
                                                delinq_2yrs = as.numeric(delinq_2yrs),
                                                earliest_cr_line = ymd(earliest_cr_line),
                                                inq_last_6mths = as.numeric(inq_last_6mths),
                                                open_acc = as.numeric(open_acc),
                                                pub_rec = as.numeric(pub_rec),
                                                revol_bal = as.numeric(revol_bal),
                                                total_acc = as.numeric(total_acc),
                                                total_pymnt = as.numeric(total_pymnt),
                                                total_rec_int = as.numeric(total_rec_int),
                                                total_rec_late_fee = as.numeric(total_rec_late_fee),
                                                last_pymnt_amnt = as.numeric(last_pymnt_amnt),
                                                pub_rec_bankruptcies = as.factor(pub_rec_bankruptcies))

str(new_data)
row.names(new_data) <- new_data$member_id
id <- grep("member_id",names((new_data)))
colnames(new_data)[4] <- paste0(colnames(new_data)[4],".")
colnames(new_data)[6] <- paste0(colnames(new_data)[6],".")
colnames(new_data)[7] <- paste0(colnames(new_data)[7],".")
colnames(new_data)[8] <- paste0(colnames(new_data)[8],".")
colnames(new_data)[10] <- paste0(colnames(new_data)[10],".")
colnames(new_data)[11] <- paste0(colnames(new_data)[11],".")
colnames(new_data)[12] <- paste0(colnames(new_data)[12],".")
colnames(new_data)[25] <- paste0(colnames(new_data)[25],".")
## ====================
# testing2 <- mutate(new_data, loan_status = if_else(loan_status == "Charged.Off",1,0))
dmy <- dummyVars(" ~ .", data = new_data[,-id], fullRank = F)
testDF2 <- as.data.frame(predict(dmy,new_data[,-id]))
# reorder names(testDF)
testDF3 <- testDF2
testDF3$addr_state.ME <- 0
# names(testDF3) <- (names(testDF))

# testDF2 <- mutate(testDF2, loan_status = if_else(loan_status == 1,"Charged.Off","Fully.Paid"))
# testing2 <- mutate(testing2, loan_status = if_else(loan_status == 1,"Charged.Off","Fully.Paid"))

### ============== New Submission

loan_status_pred <- predict(upmod_inside$finalModel,testDF2[,-id],type="class")
prob_upmod <- predict(upmod_inside$finalModel,testDF3,type="prob")

new_pred <- cbind(new_data,prob_upmod)
names(new_pred)
new_pred <- select(new_pred, member_id, Charged.Off)
names(new_pred) <- c("member_id",   "probability")
write_csv(new_pred, paste0(Original_Data_Location, "bag_cart_upsamp_5rptcv_50nbagg_submission2.csv"))


### ============================================================
## GLM MODELS - manual 
### ============================================================


###########################
# Partitioning
###########################

# We want to partition our data into 70% for training, 30% for testing

# create data partition row list
set.seed(42)  # setting a random seed ensures we get the same result each time
# We will use the function 'createDataPartition' from the caret package
# ?createDataPartition
train = createDataPartition(y = data$loan_status, p = 0.75, list = F)
# partition data data into two sets 
training = data[train, ]
testing = data[-train, ]
str(training)
str(testing)
traincol <- grep("loan_status",names(training))
set.seed(42)
down_training <- caret::downSample(x = training[,-traincol],
                                   y = as.factor(training$loan_status), list = FALSE, yname = "loan_status")
class(down_training)
up_training <- caret::upSample(x = training[,-traincol],
                               y = as.factor(training$loan_status), list = FALSE, yname = "loan_status")

###########################
# Variable selection
###########################
table(down_training$loan_status)
# In this section, we will select which variables we want to include in our model
# We'll do this by backwards selection - start with everything and remove one by one

# let's start by throwing all the variables into the logistic regression
data.glm = glm(formula = loan_status ~ .-member_id -home_ownership,
               data = down_training,
               family = "binomial")
summary(data.glm)
data.glm$formula
# AIC ~ 607
## downsampled AIC ~ 268
# It's clear that we can remove some variables. This should drop the AIC
# AIC ~ 604

print(names(data))
# We can probably remove a few more
data.glm = glm(formula = loan_status ~ loan_amnt + term + int_rate + annual_inc + dti + delinq_2yrs + earliest_cr_line + inq_last_6mths + open_acc + pub_rec + revol_bal + total_acc          
               + total_pymnt  + total_rec_late_fee + last_pymnt_amnt,
               data = down_training,
               family = "binomial")
summary(data.glm)
# AIC ~ 11492
data.glm = glm(formula = loan_status ~ loan_amnt + term + int_rate + total_acc          
               + total_pymnt  + total_rec_late_fee + last_pymnt_amnt,
               data = up_training,
               family = "binomial")
summary(data.glm)
# AIC ~11538

glmDF <- mutate(down_training, loan_status = if_else(loan_status == "Charged.Off",1,0))

glmDF <- mutate(data, loan_status = if_else(loan_status == "Charged.Off",1,0))
data.glm = glm(formula = loan_status ~ loan_amnt + purpose + funded_amnt + term + int_rate + annual_inc + dti + addr_state +installment + 
                 earliest_cr_line + inq_last_6mths + open_acc + pub_rec + revol_bal + total_acc + pub_rec_bankruptcies         
               + total_pymnt  + total_rec_late_fee + last_pymnt_amnt + home_ownership +emp_length +verification_status +sub_grade,
               data = glmDF,
               family = "binomial")
# AIC ~ 6908.6

data.glm = glm(formula = loan_status ~ loan_amnt + purpose + funded_amnt + term + int_rate + annual_inc + dti + installment + 
                 total_pymnt  + total_rec_late_fee + last_pymnt_amnt + emp_length + sub_grade,
               data = glmDF,
               family = "binomial")
summary(data.glm)
# AIC ~ 6878.6
glmDF <- mutate(data, loan_status = if_else(loan_status == "Charged.Off",1,0))

data.glm = glm(formula = loan_status ~ loan_amnt + purpose + funded_amnt + term + int_rate + annual_inc + dti +installment + 
                 inq_last_6mths + open_acc + pub_rec + revol_bal + total_acc + pub_rec_bankruptcies         
               + total_pymnt  + total_rec_late_fee + last_pymnt_amnt + home_ownership +emp_length +verification_status +sub_grade,
               data = glmDF,
               family = "binomial")
summary(data.glm)
# AIC ~ 6876.6
# Let's stick with this last model
data.glm3 = glm(formula = loan_status ~ loan_amnt + purpose + funded_amnt + term + int_rate + annual_inc + dti + installment + 
                  earliest_cr_line + inq_last_6mths + open_acc + pub_rec + revol_bal + total_acc + pub_rec_bankruptcies         
                + total_pymnt  + total_rec_late_fee + last_pymnt_amnt +emp_length + sub_grade,
                data = glmDF,
                family = "binomial")
summary(data.glm3)
# AIC ~ 6868.6

data.glm2 = glm(formula = loan_status ~ loan_amnt + purpose + funded_amnt + term + int_rate + annual_inc + dti +installment + 
                  earliest_cr_line + inq_last_6mths + open_acc + pub_rec + revol_bal + total_acc + pub_rec_bankruptcies         
                + total_pymnt  + total_rec_late_fee + last_pymnt_amnt +emp_length + verification_status + sub_grade,
                data = glmDF,
                family = "binomial")
summary(data.glm2)
# AIC ~ 6871.1
# Let's stick with this last model

###########################
# Create probabilities and predictions
###########################
data.glm <- data.glm2
# add the probabilities to the testing data
# grep("home_ownership")
testing$probability = predict(data.glm2, newdata = testing, type = "response")
# ?predict.glm
# assume that the optimum probability threshold is 0.5
# Create the class prediction - our target is the "MM" class
testing$prediction = "Fully.Paid"
testing[testing$probability >= 0.5, "prediction"] = "Charged.Off"

# Have a look at the data
head(testing)

###########################
# Evaluation

# Create a confusion matrix (along with other measures) using the 
# function 'confusionMatrix' from the caret package

confusionMatrix(data = testing$prediction, testing$loan_status)

options(scipen = 999)
testing$probability


prob <- predict(data.glm,new_data[,-1],type="response")

## Function to run through different combinations of glm models

new_data <- read_csv("/Users/corinnamm/Documents/GitHub/DAM_Assignment2_CM/03_DAM_Spring2017_Assignment_2/version2_validation_all_col_full.csv")
new_data$pub_rec_bankruptcies <- as.character(new_data$pub_rec_bankruptcies)
## remove experiemental rows from data
# data2 <- select(data, -loan_status,-funded_amnt_inv_prop,-installment_prop, -total_p, -check)
# data2 is used to ensure the same columns are selected for the validation set as those used to train the models
# data2 <- select(data, -loan_status)
new_data <- new_data[,colnames(data2)]

## use data2 to find the mean of the non-NA data to impute into validation set
new_data$annual_inc = ifelse(is.na(new_data$annual_inc),(mean(data2$annual_inc)),new_data$annual_inc)
new_data$delinq_2yrs = ifelse(is.na(new_data$delinq_2yrs),(mean(data2$delinq_2yrs)),new_data$delinq_2yrs)
new_data$earliest_cr_line = ifelse(is.na(new_data$earliest_cr_line),(mean(data2$earliest_cr_line)),new_data$earliest_cr_line)
new_data$open_acc = ifelse(is.na(new_data$open_acc),(mean(data2$open_acc)),new_data$open_acc)
new_data$pub_rec = ifelse(is.na(new_data$pub_rec),(mean(data2$pub_rec)),new_data$pub_rec)
new_data$total_acc = ifelse(is.na(new_data$total_acc),(mean(data2$total_acc)),new_data$total_acc)
new_data$inq_last_6mths = ifelse(is.na(new_data$inq_last_6mths),(mean(data2$inq_last_6mths)),new_data$inq_last_6mths)
new_data$acc_now_delinq = ifelse(is.na(new_data$acc_now_delinq),(mean(data$acc_now_delinq)),new_data$acc_now_delinq)

# check the tranformation before prediction
summary(new_data)
## lets look at the problem rows of the validation that have been imputed against the original
new_NA <- read_csv("/Users/corinnamm/Documents/GitHub/DAM_Assignment2_CM/03_DAM_Spring2017_Assignment_2/version2_validation_25col_NA.csv")
temp <- left_join(new_NA,new_data, by = "member_id")

# check that the new data has the same length as required post transformation
nrow(new_data)
ncol(new_data)
table(is.na(new_data))
str(new_data)


# new_data <- validation_cleaned_data_nick
# problem_rows_func(new_data_original)
# new_NA <- (validation_cleaned_data_nick, member_id == problem_rows[,"member_id"][[1]])

## Predictions for the validation set for the best glm model
# data.glm$formula
# loan_status ~ loan_amnt + purpose + funded_amnt + term + int_rate + 
#   annual_inc + dti + installment + total_pymnt + total_rec_late_fee + 
#   last_pymnt_amnt + emp_length + grade
testing <- mutate(testing, loan_status = if_else(loan_status == "Fully.Paid",0,1))
testing_prediction = prediction(testing$probability, testing$loan_status)
test_tpr_fpr = performance(testing_prediction, "tpr","fpr")
data.test.auc = performance(testing_prediction, "auc")
auc = unlist(slot(data.test.auc, "y.values"))
auc
##
prediction_prob = predict(data.glm, newdata = new_data[,-1],
                                type = "response")

new_data$probability = predict(data.glm, newdata = new_data[,-1],
                                 type = "response")

new_data$prediction = "Fully.Paid"
new_data[new_data$probability >= 0.5, "prediction"] = "Charged.Off"

new_submission <- select(new_data, member_id, probability)
table(is.na(new_submission))
# write_csv <- write_csv(new_submission, paste0(Original_Data_Location, "glm_submission_oct2_v4.csv"))
# write_csv <- write_csv(new_submission, paste0(Original_Data_Location, "glm_submission_oct2_v3.csv"))

### PLOT AUC RESULTS
kaggle30pct <- data.frame(AUC = c(0.99731,0.98587,0.99727,0.99564, 0.98194,0.97813,0.98095,0.98053,0.97435,0.98095, 0.97343),
                          model_name = c("SVM_no_addr_state", "glm_feature_selected", "svm_all_vars", "lasso_all_var","lasso_excl_state","bag_cart_2_subgrade", "svm_var_select", "lasso_var_selectv1", "nnet_var_select", "svm_var_selectv1","bag_cart_1_grade"))
kaggle30pct <<- as.data.frame(kaggle30pct[order(-kaggle30pct$AUC),])
row.names(kaggle30pct) = seq(1:11)
kaggle30pct$model_name <- factor(kaggle30pct$model_name, levels = kaggle30pct$model_name[order(kaggle30pct$AUC)]) 
names(kaggle30pct) <- c("AUC Score", "Model")
ggplot(kaggle30pct, aes(y = `AUC Score`, x=Model, fill = `AUC Score`)) +geom_col() +coord_flip() + labs(title = "The Submitted Models by AUC Score")

rm(kaggle30pct)
