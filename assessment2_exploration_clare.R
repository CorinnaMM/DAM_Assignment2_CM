library(tidyverse)
library(lubridate)
library(glmnet)
library(caret)
library(plyr)
setwd("/Users/claremaclean/Desktop/DAM/Assessment_2/Assessment2")
default <- read_csv("training.csv")
View(default)

#count target dist
table(default$loan_status)
#Charged.Off  Fully.Paid 
#  5670       34116 
prop.table(table(default$loan_status))
#Charged.Off  Fully.Paid 
# 0.1425124   0.8574876 

default_noID <- default[,c(2:42)]
default_table <- subset(default, select = c("term", "grade", "sub_grade", "emp_length", "home_ownership", "verification_status", "loan_status", "pymnt_plan", "purpose", "addr_state", 
                                            "delinq_2yrs", "inq_last_6mths", "open_acc", "pub_rec", "total_acc", "initial_list_status",
                                            "out_prncp", "out_prncp_inv", "collections_12_mths_ex_med", "policy_code", "application_type",
                                            "acc_now_delinq", "delinq_amnt", "pub_rec_bankruptcies", "tax_liens", "hardship_flag"))

#look at proportion of categoris in each variables 
default_table %>% lapply(table)

#get rid of variables with only one category/value 
default_noID_updated <- within(default_noID, rm("initial_list_status","out_prncp", "out_prncp_inv", "collections_12_mths_ex_med", "policy_code", "application_type",
                                                "acc_now_delinq", "delinq_amnt", "tax_liens", "hardship_flag", "pymnt_plan"))
#default corr plot with numeric variables
library(corrplot)
par(mfrow = c(1,1))
default_corplot <- default_noID_updated[sapply(default_noID_updated, function(x) is_integer(x) || is_double(x))]                                               
corrplot(cor(default_corplot)) 

#density plots
lapply(names(default_noID_updated),
  function(i) 
    ggplot(default_noID_updated, aes_string(x=i, colour = as.factor(default_noID_updated$loan_status))) + geom_density())



