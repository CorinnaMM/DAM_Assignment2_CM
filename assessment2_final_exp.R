###CLARE MACLEAN EXPLORATION CODE 
##exploration code

library(tidyverse)
library(lubridate)
library(glmnet)
library(caret)
library(plyr)
setwd("")
default <- read_csv("training.csv")
View(default)

#count target distribution
table(default$loan_status)
#Charged.Off  Fully.Paid 
#  5670       34116 
prop.table(table(default$loan_status))
#Charged.Off  Fully.Paid 
# 0.1425124   0.8574876 
#another class imbalance problem

#look at proportion of categories in each of the variables (where applicable)
default_table <- subset(default, select = c("term", "grade", "sub_grade", "emp_length", "home_ownership", "verification_status", "loan_status", "pymnt_plan", "purpose", "addr_state", 
                                            "delinq_2yrs", "inq_last_6mths", "open_acc", "pub_rec", "total_acc", "initial_list_status",
                                            "out_prncp", "out_prncp_inv", "collections_12_mths_ex_med", "policy_code", "application_type",
                                            "acc_now_delinq", "delinq_amnt", "pub_rec_bankruptcies", "tax_liens", "hardship_flag"))

default_table %>% lapply(table)

#get rid of variables with only one category/value 
default_noID <- default[,c(2:42)]
default_noID_updated <- within(default_noID, rm("initial_list_status","out_prncp", "out_prncp_inv", "collections_12_mths_ex_med", "policy_code", "application_type",
                                                "acc_now_delinq", "delinq_amnt", "tax_liens", "hardship_flag", "pymnt_plan"))
#default corr plot with numeric variables
library(corrplot)
par(mfrow = c(1,1))
default_corplot <- default_noID_updated[sapply(default_noID_updated, function(x) is_integer(x) || is_double(x))]                                               
corrplot(cor(default_corplot)) 
cor(default_corplot)
#look at how similar the correlated columns are 
library(compare)
compare(default_noID_updated["loan_amnt"], default_noID_updated["funded_amnt"])

#density plots
lapply(names(default_noID_updated),
       function(i) 
         ggplot(default_noID_updated, aes_string(x=i, colour = as.factor(default_noID_updated$loan_status))) + geom_density())


#find out how many values in funded_amnt and loan_amnt differ from each other
#clean_default[!clean_default$loan_amnt%in%clean_default$funded_amnt,]
test <- mutate(data,check_sameness = ifelse(loan_amnt == funded_amnt, 1,0))
test_tot_pay <- mutate(clean_default, check_sameness = ifelse(total_pymnt == total_pymnt_inv, 1,0))
#plot the difference
plot(clean_default$loan_amnt, clean_default$funded_amnt)

#look at clean data 
clean_default <- read_csv("clean_data_30var 2.csv")
clean_default <- clean_default %>% mutate_if(is.character, factor)

#create a correlation plot of the numeric variables in the data
library(corrplot)
par(mfrow = c(1,1))
default_corplot_clean <- clean_default[sapply(clean_default, function(x) is_integer(x) || is_double(x))]
default_corplot_clean <- within(default_corplot_clean, rm("earliest_cr_line"))
corrplot(cor(default_corplot_clean)) 

#transform target loan_status 
clean_default$loan_status <- ifelse(clean_default$loan_status == "Charged.Off",1,0)
#try to do a vif on the variables
clean_default_no_ID <- within(clean_default, rm("X1"))
lm.fit <- lm(loan_status ~.-grade, data = clean_default)

#look at the variance inflation factors in the variables
#install.packages("car")
library(car)
alias(loan_status ~., data = clean_default)
#need to remove grade to run vif as it is exactly colinear with sub grade
#https://statisticalhorizons.com/multicollinearity
"vif"(lm.fit, na.action = na.exclude)
#                            GVIF Df GVIF^(1/(2*Df))
#X1                     1.003578  1        1.001787
#loan_amnt             28.919163  1        5.377654
#funded_amnt          180.709389  1       13.442819
#funded_amnt_inv       96.610384  1        9.829058
#term                   5.454204  1        2.335424
#int_rate              25.822923  1        5.081626
#installment           54.023859  1        7.350092
#sub_grade             33.672903 34        1.053077
#emp_length             1.291730 10        1.012881
#home_ownership         1.467463  4        1.049110
#annual_inc             1.253627  1        1.119655
#verification_status    1.339877  2        1.075886
#purpose                1.420056 13        1.013580
#addr_state             1.367149 49        1.003196
#dti                    1.293329  1        1.137246
#delinq_2yrs            1.087045  1        1.042615
#earliest_cr_line       1.403015  1        1.184489
#inq_last_6mths         1.108566  1        1.052884
#open_acc               2.035325  1        1.426648
#pub_rec                3.494421  1        1.869337
#revol_bal              1.403887  1        1.184857
#total_acc              2.401379  1        1.549638
#total_pymnt          319.574523  1       17.876647
#total_pymnt_inv      135.167417  1       11.626152
#total_rec_prncp      114.729746  1       10.711197
#total_rec_int         24.550416  1        4.954838
#total_rec_late_fee     1.049715  1        1.024556
#last_pymnt_amnt        2.143938  1        1.464219
#pub_rec_bankruptcies   4.741866  3        1.296159

#adjust variables to see vifs 
View(clean_default)
summary(lm.fit)
lm.fit_select <- lm(loan_status ~.-grade -X1-funded_amnt - total_pymnt, data = clean_default)
summary(lm.fit_select)
"vif"(lm.fit_select, na.action = na.exclude)
#adjust variables to see vifs - this time with subgrade 
lm.fit_select2 <- lm(loan_status ~.-sub_grade-X1-funded_amnt-total_pymnt-funded_amnt-total_pymnt, data = clean_default)
summary(lm.fit_select2)
"vif"(lm.fit_select2, na.action = na.exclude)
#                       GVIF Df GVIF^(1/(2*Df))
#loan_amnt            19.510384  1        4.417056
#term                  3.310691  1        1.819530
#int_rate             15.116729  1        3.888024
#installment          18.854630  1        4.342192
#grade                15.485037  6        1.256491

#look at vif if we remove installment
lm.fit_select3 <- lm(loan_status ~.-grade-X1-funded_amnt-total_pymnt-funded_amnt_inv-total_pymnt_inv-installment, data = clean_default)
summary(lm.fit_select3)
"vif"(lm.fit_select3, na.action = na.exclude)
#GVIF Df GVIF^(1/(2*Df))
#loan_amnt             4.959457  1        2.226984
#term                  1.902089  1        1.379162
#int_rate             24.162507  1        4.915537
#sub_grade            30.068336 34        1.051325
#emp_length            1.285963 10        1.012655

#plot of installment vs loan amount with grade colour facet 
ggplot(clean_default, aes(x = installment, y = loan_amnt, colour = as.factor(grade))) + geom_point() 

#read in new clean data 
clean_just_default <- read_csv("clean_justified_data_25var.csv")
clean_just_default2 <- read_csv("modellingdata1.csv")
clean_just_default <- clean_just_default %>% mutate_if(is.character, factor)




