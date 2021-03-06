#imputing the data

#https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/amp/
#check for features (columns) and samples (rows) where the data is missing using a simple function
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(validation_default, 2, pMiss)
#install.packages("mice")
library(mice)
#understanding of the pattern of missing data
md.pattern(validation_default)
#imputing the missing data without the earliest credit line date variable
validationData <- mice(validation_default[,-15],m=5,maxit=50,meth='pmm',seed=42)
#see that the imputed data is following the distribution of the other data 
densityplot(validationData)
#turn it back into a data frame 
validation_test <- complete(validationData, include=TRUE)

#> apply(validation_default, 2, pMiss)
#member_id            loan_amnt          funded_amnt 
#0.0000000            0.0000000            0.0000000 
#term             int_rate                grade 
#0.0000000            0.0000000            0.0000000 
#emp_length       home_ownership           annual_inc 
#0.0000000            0.0000000            0.1455075 
#verification_status              purpose           addr_state 
#0.0000000            0.0000000            0.0000000 
#dti          delinq_2yrs     earliest_cr_line 
#0.0000000            1.0549291            1.0549291 
#inq_last_6mths             open_acc              pub_rec 
#1.0549291            1.0549291            1.0549291 
#revol_bal            total_acc          total_pymnt 
#0.0000000            1.0549291            0.0000000 
#total_rec_int   total_rec_late_fee      last_pymnt_amnt 
#0.0000000            0.0000000            0.0000000 
#pub_rec_bankruptcies 
#0.0000000 