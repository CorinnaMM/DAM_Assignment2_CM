###############################################################################
## Session 1 Exercise 3
#  Ridge Regression and Lasso
###############################################################################
##data
clean_just_default <- read_csv("clean_justified_data_25var.csv")
clean_just_default <- clean_just_default %>% mutate_if(is.character, factor)
clean_just_default_no_date <- within(clean_just_default, rm("earliest_cr_line"))
# Ridge regression and lasso can be trained using the glmnet package
#install.packages("glmnet")
library(glmnet)
library(caret)

# recall the OJ data set for classification modelling.  Purchase is our target
# create data partition row list
set.seed(42)
train = createDataPartition(y = clean_just_default_no_date$loan_status, p = 0.7, list = F)
# partition default data - remove the variable Store7
training = clean_just_default_no_date[train, ]
testing = clean_just_default_no_date[-train, ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
x = model.matrix(~ .-member_id, training[, -11])
y = training$loan_status

# We will use the cv.glmnet function to train our model
# This function automatically selects the optimal lambda
# Have a look at the documentation,
#?cv.glmnet

# Setting the alpha argument to 0 does ridge regression, 1 does lasso


###########################
# Lasso Regression

set.seed(42)
# alpha = 1 specifies lasso regression
cv.fit_lasso = cv.glmnet(x, y, family = 'binomial', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min
cv.fit_lasso$lambda.1se
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min)

prediction_lasso = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ .-member_id, testing[, -11]), 
                           type = "class",
                           s = cv.fit_lasso$lambda.min)

lasso_confusion = confusionMatrix(data = prediction_lasso, testing$loan_status)
lasso_confusion
Reference
#Prediction    Charged.Off Fully.Paid
#Charged.Off        1626          1
#Fully.Paid           75      10233

#Accuracy : 0.9936        
#95% CI : (0.992, 0.995)
#No Information Rate : 0.8575        
#P-Value [Acc > NIR] : < 2.2e-16     

#Kappa : 0.9735        
#Mcnemar's Test P-Value : < 2.2e-16     

#Sensitivity : 0.9559        
#Specificity : 0.9999        
#Pos Pred Value : 0.9994        
#Neg Pred Value : 0.9927        
#Prevalence : 0.1425        
#Detection Rate : 0.1362        
#Detection Prevalence : 0.1363        
#Balanced Accuracy : 0.9779        

#'Positive' Class : Charged.Off 
#find the AUC
library(ROCR)
prediction_lasso_prob = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ .-member_id, testing[, -11]), 
                                type = "response",
                                s = cv.fit_lasso$lambda.min)

testing$probability = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ .-member_id, testing[, -11]), 
                              type = "response",
                              s = cv.fit_lasso$lambda.min)
testing_prediction = prediction(testing$probability, testing$loan_status)
test_tpr_fpr = performance(testing_prediction, "tpr","fpr")
data.test.auc = performance(testing_prediction, "auc")
auc = unlist(slot(data.test.auc, "y.values"))
auc
#[1] 0.9929145

###########################
# Evaluate the models

ridge_confusion
lasso_confusion


validation_default <- read_csv("cleaned_validation.csv")
validation_default <- validation_default %>% mutate_if(is.character, factor)
names(which(colSums(is.na(validation_default))>0))
[1] "annual_inc"       "delinq_2yrs"      "earliest_cr_line" "inq_last_6mths"  
[5] "open_acc"         "pub_rec"          "total_acc"       
names(which(colSums(is.na(validation_default_test))>0))

validation_test <- complete(validationData, include=TRUE)

#validation_test$prediction = predict(cv.fit_lasso$glmnet.fit, newx = data.matrix(~.-member_id,validation_test), 
#                                             type = "class",
#                                     s = cv.fit_lasso$lambda.min)

#x.test <- data.matrix(validation_test [,1:(length(validation_test))])
