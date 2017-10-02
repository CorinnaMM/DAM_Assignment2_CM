####model code
##########################
###lasso model code#####
#install.packages("glmnet")
library(glmnet)
library(caret)

# CRISP DM - create data partition row list
set.seed(42)
clean_just_default <- read_csv("clean_justified_data_25var.csv")
clean_just_default <- clean_just_default %>% mutate_if(is.character, factor)
clean_just_default_no_ID <- within(clean_just_default, rm("member_id"))
train = createDataPartition(y = clean_just_default_no_ID$loan_status, p = 0.7, list = F)
# partition default data - remove the variable Store7
training = clean_just_default_no_ID[train, ]
testing = clean_just_default_no_ID[-train, ]

# glmnet  requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
x = model.matrix(~ ., training[, -10])
y = training$loan_status


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

prediction_lasso = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testing[, -10]), 
                           type = "class",
                           s = cv.fit_lasso$lambda.min)

lasso_confusion = confusionMatrix(data = prediction_lasso, testing$loan_status)
#lasso_confusion
#Prediction    Charged.Off Fully.Paid
#Charged.Off        1627          2
#Fully.Paid           74      10232

#Accuracy : 0.9936        
#95% CI : (0.992, 0.995)
#No Information Rate : 0.8575        
#P-Value [Acc > NIR] : < 2.2e-16 
#Kappa : 0.9735        
#Mcnemar's Test P-Value : 3.816e-16     

#Sensitivity : 0.9565        
#Specificity : 0.9998        
#Pos Pred Value : 0.9988        
#Neg Pred Value : 0.9928        
#Prevalence : 0.1425        
#Detection Rate : 0.1363        
#Detection Prevalence : 0.1365        
#Balanced Accuracy : 0.9782        

#'Positive' Class : Charged.Off

#find the AUC
library(ROCR)
prediction_lasso_prob = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testing[, -10]), 
                                type = "response",
                                s = cv.fit_lasso$lambda.min)

testing$probability = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testing[, -10]), 
                              type = "response",
                              s = cv.fit_lasso$lambda.min)
testing_prediction = prediction(testing$probability, testing$loan_status)
test_tpr_fpr = performance(testing_prediction, "tpr","fpr")
data.test.auc = performance(testing_prediction, "auc")
auc = unlist(slot(data.test.auc, "y.values"))
auc
#[1] 0.9928011

#predict with validation set 
validation_default <- read_csv("cleaned_validation.csv")
validation_default <- validation_default %>% mutate_if(is.character, factor)
#there's missing values in the data set 
names(which(colSums(is.na(validation_default))>0))
[1] "annual_inc"       "delinq_2yrs"      "earliest_cr_line" "inq_last_6mths"  
[5] "open_acc"         "pub_rec"          "total_acc"       
names(which(colSums(is.na(validation_default_test))>0))
#impute data to reduct missing values and remove earliest_cr_line variable is it's a date 
library(randomForest)
validation_default <- na.roughfix(validation_default[,-15])
#this doesn't work
validation_default$prediction = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., validation_default[, -1]), 
                                             type = "class",
                                             s = cv.fit_lasso$lambda.min)

##################################
##################################
#svm model 

#load e1071 if not loaded
library(e1071)

#data
##data
clean_just_default <- read_csv("clean_justified_data_25var.csv")
clean_just_default <- clean_just_default %>% mutate_if(is.character, factor)
clean_just_default_no_date <- within(clean_just_default, rm("earliest_cr_line"))
set.seed(42)
clean_just_default_no_date[,"train"] <- ifelse(runif(nrow(clean_just_default_no_date))<0.8,1,0)
#write dataframe to disk to check
#write.csv(clean_just_default_no_date,"clean_just_default.csv")
#separate training and test sets
trainset <- clean_just_default_no_date[clean_just_default_no_date$train==1,]
testset <- clean_just_default_no_date[clean_just_default_no_date$train==0,]
trainColNum <- grep("train",names(trainset))
typeColNum <- grep("loan_status",names(clean_just_default_no_date))
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]

#Try linear kernel...
svm_model<- svm(loan_status~ .-member_id, data=trainset, method="C-classification", kernel="linear", probability = TRUE)
pred_train <- predict(svm_model,trainset)
mean(pred_train==trainset$loan_status)
#[1] 0.9931981
pred_test <- predict(svm_model,testset)
mean(pred_test==testset$loan_status)
#[1] 0.992279
svm_confusion = confusionMatrix(data = pred_test, testset$loan_status)

#Prediction    Charged.Off Fully.Paid
#Charged.Off        1075          0
#Fully.Paid           62       6893

#Accuracy : 0.9923          
##95% CI : (0.9901, 0.9941)
#No Information Rate : 0.8584          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.9675          
#Mcnemar's Test P-Value : 9.408e-15       

#Sensitivity : 0.9455          
#Specificity : 1.0000          
#Pos Pred Value : 1.0000          
#Neg Pred Value : 0.9911          
#Prevalence : 0.1416          
#Detection Rate : 0.1339          
#Detection Prevalence : 0.1339          
#Balanced Accuracy : 0.9727          

#'Positive' Class : Charged.Off 

#predict on validation 
#add 50th level to addr state as it currently on has 49
validation_default$addr_state <- factor(validation_default$addr_state, levels = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL",
                                                                            "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "NE", "NH",
                                                                            "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"))

#create a prediction                                                                                                                                                    
validation_test_svm <- validation_default
validation_test_svm$prediction <- predict(svm_model,validation_test_svm)
validation_test_svm$probability <- attr((predict(svm_model, validation_test_svm, probability = TRUE)), "probabilities")
table(validation_test_svm$prediction)

validation_svm_submission <- subset(validation_test_svm, select = c("member_id", "prediction", "probability"))
write.csv(validation_svm_submission,file="validation_svm.csv")
#Charged.Off  Fully.Paid 
#712        2037

######################################
#neural net model 

library(nnet)
library(NeuralNetTools)
library(caret)
###create a test and train set 
samp = sample(1:length(clean_just_default_no_date$loan_status), 100)

train = clean_just_default_no_date[-samp, ]
test = clean_just_default_no_date[samp, ]

nn_fit = nnet(loan_status ~ .-member_id, train, size=8)
plotnet(nn_fit, nid = F)

nnet_confusion =confusionMatrix(predict(nn_fit, test, type="class"), test$loan_status)

#nnet_confusion
#Prediction    Charged.Off Fully.Paid
#Charged.Off          15          0
#Fully.Paid            1         84

#Accuracy : 0.99            
#95% CI : (0.9455, 0.9997)
#No Information Rate : 0.84            
#P-Value [Acc > NIR] : 5.37e-07        

#Kappa : 0.9618          
#Mcnemar's Test P-Value : 1               

#Sensitivity : 0.9375          
#Specificity : 1.0000          
#Pos Pred Value : 1.0000          
#Neg Pred Value : 0.9882          
#Prevalence : 0.1600          
#Detection Rate : 0.1500          
#Detection Prevalence : 0.1500          
#Balanced Accuracy : 0.9688          

#'Positive' Class : Charged.Off  

#create a prediction for validation test 
validation_test_nnet <- validation_test
nnet_val=table(predict(nn_fit, validation_test_nnet, type="class"))
nnet_val
validation_test_nnet$probability <- predict(nn_fit, validation_test_nnet, type="raw")
validation_nnet_submission <- subset(validation_test_nnet, select = c("member_id", "probability"))
write.csv(validation_nnet_submission,file="validation_nnet.csv")
