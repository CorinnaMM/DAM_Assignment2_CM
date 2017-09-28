###########################################################

###            DATA ALGOITHMS AND MEANING
###           ASSESSMENT 2: Classification 
###              LOAN DEFAULT CHALLENGE

###              Corinna Maher Mittmann
###                    SEP 2017

###   DOCUMENT NAME: DAM_modelling_corinna
###########################################################
library (rpart)
library(rpart,plot)
library(mlbench)
library(caret)
library(readr)
library(tidyverse)
library(lubridate)

data_file_path <- "/Users/corinnamm/Documents/GitHub/DAM_Assignment2_CM/02_Working_data_folder/clean_justified_data_25var.csv"

# Load DATA "modellingdata1.csv"
data <- read_csv(paste0(data_file_path,"clean_justified_data_25var.csv"),trim_ws = T,col_names = TRUE) #,
                # col_types = cols(member_id = col_factor(),
                #                  loan_amnt = col_double(),
                #                  funded_amnt = col_double(),
                #                  term = col_factor(),
                #                  int_rate = col_double(),
                #                  grade = col_factor(),
                #                  emp_length = col_factor(),         
                #                  home_ownership = col_factor(),
                #                  annual_inc = col_double(),
                #                  verification_status = col_factor(),
                #                  loan_status = col_factor(),
                #                  purpose = col_factor(),
                #                  addr_state = col_factor(),          
                #                  dti = col_double(),
                #                  delinq_2yrs = col_double(),
                #                  earliest_cr_line = col_date(format = "ymd"),
                #                  inq_last_6mths = col_double(),
                #                  open_acc = col_double(),
                #                  pub_rec = col_double(),
                #                  revol_bal = col_double(),
                #                  total_acc = col_double(),
                #                  total_pymnt = col_double(),
                #                  total_rec_int = col_double(),
                #                  total_rec_late_fee = col_double(),   
                #                  last_pymnt_amnt = col_double(),    
                #                  pub_rec_bankruptcies = col_factor()))

problems(data)

##
# Renove ID
row.names(data) <- data$member_id
data <- select(data, -member_id)

str(data)
data$loan_status <- as.factor(data$loan_status)

data<- data %>% mutate(                 loan_amnt = as.numeric(loan_amnt),
                                        funded_amnt = as.numeric(funded_amnt),
                                        term = as.factor(term),
                                        int_rate = as.numeric(int_rate),
                                        grade = as.factor(grade),
                                        emp_length = as.factor(emp_length),
                                        home_ownership = as.factor(home_ownership),
                                        annual_inc = as.numeric(annual_inc),
                                        verification_status = as.factor(verification_status),
                                        loan_status = as.factor(loan_status),
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

str(data)

##
###############
## SUB SAMPLING FOR UNBALANCED CLASS
#to improve the model subsampling is also performed on an alternate dataset
###############
## Partitioning to train the glmnet model
#######################################
set.seed(42)
train = createDataPartition(y = data$loan_status, p = 0.7, list = F)
# partition default data
training = data[train, ]
testing = data[-train, ]


table(testing$loan_status)
table(training$loan_status)

## upsampled dataframe
# imbal_training$ID <- rownames(imbal_training)
set.seed(42)
x <- select(training, -loan_status)
set.seed(42)
up_training <- upSample(x = x,
                        y = training$loan_status, list = F, yname = "loan_status")
set.seed(42)
down_training <- downSample(x = x,
                            y = training$loan_status, list = F, yname = "loan_status")

# Even class balance will present problems for interpretability
table(up_training$loan_status)
head(up_training$x)
class(up_training)
str(up_training)
backup_up_train_up <- up_training
backup_up_train_dn <- down_training
## TREE 
## RPART with PRUNE

rpart_model <- rpart(loan_status~.,data = up_training, method="class")

#plot tree (UNPRUNED - same as exercise 1)
rpart.plot::prp(rpart_model)
#Unpruned model (same as exercise 1)
rpart_predict <- predict(rpart_model,testing[,-10],type="class")
mean(rpart_predict==testing$loan_status)
#confusion matrix (same as exercise 1)
table(pred=rpart_predict,true=testing$loan_status)

# Prune
plotcp(rpart_model)
#find minimum
opt <- which.min(rpart_model$cptable[,"xerror"])
cp <- rpart_model$cptable[opt, "CP"]
pruned_model <- prune(rpart_model,cp)
#plot tree (PRUNED)
rpart.plot::prp(pruned_model)
#PRUNED model
rpart_pruned_predict <- predict(pruned_model,testing[,-10],type="class")
mean(rpart_pruned_predict==testing$loan_status)
#confusion matrix (same as exercise 1)
table(pred=rpart_pruned_predict,true=testing$loan_status)

## Bagged CART Model
# names(up_training) <- make.names(up_training)
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
str(upmod_inside)
upmod_inside$results
#confusion matrix
table(repurchase.rf$test$predicted,testing$Target)
predictions_upmod <- predict(upmod_inside$finalModel,testing[,-10],type="class")
probability_upmod <- predict(upmod_inside$finalModel,testing[,-10],type="prob")
# cbind(testing, predictions_upmod)
# cbind(testing,probability_upmod)
# test_validationfile <- testing
wow <- confusionMatrix(predictions_upmod,testing$loan_status)
wow
wow$byClass
typeof(predictions_upmod)
tesi<- testing

tesi$pred <- predictions_upmod
binary<- mutate(tesi, pred = ifelse(pred == "Charged.Off", 1,0), loan_status = ifelse(loan_status == "Charged.Off", 1,0)) 
testing_prediction = prediction(binary$pred, binary$loan_status)
data.test.auc <- ROCR::performance(testing_prediction, "auc")
auc = unlist(slot(data.test.auc, "y.values"))

ROCR::plot()

model_evaluation(up_training,testing, upmod_inside$finalModel, target = "loan_status")

impmod<- varImp(upmod_inside$finalModel)
plot(impmod)

## predict the validation set
new_data <- read_csv(paste0(working_file_path,"cleaned_validation.csv"))

## prep
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
loan_status_pred <- predict(upmod_inside$finalModel,new_data[,-1],type="class")
prob_upmod <- predict(upmod_inside$finalModel,new_data[,-1],type="prob")

new_pred <- cbind(new_data,prob_upmod)
names(new_pred)
new_pred <- select(new_pred, member_id, Charged.Off)
names(new_pred) <- c("member_id",   "probability")
write_csv(new_pred, paste0(data_file_path, "bag_cart_upsamp_5rptcv_50nbagg_submission.csv"))

# new_submission <- read.csv(paste0(Original_Data_Location,"validation.csv"))
# new_submission <- left_join(new_submission,new_pred, by = "member_id")
# write_csv(new_submission, paste0(data_file_path, "bag_cart_upsamp_5rptcv_50nbagg_submission.csv"))
# names(new_submission)
# head(new_submission$loan_status_pred)
# table(is.na(new_submission$loan_status_pred))
### Rpart Tuned for cost (cp, Cost) - rpart pruning measure: rpartScore (cp, split, prune)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           search = "random")

set.seed(42)
rda_fit <- train(loan_status ~ ., data = up_training, 
                 method = "rpart",
                 metric = "ROC",
                 tuneLength = 30,
                 trControl = fitControl)

## rpart2 - in caret to control tree depth

## RF
# mtry <- sqrt(ncol(x))
# grid <- expand.grid(.mtry=mtry, .ntree = c(700, 1000,2000))
# ctrl <- caret::trainControl(method = "oob", repeats = 3, number =10,
#                             classProbs = TRUE)
# set.seed(42)
# rf_tune <- train(loan_status ~., data = up_training, method = "rf",
#                  trControl = ctrl,
#                  metric = "ROC",
#                  tuneGrid = data.frame(mtry=1:5),
#                  pValue = .1,
#                  maxRuns = 11)
# 
## RF
## SVM