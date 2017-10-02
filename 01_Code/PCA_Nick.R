library(dplyr)
library(plyr)
library(ggplot2)
library(caret)
library(pls)
library(xgboost)
library(parallel)
library(doParallel)


# set up cluster for parallel computing
cluster = makeCluster(detectCores() - 1)
registerDoParallel(cluster)

setwd("~/Documents/GitHub/DAM_Assignment2_CM/01_Code")

raw_df = read.csv("~/Documents/GitHub/DAM_Assignment2_CM/02_Working_data_folder/clean_data_v5.csv")

df = raw_df

#basic exploration
head(df, 10)
tail(df, 10)
dim(df)
str(df)
summary(df)

#frequency of target and non-target variable
count(df$loan_status)

#check for duplicated values
sum(duplicated(df)==TRUE)

pca_var <- c(2,3,4,7,25:28)

pcadf = df[,pca_var]

str(df)
str(pcadf)
pr_out = prcomp(df[,pca_var], scale = T)

names(pr_out)
pr_out$rotation
#biplot(pr_out)

#scree plot
pr_var = pr_out$sdev ^ 2
pve = pr_var/sum(pr_var)

plot(pve, type = "b", main ="Scree Plot", 
     ylab = "Proportion of Variance Explained", 
     xlab = "Principal Component")

plot(cumsum(pve), type = 'b', main = "Cumulative Variance Explained",
     xlab = "Number of Components",
     ylab = "Cumulative Proportion of Variance Explained")

#come back to this and revaluate groupings


#=====================================================
# XGBoost Model w/ k = 5 fold cv plus grid search
#=====================================================

set.seed(42)
train = createDataPartition(y = df$loan_status, p = 0.7, list = F)

# drop ID column
training = df[train,]
testing = df[-train,]

dim(training)
dim(testing)

xgb_control = trainControl(method = "cv",
                           number = 5,
                           classProbs = T, 
                           summaryFunction = twoClassSummary, 
                           allowParallel = TRUE
)

# XGB Hyperparams
# tuning params for XGB best practice https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/
xgb_hyperparams = expand.grid(nrounds = 100,
                              eta = 0.1,
                              max_depth = c(3,6),
                              gamma = 1, #default = 0 
                              colsample_bytree = c(0.5,0.7), 
                              min_child_weight = 2, 
                              subsample = 0.5  
)

# Consider scaling this back as model takes a while to train
#xgb_hyperparams = expand.grid(nrounds = 1000,
#                              eta = c(0.01, 0.001, 0.001),
#                              max_depth = c(2,4,6,8,10),
#                              gamma = c(0,1), #default = 0 
#                              colsample_bytree = c(0.6,0.8,1), 
#                              min_child_weight = 1, 
#                              subsample = c(0.5,0.75,1)  
#)

x_train <- model.matrix( ~ ., training[,-14])

xgb_fit = train(x = x_train, y = training$loan_status,
                method='xgbTree',
                trControl= xgb_control, 
                tuneGrid = xgb_hyperparams, 
                metric = 'ROC')


#Models using upsampling and downsampling
#https://topepo.github.io/caret/subsampling-for-class-imbalances.html 


xgb_fit$results
print(xgb_fit)

newx = model.matrix( ~ ., testing[,-14])

ridge_pred = predict(xgb_fit, newx, type = "raw")
confusionMatrix(data = ridge_pred, testing$loan_status, mode = "everything", positive="Charged.Off")

count(df$loan_status)

testing$probability = predict(xgb_fit, newx, type = "prob")[, 1]
training$probability = predict(xgb_fit, x_train, type = "prob")[, 1]


#===============================================================
# Ridge Model - Finished Running
#===============================================================

set.seed(42)
train = createDataPartition(y = df$loan_status, p = 0.7, list = F)

# drop ID column
training = df[train,]
testing = df[-train,]

str(training)
str(testing)
dim(training)

trainControl = trainControl(method = "cv",
                            number = 5,
                            classProbs = T, 
                            summaryFunction = twoClassSummary, 
                            allowParallel = TRUE
)


#gradient boosted hyperparams
ridge_hyperparams = expand.grid(alpha = 0, 
                                lambda = seq(0.0001, 1, length = 100))

x_train <- model.matrix( ~ ., training[,-14])

ridge_fit = train(x = x_train, y = training$loan_status,
                  method='glmnet',
                  trControl= trainControl, 
                  tuneGrid = ridge_hyperparams,
                  metric = "ROC" )

ridge_fit$results
print(ridge_fit)

newx = model.matrix( ~ ., testing[,-14])

ridge_pred = predict(ridge_fit, newx, type = "raw")
confusionMatrix(data = ridge_pred, testing$loan_status, mode = "everything", positive="Charged.Off")

ridge_fit

testing$probability = predict(ridge_fit, newx, type = "prob")[, 1]
training$probability = predict(ridge_fit, x_train, type = "prob")[, 1]

ridge_importance = varImp(ridge_fit)$importance
ridge_importance$variable = rownames(ridge_importance)
ridge_importance = ridge_importance[order(ridge_importance$Overall, decreasing = T), ]$variable

# partial dependence plots
par(mfrow = c(2,2))
for (var in ridge_importance[1:length(ridge_importance)]) {
  plot.glmnet(ridge_fit$finalModel, i.var = var, type = "response")
}


#===============================================================
# Lasso Model - Finished Running
#===============================================================

set.seed(42)
train = createDataPartition(y = df$loan_status, p = 0.7, list = F)

# drop ID column
training = df[train,]
testing = df[-train,]

trainControl = trainControl(method = "cv",
                            number = 5,
                            classProbs = T, 
                            summaryFunction = twoClassSummary, 
                            allowParallel = TRUE
)

#gradient boosted hyperparams
lasso_hyperparams = expand.grid(alpha = 1, 
                                lambda = seq(0.0001, 1, length = 100))

x_train <- model.matrix( ~ ., training[,-14])

lasso_fit = train(x = x_train, y = training$loan_status,
                  method='glmnet',
                  trControl= trainControl, 
                  tuneGrid = lasso_hyperparams,
                  metric = "ROC" )

print(lasso_fit)

newx = model.matrix( ~ ., testing[,-14])

lasso_pred = predict(lasso_fit, newx, type = "raw")
confusionMatrix(data = lasso_pred, testing$loan_status, mode = "everything", positive="Charged.Off")

testing$probability = predict(lasso_fit, newx, type = "prob")[, 1]
training$probability = predict(lasso_fit, x_train, type = "prob")[, 1]

#===============================================================
# Part 2 - Tree based classification model - basic decision tree
#===============================================================

#===============================================================
# Gradient Boosted Model - TIMEOUT
#===============================================================

set.seed(42)
train = createDataPartition(y = df$Target, p = 0.7, list = F)

# drop ID column
training = df[train,]
testing = df[-train,]

trainControl = trainControl(method = "cv",
                            number = 5,
                            classProbs = T, 
                            summaryFunction = twoClassSummary, 
                            allowParallel = TRUE
)

#gradient boosted hyperparams
gb_hyperparams = expand.grid(interaction.depth = c(3,5), 
                             n.trees = c(200, 300),
                             shrinkage = 0.05,
                             n.minobsinnode = c(10,20))

x_train <- model.matrix( ~ ., training[,-14])

gbm_fit = train(x = x_train, y = training$loan_status,
                method = "gbm",
                trControl = trainControl,
                tuneGrid = gb_hyperparams,
                metric = "ROC")

gbm_fit$results
print(gbm_fit)

newx = model.matrix( ~ ., testing[,-14])

gbm_pred = predict(gbm_fit, newx, type = "raw")
confusionMatrix(gbm_pred, testing$loan_status, mode = "everything", positive="Charged.Off")


testing$probability = predict(gbm_fit, testing, type = "prob")[, 1]
training$probability = predict(gbm_fit, training, type = "prob")[, 1]

gbm_importance = varImp(gbm_fit)$importance
gbm_importance$variable = rownames(gbm_importance)
gbm_importance = gbm_importance[order(gbm_importance$Overall, decreasing = T), ]$variable
gbm_importance
# partial dependence plots
par(mfrow = c(2,2))
for (var in gbm_importance[1:length(gbm_importance)]) {
  plot.gbm(gbm_fit$finalModel, i.var = var, type = "response")
}

#===============================================================
#Random Forest Model - TIMEOUT
#===============================================================

df$Target = make.names(df$Target)
set.seed(42)
train = createDataPartition(y = df$Target, p = 0.7, list = F)

# drop ID column
training = df[train,]
testing = df[-train,]

trainControl = trainControl(method = "cv",
                            number = 5,
                            classProbs = T, 
                            summaryFunction = twoClassSummary, 
                            allowParallel = TRUE
)

#random forrest hyperparams
rf_hyperparams = expand.grid(mtry = c(4,5,6))

# need to fix this 
set.seed(42)
x_train <- model.matrix( ~ ., training[,-14])

rf_fit = train(x = x_train, y = training$loan_status,
               method = "rf",
               trControl = trainControl,
               tuneGrid = rf_hyperparams,
               metric = "ROC")

rf_fit$results
print(rf_fit)
newx = model.matrix( ~ ., testing[,-14])

rf_pred = predict(rf_fit, newx, type = "raw")
confusionMatrix(rf_pred, testing$loan_status, mode = "everything", positive="Charged.Off")

testing$probability = predict(rf_fit, testing, type = "prob")[, 1]
training$probability = predict(rf_fit, training, type = "prob")[, 1]

rf_importance = varImp(rf_fit)$importance
#rf_importance$variable = rownames(rf_importance)
#rf_importance = rf_importance[order(rf_importance$Overall, decreasing = T), ]$variable
rf_importance

# partial dependence plots
par(mfrow = c(2,2))
counter = 0
for (i in seq_along(rf_importance)[1:14]) {
  counter = counter + 1
  partialPlot(x = rf_fit$finalModel, pred.data = training, x.var = rf_importance[i],
              main = paste("Partial Dependence on", rf_importance[i]),
              xlab = rf_importance[i], col = "red",
              which.class = "X1")
}

#validation data set
#validation = read.csv("repurchase_validation.csv")
#validation$validation_pred = predict(rf_fit, validation, type = "raw")
#validation$probability = predict(rf_fit, validation, type = "prob")[, 1]
#write.csv(validation, "validation.csv")

#count(validation$validation_pred)

