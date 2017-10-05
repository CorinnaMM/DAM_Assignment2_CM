
###############################################################################
## Session 1 Exercise 2
#  Logistic Regression
###############################################################################

# This exercise involves training a logistic regression model
# We will us the lending club challenge data set from the ISLR package - Orange Juice sales
# We will also use the caret package to make the process easier

install.packages("caret")
library(caret)

# Let's start by summarising our data
"/Users/corinnamm/Documents/GitHub/DAM_Assignment2_CM/03_DAM_Spring2017_Assignment_2/v2_clean_justified_data_allvar"
str(data)

data <- read_csv("/Users/corinnamm/Documents/GitHub/DAM_Assignment2_CM/03_DAM_Spring2017_Assignment_2/v2_clean_justified_data_allvar")

# paste0(Original_Data_Location,"v2_clean_justified_data_allvar.csv"))

# data = as.tibble(data)
# names(data)
# temp <- data %>% mutate(tempnew = if_else(loan_amnt == funded_amnt, 0,1)) %>% summarise(sum(tempnew)) %>% View()
# qplot(x= data$loan_amnt, y= (data$funded_amnt + data$funded_amnt_inv), geom ="point")
# str(data)
# data <- data %>% mutate(funded_amnt_inv_prop = (funded_amnt_inv/loan_amnt),
                        # funded_amnt_prop - (funded_amnt/loan_amount),
                        # installment_prop = (installment/loan_amnt),
                        # total_p = total_rec_prncp+total_rec_int+total_rec_late_fee)
# data <- data %>% mutate(check = if_else(total_p == total_pymnt, 0,1)) %>% sumarise ()
# int_data <- select(data, -term, -grade, -sub_grade, -home_ownership, 
#                    -verification_status, -purpose, -addr_state,
#                    -pub_rec_bankruptcies, -earliest_cr_line) # loan_status
# int_data <- select(int_data, -emp_length)
# 
# # corrplot::corrplot(int_data, method = "shade")
# str(int_data)
# default_corplot <- int_data[sapply(int_data, function(x) is_integer(x) || is_double(x))]                                               
# par(mfrow = c(1,1))
# corrplot::corrplot(cor(default_corplot), method = "shade") 

# findco <- findCorrelation(int_data[,-1])
# ??findCorrelation
# findco
###########################
# Paritioning
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
glmDF <- mutate(data, loan_status = if_else(loan_status == "Charged.Off",1,0))
data.glm = glm(formula = loan_status ~ loan_amnt + term + int_rate + annual_inc + dti + delinq_2yrs + earliest_cr_line + inq_last_6mths + open_acc + pub_rec + revol_bal + total_acc          
               + total_pymnt  + total_rec_late_fee + last_pymnt_amnt,
             data = down_training,
             family = "binomial")
summary(data.glm)

# AIC ~ 11492
# AIC ~11538
data.glm = glm(formula = loan_status ~ loan_amnt + term + int_rate + total_acc          
               + total_pymnt  + total_rec_late_fee + last_pymnt_amnt,
               data = down_training,
               family = "binomial")
summary(data.glm)
# AIC ~11538

glmDF <- mutate(data, loan_status = if_else(loan_status == "Charged.Off",1,0))
data.glm = glm(formula = loan_status ~ loan_amnt + purpose + funded_amnt + term + int_rate + annual_inc + dti + addr_state +installment + 
                 earliest_cr_line + inq_last_6mths + open_acc + pub_rec + revol_bal + total_acc + pub_rec_bankruptcies         
               + total_pymnt  + total_rec_late_fee + last_pymnt_amnt + home_ownership +emp_length +verification_status +sub_grade,
               data = glmDF,
               family = "binomial")
# AIC ~ 6908.6
glmDF <- mutate(down_training, loan_status = if_else(loan_status == "Charged.Off",1,0))
data.glm = glm(formula = loan_status ~ loan_amnt + purpose + funded_amnt + term + int_rate + annual_inc + dti +installment + 
                 inq_last_6mths + open_acc + pub_rec + revol_bal + total_acc + pub_rec_bankruptcies         
               + total_pymnt  + total_rec_late_fee + last_pymnt_amnt + home_ownership +emp_length +verification_status +sub_grade,
               data = glmDF,
               family = "binomial")
summary(data.glm)
# AIC ~ 6876.6

data.glm = glm(formula = loan_status ~ loan_amnt + purpose + funded_amnt + term + int_rate + annual_inc + dti + installment + 
               total_pymnt  + total_rec_late_fee + last_pymnt_amnt + emp_length + sub_grade,
               data = glmDF,
               family = "binomial")
summary(data.glm)
# AIC ~ 6878.6

data.glm2 = glm(formula = loan_status ~ loan_amnt + purpose + funded_amnt + term + int_rate + annual_inc + dti +installment + 
                 earliest_cr_line + inq_last_6mths + open_acc + pub_rec + revol_bal + total_acc + pub_rec_bankruptcies         
               + total_pymnt  + total_rec_late_fee + last_pymnt_amnt +emp_length + verification_status + sub_grade,
               data = glmDF,
               family = "binomial")
summary(data.glm2)
# AIC ~ 6871.1
data.glm3 = glm(formula = loan_status ~ loan_amnt + purpose + funded_amnt + term + int_rate + annual_inc + dti + installment + 
                  earliest_cr_line + inq_last_6mths + open_acc + pub_rec + revol_bal + total_acc + pub_rec_bankruptcies         
                + total_pymnt  + total_rec_late_fee + last_pymnt_amnt +emp_length + sub_grade,
                data = glmDF,
                family = "binomial")
summary(data.glm3)
# AIC ~ 6868.6

# Let's stick with this last model

# we may have multi-collinearity present.  Use the 'pairs' plot function to check
# pairs(data[, c("StoreID","PriceCH","PriceMM","DiscMM","LoyalCH","PctDiscMM","PctDiscCH")])

# QUESTION - are any predictors collinear?

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

new_pred <- cbind(new_data,prob_upmod)
names(new_pred)
new_pred <- select(new_pred, member_id, Charged.Off)
names(new_pred) <- c("member_id",   "probability")
write_csv(new_pred, paste0(Original_Data_Location, "bag_cart_upsamp_5rptcv_50nbagg_submission2.csv"))


