###########################################################

###            DATA ALGOITHMS AND MEANING
###               Functions Collection
###              LOAN DEFAULT CHALLENGE

###              Corinna Maher Mittmann
###                    SEP 2017

###   DOCUMENT NAME: 99_functions_DAM
###########################################################


########################################################### Load functions for evaaluation
# Function to create the ROC chart, and the sensitivity / specificity charts


model_evaluation = function(training = training, testing = testing, 
                            model = gbm_fit,
                            target = "target") {
  library(ROCR)
  
  # create prediction object on the testing data
  testing_prediction = prediction(testing$probability, testing[, target])
  # create prediction object on the training data
  training_prediction = prediction(training$probability, training[, target])
  
  
  # Create model performance objects on test data set - uses ROCR
  test_tpr_fpr = performance(testing_prediction, "tpr","fpr")
  
  data.test.auc = performance(testing_prediction, "auc")
  data.test.lift = performance(testing_prediction, "lift", "rpp")
  # Area under the ROC curve
  auc = unlist(slot(data.test.auc, "y.values"))
  cat("\nAUC on test data is ", round(100 * auc, 2), "%", sep = "")
  # Create model performance objects on training data set - uses ROCR
  train_tpr_fpr <- performance(training_prediction, "tpr","fpr")
  
  
  # Plot the tpr and fpr gains chart ROC for both testing and training data
  plot(test_tpr_fpr, main = "Gains Chart ROC", type = "l", col = "red", lwd = 2, 
       xlim = c(0,1), ylim = c(0,1))
  plot(train_tpr_fpr, add = T, col = "blue", lwd = 2, lty = 2, xlim = c(0,1), ylim = c(0,1))
  legend("bottomright", legend = c("Training","Testing"), col = c("blue","red"), lty = 1, lwd = 2)
  abline(0,1, col = "darkgray")
  grid()
  
  # Sensitivity / Specificity charts
  
  test_sens_spec = performance(testing_prediction, "sens","spec")
  
  sens = performance(testing_prediction, "sens")
  spec = performance(testing_prediction, "spec")
  
  plot(sens, 
       main = "Sensitivity Specificity Chart", type = "l", col = "red", lwd = 2, 
       xlim = c(0,1), ylim = c(0,1), 
       ylab = "Values")
  axis(side = 1, at = seq(0, 1, 0.1))
  axis(side = 2, at = seq(0, 1, 0.1))
  plot(spec, add = T, col = "blue", lwd = 2, 
       xlim = c(0,1), ylim = c(0,1)
  )
  legend("bottomright", legend = c("Sensitivity","Specificity"), col = c("red", "blue"), lty = 1, lwd = 2)
  abline(h = seq(0, 1, 0.1), v = seq(0, 1, 0.1), col="gray", lty=3)
  
  
  # Probability Threshold calculation
  threshold.df = data.frame(cut = test_sens_spec@alpha.values[[1]], 
                            sens = test_sens_spec@x.values[[1]],
                            spec = test_sens_spec@y.values[[1]])
  threshold = threshold.df[which.max(threshold.df$sens + threshold.df$spec), "cut"]
  cat("\nProbability threshold is", threshold)
  cat("\n")
  return(threshold)
}

###########################################################
# Corinna's personal functions
#####  Check for missing function
check_for_missing <- function(data_input) {
  for (i in 1:length(names(data_input))){
    print(names(data_input)[i])
    
    if (sum(is.na(data_input[,i]) | data_input[,i] %in% c("", " ","  ","N/A","NULL","na","n.a","NA",NA,"missing","unknown","Unknown","Missing")) !=0) {
      message(paste0(names(data_input)[i]), " has missing values")
      number_of_missings <- table(data_input[,i] %in% c("", " ","  ","N/A","NULL","na","n.a","NA",NA,"missing","unknown","Unknown","Missing"))
      print(number_of_missings)
      message(paste0("The number of missing values is ",number_of_missings[[2]]))
      message(paste0("the % of missing values is ",round((number_of_missings[[2]])/(length(data_input[,i]))*100, digits = 2)," %"))
    } else {
      message("no missing")
    }
    
  }
}
##### define a row function
row_distinction <- function(data_input){
  for (i in 1:(length(names(data_input)))){
    print(i)
    print(n_distinct(data_input[,-i]))
    print(n_distinct(data_input))
    if (n_distinct(data_input[,-i]) == n_distinct(data_input)) {
      message(paste0("dataset is distinct without ", names(data_input)[i]))
    } else { message(paste0("dataset is NOT distinct without ",names(data_input)[i]))
    }
    
  }
}

# GEt the mean and standart deviations

descriptive_info = function(data_input, target_col) {
  q_temp <- NULL
  for (i in 1:(length(names(data_input)))){
    print(i)
    if (class(data_input[,i]) == "integer") {
      print(i)
      this_col <- names(data_input)[i]
      print(paste0("the column is ",this_col))
      temp = data_input[,i]
      mean = mean(temp)
      print(paste0("mean = ", mean))
      median = median(temp)
      print(paste0("median = ", median))
      standard_deviation = sd(temp)
      print(paste0("standard deviation = ", standard_deviation))
      minimum = min(temp)
      print(paste0("minimum value = ", minimum))
      maximum = max(temp)
      print(paste0("maximum value = ", maximum))
      number_of_rec = nrow(temp)
      
      # density plot
      x_temp <- names(data_input[,i])
      q_temp <- qplot(x = data_input[,i], data = data_input, geom = "density", fill = data_input[,target_col] , alpha = I(0.25)) +
        labs(title = paste0("Density plot for ",this_col),fill = names(paste0(data_input[,target_col]))) + xlab(paste0(this_col))
      plot(q_temp)
    } else {
      message("non-numeric")
      
    }
    
  } 
  return()
}
###########################################################