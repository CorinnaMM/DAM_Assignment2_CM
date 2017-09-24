# =========================================================================== #
#                                                                             #
# Date:  Wed 05 July 2017                                                     #
# File Name: Data_Summarisation_template.R                                    #
# Description: Corinna's data summarisation and integrity check               #
#              tool for pre-initialisation                                    #
#                                                                             #
# =========================================================================== #

# == Header ===================================================================

# Set Workspace
# setwd("C:/Users/Corinna/~")

## Required installations
# install.packages('tidyverse')
# install.packages('lubridate')
# install.packages('dplyr')
# install.packages('ggplot2')

# Load packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

default <- read.csv(paste0(Original_Data_Location,"training.csv"), header = T)
data_dict <- read.csv(paste0(Original_Data_Location,"data_dictionary.csv"), header = T)
use_case <- default
# == Read in the data =========================================================

# Read in data (note: use_case is the object name for the file)

########### INPUT START ############
# use_case <- read.csv()
# use_case <- tbl_df(use_case)
# input data name
#use_case <- <add dataframe name>
########### INPUT END ##############

# = Identify and fix problems with the import =================================
# There are some problems with the import
problem_points <- readr::problems(use_case)


#######################################################################
#                               Stage 1                               #
#                          Meta Data Summary                          #
#                                                                     #
#######################################################################

# Inspect
# glimpse(use_case)
str(use_case)
summary(use_case)

#   Number of Cases & What each row represents
#    1 what does 1 row mean/stand for? This needs to be clear to all internal stakeholders
#    2 If 1 row stands for different items, is the data given metric aggregatable?
nrow(use_case)
row_distinction(use_case) 
nrow(distinct(use_case)) #should be the same as the nrow 
# try to make a row that is distinct by experimenting with different column combinations 

#  Number of Columns and their type
column_name<- colnames(use_case)
column_name
length(column_name)
# ncol(use_case)

#  Meta Data Count of the classes of columns    
class_count <- table(sapply(use_case, class))
class_count

# Count of class types  
class_count %>%
  data.frame(class_count) %>%
  group_by(class_count) %>%
  count(class_count, sort = TRUE) %>%
  print()

# Date column identifiy
potential_data_dates <- data_dict[(grepl(c("month","date","year"),data_dict$Description) == T),]
morepotential_data_dates <- data_dict[(grepl("date",data_dict$LoanStatNew) == T),]


# What is the date range
unique(use_case$date)
daterange <- ymd(use_case$date) 
mindate <- min(daterange)
maxdate <- max(daterange)

# Visualise, Summarise Data Responses for Integrity Check
##### DATA QUALITY REVIEW  
# Date range
unique(use_case$date)
daterange <- ymd(use_case$date) 
mindate <- min(daterange)
maxdate <- max(daterange)

##======= FIND THE MONTHLY DATES DATA
##### INPUT RULES #### Need to change the 
## Step 1 At what level is the date aggregated? Convert the date to monthly with floordate
## Step 2 Summarise the data into a monthly table
## Step 3 plot

## Step 4 Convert the data now into yearly data
## Step 5 Summarise the data into a monthly table
## Step 6 plot

#aggregate(use_case$input_metric, by =list(use_case$date), FUN = sum)

#######################################################################
#                               Stage 1.1                             #
#                            Data Dictionary                          #
#                                                                     #
#######################################################################
#  Meta Data Create a CSV file for Data Dictionary  
#    Variable name
#    Variable class
#    Unique levels per column 
#    Examples
#==================================================
# Extract the class type from the data frame wrapper
mylist_classes <- sapply(use_case, class)

# convert the see_classes from a list and then a dataframe with class and variable 
mydf_classes <- as.data.frame(mylist_classes)
mydf_classes <- mutate(mydf_classes,variable = row.names(mydf_classes))
names(mydf_classes) <- c("class","LoanStatNew")

## the above method is an update to the below method
# mylist <- mylist[2,]
# see_classes <- as.data.frame(gather(mylist, "variable", "class"))

# Generate the list of levels
mylist_levels <- use_case %>%
  summarise_each(funs(n_distinct)) %>%
  tidyr::gather("LoanStatNew", "level")

# Combine the levels and column class df to one final summary
final_summary <- mylist_levels %>%
  left_join(mydf_classes, by = "LoanStatNew")

## merge to data dictionary

### Add example values if required
UC_sample <- use_case[sample(nrow(use_case),3),]

### Add specific examples for certain situations like !is.na
# sadd1 <- filter(usecase, !is.na(use_case$date)) ## IMPUT REQUIRED - PLEASE SPECIFY COLUMN
# sadd1 <- sadd1[sample(nrow(sadd1),3),]
# sadd2 <- filter(usecase, (use_case$date == "2017-10-30")) ## IMPUT REQUIRED - PLEASE SPECIFY COLUMN
# sadd2 <- sadd2[sample(nrow(sadd2),1),]
## combine samples
# UC_sample <- bind_rows(UC_sample, sadd1)
# UC_sample <- bind_rows(UC_sample, sadd2)

## make/transpose the columns into rows
uc_sample <- as.matrix(UC_sample)
uc_sample <- t(uc_sample)
uc_sample <- data.frame(uc_sample)
uc_sample <- mutate(uc_sample, variable = row.names(uc_sample)) # variable column is at the end of the table
k <- 1:(length(uc_sample)-1)
names(uc_sample) # check the variable column is at the end of the table or change the order of the next row
names(uc_sample) <- c(paste("example",k,sep="_"),"LoanStatNew") 
# valdate transpose
nrow(uc_sample) == ncol(UC_sample)

# combine examples to list
final_summary <- final_summary %>% left_join(uc_sample, by ="LoanStatNew")
data_dict <- left_join(data_dict,final_summary, by="LoanStatNew")
### Write a csv file for the data dictionary

# add columns for manual entry
# data_dictt <- mutate(final_summary, description = "-")
# data_dictt <- final_summary
write.csv(data_dict, paste0(working_file_path,"modified_dict.csv"), row.names = F)
#=====================================================
# columns_to_remove <- final_summary %>% filter(final_summary, level == 1) 
# columns_to_remove <- columns_to_remove[,1]
# remove_colnum <- 
# keep_cols <- default
# default <- select(default, paste0("-",columns_to_remove))
#######################################################################
#                                 Stage 2                             #
#                         Business Data Evaluations                   #
#                                                                     #
#######################################################################

# 3 Top 5 questions for the client (taken from stage 1) are answerable by the data

# 4 The current analytic solutions in use to inform on what analytics types/functionalities 
#     are currently important and what needs to be part of the reconciliation process

#######################################################################
#                       Notes on Data Treatment                       #
#######################################################################

#####  MISSINGS AND NA
for (i in 1:length(names(use_case))){
  print(i)
  if (sum(is.na(use_case[,i]) | use_case[,i] %in% c("", " ","  ","N/A")  ) !=0) {
    message(names(use_case)[i])
    message(summary(is.na(use_case[,i]) | use_case[,i] %in% c("", " ","  ","N/A") ))
  }
}

source(paste0(code_file_path,"99_general_functions_DAM_Corinna.R"))
check_for_missing(use_case)



##### SPECIAL CHARACTERS
### Step 7.2 Check and Clean Special Characters
### NOTE: metacharacters for R are * \/ % " ' : ;`` ^ {} [] ? . + $ | ()
### Resource: http://biostat.mc.vanderbilt.edu/wiki/pub/Main/SvetlanaEdenRFiles/regExprTalk.pdf

spchar <- c( "\\*","/","\'", '\"',",","\\\\")
paste0(spchar, collapse = "|")
test_string <- "/ ' , * \\ \""
test_string
gsub( paste0(spchar, collapse = "|")," " ,test_string)

  for (i in 1:length(names(data))){
    print(i)
    if (sum(grepl(paste0(spchar, collapse = "|"), use_case[,i])) !=0) {
      message(names(data[i]))
    }
  }
#######################################################################
#                       Notes on Data Inbound                         #
#######################################################################

# COLUMNS REQUIRE REMAPPING
# unique(use_case$factor)

# COLUMNS WITH OVERLAPPING INFORMATION
# unique(d$a)[unique(d$a) %in% unique(d$b)]
# table(d$f)[unique(d$f) %in% unique(d$b)]