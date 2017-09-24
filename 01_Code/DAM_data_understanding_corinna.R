###########################################################

###            DATA ALGOITHMS AND MEANING
###           ASSESSMENT 2: Classification 
###              LOAN DEFAULT CHALLENGE

###              Corinna Maher Mittmann
###                    SEP 2017

###   DOCUMENT NAME: DAM_data_understanding_corinna
###########################################################
## Work-space management
Original_Data_Location <- "/mnt/mdsi/home/12750067/AT1B/"

working_file_path <- "/mnt/mdsi/home/12750067/AT1B/"

code_file_path <- "/mnt/mdsi/home/12750067/AT1B/"

wd_path <- "/mnt/mdsi/home/12750067/AT1B/"

getwd()
### 
default <- read.csv("/Users/corinnamm/Desktop/training.csv", header = T)
head(default)
str(default)
