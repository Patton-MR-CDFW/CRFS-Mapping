# script that reads in the i1, i2, i3, and i8 files produced by CDFW for each year from 2012
# saves the combined yearly files for each i table into the Data folder
# Michael Patton (michael.patton@wildlife.ca.gov)


#USER INPUT:
# copy path to where you downloaded the shared CRFS_Mapping folder between the ()
working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping\RCode\PC\Dat12toPresent\Data)"
setwd(working_directory)


library(readxl)
library(tidyverse)
library(openxlsx)

# manually moved each required PC raw yearly file for each i table into the rawfiles folder, this lists out the names of each file in this folder
files = list.files('rawfiles')

# different i tables to loop through
tables = c('i1_', 'i2_', 'i3_', 'i3d_', 'i8_', 'i8a_', 'i8b_', 'i8c_')

for (table in tables){
  i_files = c() # create empty list to contain the files filtered to the same i table
  # loops through every file in the raw data folder, if the file is the right i table (detected by having 'i1' or 'i2' in its name then it is saved to a new list of these files)
  for (i in 1:length(files)) {
    if (grepl(table , files[i])) {
      print(files[i])
      i_files <- c(i_files, files[i])
    }
  }
  
  # takes all file names of the same i table and reads it, saves each one to a new dataframe which is written to a new csv
  for (file in i_files) {
    x = read_excel(paste0('rawfiles/', file), guess_max = 21474836)
    if (exists('alldata')) {
      alldata = rbind(alldata, x)
    } else {
      alldata = x
    }
  }
  
  write.csv(alldata, paste0(table, 'data_12to24.csv'), row.names = FALSE, na = "")
  
  rm(alldata)
  
  
}






