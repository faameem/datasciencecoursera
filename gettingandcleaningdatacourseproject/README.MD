# Introduction
This README explains the solution to Coursera Assignment: Getting and Cleaning Data Course Project

* Please, read details of the assignment requirements [here](https://www.coursera.org/learn/data-cleaning/peer/FIZtT/getting-and-cleaning-data-course-project/submit).
* Script file run_analysis.R creates the required tidy data solution. Script details are described below. 
* File CODEBOOK.MD describes the variables in the tidy data solution.

### Steps For Running Script [run_analysis.R]:
* Get the data for the project: [here](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip).
* Un-zip the data in a local directory.
* Place script [run_analysis.R] in the same directory.
* Make sure that [dplyr] package is installed in your R IDE.
* Run the script [run_analysis.R].
* The script will create a file data [data-tidy.txt] in the same directory.

### Script [run_analysis.R] Details:
The script performs the following steps:
* Read file [features.txt] to get the names of the variables.
* Read files [X_train.txt,subject_train.txt,y_train.txt] to get training data.
* Read files [X_test.txt,subject_test.txt,y_test.txt] to get test data.
* Merge all training data using bind_cols()
* Merge all test data using bind_cols()
* Merge the training and test data using bind_rows()
* Apply variable names to the merged data using features data.
* Remove the columns with duplicate names.
* Extract the mean and standard deviation columns
* Apply descriptive activity names to the activities in the data set.
* Remove non-alphanumeric characters from the variable names [(,),-]
* Make the variable names lower case.
* From the data set, create a second, independent tidy data set with the average of each variable for each activity and each subject using [dplyr] package group_by and summarise_each functions.
* Export the final tidy data set to a file [data-tidy.txt] in the same directory.
