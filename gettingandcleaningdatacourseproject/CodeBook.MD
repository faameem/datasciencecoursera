#Code Book
* The steps for running the script are detailed in README.MD file.
* This document describes the code inside run_analysis.R script file.

## Constants
* [rpwd] contains the present working directory path

## Read Data Files
* Read file [features.txt] into [features] to get the names of the columns.
* Read files [train/X_train.txt,train/subject_train.txt,train/y_train.txt] into [x.train,subject.train,y.train] to get training data.
* Read files [test/X_test.txt,test/subject_test.txt,test/y_test.txt] into [x.test,subject.test,y.test] to get test data.

## Merge Data
* Merge all training data using bind_cols() into [subject.y.x.train]
* Merge all test data using bind_cols() into [subject.y.x.test]
* Merge the training and test data using bind_rows() into [data.merged]
 
## Manipulate Data
* Apply column names to [data.merged] using [features].
* Remove [data.merged] columns with duplicate names and save into [data.merged.nonduplicate].
* Using [dplyr] chaining, from [data.merged.nonduplicate], extract the mean and standard deviation columns, then appply descriptive activity names using data in [activity_labels.txt], and save the data in [data] variable.
* Remove non-alphanumeric characters from the column names [(,),-] of [data] data frame.
* Make the column names lower case of [data] data frame.

## Output Data
* From [data] data frame, create a second, independent tidy data set with the average of each variable for each activity and each subject using [dplyr] package group_by and summarise_each functions. Store this in [data.tidy] data frame.
* Export the final tidy data set to a file [data-tidy.txt] in the home directory [rpwd].
* In the output file [data-tidy.txt], column [subject] refers to the participant id, column [activity] refers to activity description, and the rest of the column names refer to the average of mean/std deviation measurements, the details of which can be obtained from [Features.txt] file.
 
