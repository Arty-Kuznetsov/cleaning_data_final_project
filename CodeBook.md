
#####  Background for data collection

The experiments was carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

##### Project website
[Human Activity Recognition Using Smartphones Data Set]
(http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

##### Link to download source data

[UCI HAR Dataset]
(https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)
`https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip`

##### Original dataset files 



The following files are available for the train and test data. 

- `README.txt`

- `features_info.txt`: Shows information about the variables used on the feature vector.

- `features.txt`: List of all features.

- `activity_labels.txt`: Links the class labels with their activity name.

- `train/X_train.txt`: Training set.

- `train/y_train.txt`: Training labels.

- `test/X_test.txt`: Test set.

- `test/y_test.txt`: Test labels.


- `train/subject_train.txt`: Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

##### Feature variables in more detail

The features selected in the dataset come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ.  The acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) 

The body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Finally the magnitude are shown by the following (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 


##### Additional files provided by this Coursera project 


The following files have been created as part of the project

- `README.md`: The project readme file, which explains how to run the script in your own environment

- `CodeBook.md`: This file, which gives more information on the dataset and the steps performed on it. 

- `run_analysis.R`: R script used to merge, transform and provide additional analysis of the original UCI HAR Dataset

- `tidydataset.txt`: An exported file which gives average of each variable for each activity and each subject. The variables averaged are only for the means and standard deviations provided in the original dataset.


##### Final export description

The final export consists of a total 180 observations with 6 each for the 30 subject identifiers. There are 48 variables with there means for each activity and subject which are displayed in the appendix below




##### Appendix 

List of labels for the final tidy dataset are displayed below
`Activity_Labels`
`Subjectidentifier`
`Time_Body_Acceleration_Mean_X`
`Time_Body_Acceleration_Mean_Y`
`Time_Body_Acceleration_Mean_Z`
`Time_Gravity_Acceleration_Mean_X`
`Time_Gravity_Acceleration_Mean_Y`
`Time_Gravity_Acceleration_Mean_Z`
`Time_Body_AccelerationJerk_Mean_X`
`Time_Body_AccelerationJerk_Mean_Y`
`Time_Body_AccelerationJerk_Mean_Z`
`Time_Body_Gyro_Mean_X`
`Time_Body_Gyro_Mean_Y`
`Time_Body_Gyro_Mean_Z`
`Time_Body_GyroJerk_Mean_X`
`Time_Body_GyroJerk_Mean_Y`
`Time_Body_GyroJerk_Mean_Z`
`Frequency_Body_Acceleration_Mean_X`
`Frequency_Body_Acceleration_Mean_Y`
`Frequency_Body_Acceleration_Mean_Z`
`Frequency_Body_AccelerationJerk_Mean_X`
`Frequency_Body_AccelerationJerk_Mean_Y`
`Frequency_Body_AccelerationJerk_Mean_Z`
`Frequency_Body_Gyro_Mean_X`
`Frequency_Body_Gyro_Mean_Y`
`Frequency_Body_Gyro_Mean_Z`
`Time_Body_Acceleration_StandardDeviation_X`
`Time_Body_Acceleration_StandardDeviation_Y`
`Time_Body_Acceleration_StandardDeviation_Z`
`Time_Gravity_Acceleration_StandardDeviation_X`
`Time_Gravity_Acceleration_StandardDeviation_Y`
`Time_Gravity_Acceleration_StandardDeviation_Z`
`Time_Body_AccelerationJerk_StandardDeviation_X`
`Time_Body_AccelerationJerk_StandardDeviation_Y`
`Time_Body_AccelerationJerk_StandardDeviation_Z`
`Time_Body_Gyro_StandardDeviation_X`
`Time_Body_Gyro_StandardDeviation_Y`
`Time_Body_Gyro_StandardDeviation_Z`
`Time_Body_GyroJerk_StandardDeviation_X`
`Time_Body_GyroJerk_StandardDeviation_Y`
`Time_Body_GyroJerk_StandardDeviation_Z`
`Frequency_Body_Acceleration_StandardDeviation_X`
`Frequency_Body_Acceleration_StandardDeviation_Y`
`Frequency_Body_Acceleration_StandardDeviation_Z`
`Frequency_Body_AccelerationJerk_StandardDeviation_X`
`Frequency_Body_AccelerationJerk_StandardDeviation_Y`
`Frequency_Body_AccelerationJerk_StandardDeviation_Z`
`Frequency_Body_Gyro_StandardDeviation_X`
`Frequency_Body_Gyro_StandardDeviation_Y`
`Frequency_Body_Gyro_StandardDeviation_Z`
