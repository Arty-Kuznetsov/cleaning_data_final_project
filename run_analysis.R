######## Course project #########

library(data.table)
library(dplyr)

## Set Working Directory
setwd("C:/Users/artyg/Documents/Rprojects/GETTING and CLEANING data/UCI HAR Dataset")

## Import test and training files into data frames
testSubject <- read.table("test/subject_test.txt", header = FALSE)
testX <- read.table("test/X_test.txt", header = FALSE)
testY <- read.table("test/Y_test.txt", header = FALSE)
trainSubject <- read.table("train/subject_train.txt", header = FALSE)
trainX <- read.table("train/X_train.txt", header = FALSE)
trainY <- read.table("train/Y_train.txt", header = FALSE)

## Import features file for column names 
features <- read.table("features.txt", header = FALSE)

## Set column names for X data frame
colnames(testX) <- features$V2
colnames(trainX) <- features$V2

## Set column names for Y and Subject data frames
colnames(testSubject) <- "SubjectId"
colnames(trainSubject) <- "SubjectId"
colnames(testY) <- "actId"
colnames(trainY) <- "actId"


########## 1 ############

## Merge the training and the test sets to create one data set:

## Combining test data frames
test <- bind_cols(testSubject , testX , testY)

## Do the same with train data frames
train <- bind_cols(trainSubject , trainX , trainY)

## Bind rows of test and train data sets together
All_data <- rbind(test , train)



########## 2 ############

## Extract only the measurements on the mean
# and standard deviation for each measurement

## For convenience, delete unnecessary characters from column names
New_names <- make.names(names = colnames(All_data) , unique = T , allow_ = T)
colnames(All_data) <- New_names
data.frame(colnames(All_data))

## Select standard deviation and mean columns
mean_stdiv <- bind_cols(select(All_data, contains("SubjectId"))
                        ,select(All_data, contains("actId"))
                        ,select(All_data, contains("mean..."))
                        ,select(All_data, contains("std...")))


########## 3 ############

## Use descriptive activity names to 
#name the activities in the data set

## Import activity labels
act_labels <- read.table("activity_labels.txt", header = FALSE)
names(act_labels) <- c("actId", "Activity_Labels")
All_data_act <- merge(mean_stdiv,act_labels,by="actId" ,all = TRUE)

## For convenience, change the order of columns
act_labels_temp <- select(All_data_act , "Activity_Labels")
tempdata <- select(All_data_act , -"actId" , -"Activity_Labels")
totaldata <- bind_cols(act_labels_temp , tempdata)

########## 4 ############

## Appropriately labels the data set 
#with descriptive variable names

header <- gsub("tBody","Time_Body_",names(totaldata))
header <- gsub("tGravity","Time_Gravity_",header)
header <- gsub("fBody","Frequency_Body_",header)
header <- gsub(".mean...","_Mean_",header)
header <- gsub(".std...","_StandardDeviation_",header)
header <- gsub("Acc","Acceleration",header)
names(totaldata) <- header

########## 5 ############

## From the data set in step 4, create a second,
#independent tidy data set with the average of 
#each variable for each activity and each subject

## Create a list of columns to average minus the Activity_Label and SubjectId columns
list <- header[-c(1 , 2)]

## Aggregate the totaldata set by Activity_Label and SubjectId
#and generate the mean for each of the measured variables

tidydata <- aggregate(totaldata[,c(list)]
            , by = list(Activity_Labels  = totaldata$Activity_Labels
            , Subjectidentifier = totaldata$SubjectId ), FUN = mean)

## Export the final tidy data set
write.table(tidydata , file = "tidydata.txt" , row.names = F)
