
Coursera Getting and Cleaning Data project


##### Source data
The source data used for the project is provided by UC Irvine Center for Machine Learning and Intelligent Systems from their Machine Learning Repository and is a publicly available dataset used to study Human Activity Recoginition

[Human Activity Recognition Using Smartphones Data Set]
(http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

The data was provided in a zip file by Coursera available on the following link 

[UCI HAR Dataset]
(https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

More specific details of the files in the zip together with the transformations performed are outlined in the following markdown file also provided in this repository
[Code Book]
(CodeBook.md)

##### Project requirements
The requirements for the project assigment were as follows

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Detailed information regarding how each step was carried out are covered in the [Code Book]
(CodeBook.md)

##### Script file
The script file to run to transform the data is called `run_analysis.R` and is located in the same folder as this readme.md file. 

##### R package requirements
This project work in the `run_analysis.R` mainly use R base packages with the exception of `dplyr` which needs to be installed from CRAN and enabled using the commands `library(dplyr)` or `require(dplyr)`. The latter is used in the script


##### Steps carried out for project analysis in detail 
The following steps are performed after the data files listed above are imported into R data frames using the `read.table()` function in R. 

##### Step 1 : Merging the training and the test sets to create one data set.

First the columns of the test and train datasets are linked together using the function `dplyr::bind_cols`
Second the rows of the test and train datasets are appended to form one dataframe using `rbind` 

##### Step 2 : Extract only the measurements on the mean and standard deviation for each measurement.
First illegal characters in column names are removed using the `make.name` function then the relevant columns are selected using `dplyr::select` to create a new dataframe


##### Step 3 : Use descriptive activity names to name the activities in the data set

The activity labels are imported to R using `read.table` and then merged into the dataset using the `merge` function.

Columns are then reordered so that the labels are not the last column.


##### Step 4 : Appropriately labels the data set with descriptive variable names.

Labels of the variable are renamed using the `gsub` function so they are in title case with underscores instead of dots. 

##### Step 5 : From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

A list is created of the variable header names required, which are then used by the `aggregate` function to calculate the mean of each column.

Finally the text file `tidydataset.txt` is written out to the file system using the function `write.table`


##### Running the script 
Here are the following steps if you wish to run the script

1. Clone the repo from Github.

`git clone git@github.com:johnost/data-cleaning-project.git`
 
2. Download the dataset `https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)
` 
3. Unzip inside the downloaded file inside the cloned project root level directory. Please do not rename the folder from `UCI HAR Dataset`

4. Open `run_analysis.R` in R Studio IDE

5. Update the `setwd()` function to match your environment and the location of the git repo clone.

6. Run line by line or Run All 

6. An export file called `tidydataset.txt` is placed in the root folder of the cloned project.

##### Importing the final tidy data file for verification

To import the final tidy data txt file please use the following command in R 

``` importtidy <- read.table(file="tidydataset.txt", header = TRUE) ```
