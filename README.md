#Coursera: Getting and Cleaning Data Course Project

##Overview

This project contains my implementation of the Coursera "[Getting and Cleansing Data](https://www.coursera.org/course/getdata)" course project.

Please refer to the file CodeBook.md in this repository for more detailed information about the data cleansing process and the variables produced.

## Pre-requisites

Before the `run_analysis` function can be called, it is expected that the pre-requisite data files exist in the same directory as the script and this is set to the current working directory.

Please follow the following steps:

1. Download the zipped data file from: [UCI HAR Dataset.zip](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)
2. Unzip the file into the working directory containing `run_analysis.R`
3. This should create a new sub-directory called _UCI HAR Dataset_

## Code
All code is supplied within the `run_analysis.R` file. This contains a function called `run_analysis()` which can be called to load, tidy and output the final cleansed and summarised result set.

E.g. From the working directory containing the R file:

```
source('./run_analysis.R')
tidy.data <- run_analysis()
View(tidy.data)
```
