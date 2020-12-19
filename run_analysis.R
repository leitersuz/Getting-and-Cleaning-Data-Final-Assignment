### Getting and Cleaning Data Course Project
### Creating a tidy data set using the
### Human Activity Recognition Using Smartphones Dataset
## Author: S. Leiter
## Date: December 2020

# The output of this script is a tidy data set displaying the 
# the average of each feature measured in the experiment for each of
# six activities and each of 30 volunteer subjects.

## Install necessary packages
library(dplyr)
library(reshape2)

## Create data directory
if(!file.exists("data")){
    dir.create("data")
}

## Retrieve data using URL
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile="./data/galaxy.zip", mode="wb")

## Unzip data
unzip("./data/galaxy.zip", junkpaths = TRUE, exdir = "./data")

## Read in data files
# Read in X_test (test dataset)
X_test <- read.table("./data/X_test.txt")

# Read in y_test (activity values for test dataset)
y_test <- read.table("./data/y_test.txt")

# Read in subject_test (subject IDs for test dataset)
subject_test <- read.table("./data/subject_test.txt")

# Read in X_train (training dataset)
X_train <- read.table("./data/X_train.txt")

# Read in y_train (activity values for training dataset)
y_train <- read.table("./data/y_train.txt")

# Read in subject_train (subject IDs for training dataset) 
subject_train <- read.table("./data/subject_train.txt")

# Read in activity_labels (labels for the activity values in y_train and y_test
activity_labels <- read.table("./data/activity_labels.txt")

# Read in the features (labels for the columns in X_train and X_test)
features <- read.table("./data/features.txt")

## Rename the variables in each data file
# This will help identify columns when data files are compiled in dataframe
y_test <- rename(y_test, activityType = V1)
y_train <- rename(y_train, activityType = V1)
subject_test <- rename(subject_test, subjectID = V1)
subject_train <- rename(subject_train, subjectID = V1)

# Create vector of column names from the features dataframe
feature_names <- pull(features, V2)
# Use vector to rename the columns of X_train and X_test
colnames(X_train) <- feature_names
colnames(X_test) <- feature_names

## Step 1: Merge the training and the test sets to create one data set
# Compile the training data files
training <- cbind(subject_train, y_train, X_train)

# Compile the test data files
test <- cbind(subject_test, y_test, X_test)

# Compile training and test dataframes into one dataframe
fullDF <- rbind(test, training)

# Check for missing data
sum(is.na(fullDF))  # There are no missing values in the dataframe

## Step 2: Extract only the measurements on the mean and 
## standard deviation for each measurement.

# Use regular expressions to identify the names of the columns containing 
# means and standard deviations using the vector of feature names
colsToExtract <- grep(("mean()|std()"), feature_names, value=TRUE)

# exclude meanFrequency columns
colsToExtract <- grep("meanFreq", colsToExtract, value=TRUE, invert = TRUE)

# Add the names of the subjectID and activityType columns to the vector
# of column names to be extracted from the full dataframe
column_names <- c("subjectID", "activityType")
column_names <- c(column_names, colsToExtract)

# Extract the columns from the full dataframe
extract <- select(fullDF, all_of(column_names))

## Step 3: Use the descriptive activity names to name the activities 
## in the data set
extract$activityType <- factor(extract$activityType,
                               levels = c(1,2,3,4,5,6),
                               labels = c("Walking", "Walking_Upstairs", "Walking_Downstairs",
                                          "Sitting", "Standing", "Laying")) 

## Step 4: Label the data set with descriptive variable names
# Remove dashes and parentheses
names(extract) <- gsub("()","",names(extract), fixed=TRUE)
names(extract) <- gsub("-","",names(extract), fixed=TRUE)

# Use limited capitalization of letters to distinguish 
# individual words within each variable name
names(extract) <- gsub("mean","Mean",names(extract), fixed=TRUE)
names(extract) <- gsub("std","StDev",names(extract), fixed=TRUE)

# Spell out abbreviations for easier understanding
names(extract) <- gsub("Acc","Acceleration",names(extract), fixed=TRUE)
names(extract) <- gsub("Gyro","Gyroscope",names(extract), fixed=TRUE)
names(extract) <- gsub("Mag","Magnitude",names(extract), fixed=TRUE)
names(extract) <- sub("^t","time",names(extract))
names(extract) <- sub("^f","FFT",names(extract))

## Step 5: Create a second, independent tidy data set with
## the average of each variable for each activity and each subject.

# Create a new data set, tidydata grouped by activity and subject
# Summarize the data set by the mean

tidydata <- extract %>% 
    group_by(activityType, subjectID) %>%
    summarize_all(mean)

# The code above creates a tidy data set in wide form in which
# each feature is treated as a separate variable in its own column,
# and each row is an observation with mean values across all features
# for one subject and one activity.

## Alternate tidy data set format:
# Alternately, the data could be melted into a tidy data set
# in tall form in which all features are grouped into a single
# variable and each row is an observation with one subject, 
# one activity, and mean value of one feature.
# This code is provided below as an alternative, though
# it will not be used as the final output of this script.

tidydata2 <- melt(tidydata, id.vars=c("subjectID","activityType"))

## Final step: Output the data
write.table(tidydata, file = "./data/tidydata.txt", row.name=FALSE)

## These data can be read back into R using the code below,
# which is currently commented out.
# read.table("./data/tidydata.txt")
