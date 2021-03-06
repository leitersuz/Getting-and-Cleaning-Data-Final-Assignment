Getting and Cleaning Data Course: Final Project Repository
================
December 2020

This repository contains my assignment submission for the Getting and
Cleaning Data Course offered on Coursera from Johns Hopkins University.
The repository contents are outlined below, and I provide a rationale
for my approach to the assignment. The final course project involved
using raw data from the Human Activity Recognition Using Smartphones
Dataset to create a tidy data set.

## Repository Contents

This repository contains three files.

| File            |                                                               Description                                                               |
| :-------------- | :-------------------------------------------------------------------------------------------------------------------------------------: |
| run\_analysis.R | R script containing code that takes raw data from the Human Activity Recognition Using Smartphones Dataset and outputs a tidy data set. |
| CodeBook.md     |                      Markdown file containing a codebook with descriptions of each variable in the tidy data set.                       |
| ReadMe.md       |              Markdown file explaining the contents of the repository and rationale for my approach to the course project.               |

## Project Approach and Rationale

The script inputs raw data from the Human Activity Recognition Using
Smartphones Dataset. The raw data files are available here:
<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>

The data contain descriptive statistics for signals (referred to below
as features), which measured via Samsung smartphones while 30 volunteer
research subjects completed six physical activities: walking, walking
upstairs, walking downstairs, sitting, standing, and lying down. The
features were measured using accelerometers and gyroscopes within the
subjects’ smartphones. The data from the features in the raw data were
preprocessed and normalized and bounded within \[-1,1\].

The R script in this repository takes the raw data from above and
creates a tidy data set.

### Compiling the raw data files and applying descriptive labels

The script retrieves the raw data and reads the data files required for
the project into R. The script then combines the data files into one
data set and applies names the columns in the data set.

The script applies descriptive names to the values within the activity
variable in the data set, which correspond with the six activities
completed by research subjects.

The script also labels the columns in the data set with descriptive
variable names. Because the measurements from the experiments are
complex, my approach to variable naming tried to balance descriptive
information with best practices for variable naming. For example,
characters such as dashes and parentheses have been removed, and
abbreviations in the variable names have been spelled out for to make
them easier to understand. However, the variable names do contain some
capitalization. Because the variable names contain multiple words
necessary to describe the data they contain, I used capitalization
within variable names to help the user distinguish the individual words
within each variable name.

### Extracting measurements only of the mean and standard deviation

The script extracts only measurements of the mean and standard deviation
for each feature. The data set includes a number of variables involving
mean values, including mean, mean frequency (the weighted average of
frequency components of a feature), and a set of five vectors containing
average signals from specific signal window samples, which were used to
calculate the angle between vectors. The assignment directions
instructed students to “extract only the measurements on the mean and
standard deviation for each measurement.” I took this to mean that
students should extract only those features in the data set which
contained the mean and standard deviation for the signals; that is, I
only extracted those features labeled with mean() or std(), and I
excluded the features containing mean frequencies and mean signal with
the angle variable.

### Creating a tidy data set

Last, the script uses the data set compiled in the prior steps is used
to create an independent tidy data set. This tidy data set contains the
average of each variable for each activity and each subject. This means
that each row in the data set contains a unique subject-activity pair.
Each column represents the average value of a feature measured in the
experiment for that subject-activity pair, plus a column for activity
type and a column for subject ID. In this data set each feature is
treated as its own variable and is a tidy data set with a wide format.

The script also contains code to generate a tidy data set in a tall
format. In the tall format, there are four columns: activity type,
subject ID, variable, and value. In this format, all features are
grouped into a single variable column and the average value for each
feature is displayed in the value column. In this format, each row
contains an observation for a unique subject-activity-feature pair.

Both data sets are tidy according to the tidy data principles laid out
by Hadley Wickham in his 2014 paper entitled “Tidy Data” in the Journal
of Statistical Software. For this assignment the wide format data set is
used as the final output of the script.
