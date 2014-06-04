# Code Book for Getting & Cleaning Data Course Project
-----------------------------------------

The file Meandata.txt contains summary data based on the [Human Activity Recognition Using Smartphones Data Set](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones). The actual zip file is retrieved from a [location](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones) that is specific to Coursera _Getting and Cleaning Data_ course. 

# Transformations

The code to create this data set is in `run_analysis.R`. To re-generate the
data set, run the following code

    source("run_analysis.R")

The original data set contains many data points on many variables (features) for two sets of data (a training set and a test set). All together it covers 30 test subjects. Transformation applied are:

1. Activities were converted from numerical ids to lower case names
1. Measurement names were sanitized so that they can be use as column names in R
1. Only mean measurements were preserved (standard deviation, min, max, and others were discarded)
1. The training and test sets were combined in a single set covering all 30 subjects
1. The means were then averaged per subject and activity.

The columns are:

* subject, activity, tBodyAcc-mean()-X, tBodyAcc-mean()-Y, tBodyAcc-mean()-Z, tBodyAcc-std()-X, tBodyAcc-std()-Y, tBodyAcc-std()-Z, tGravityAcc-mean()-X, tGravityAcc-mean()-Y, tGravityAcc-mean()-Z, tGravityAcc-std()-X, tGravityAcc-std()-Y, tGravityAcc-std()-Z, tBodyAccJerk-mean()-X, tBodyAccJerk-mean()-Y, tBodyAccJerk-mean()-Z, tBodyAccJerk-std()-X, tBodyAccJerk-std()-Y, tBodyAccJerk-std()-Z, tBodyGyro-mean()-X, tBodyGyro-mean()-Y, tBodyGyro-mean()-Z, tBodyGyro-std()-X, tBodyGyro-std()-Y, tBodyGyro-std()-Z, tBodyGyroJerk-mean()-X, tBodyGyroJerk-mean()-Y, tBodyGyroJerk-mean()-Z, tBodyGyroJerk-std()-X, tBodyGyroJerk-std()-Y, tBodyGyroJerk-std()-Z, tBodyAccMag-mean(), tBodyAccMag-std(), tGravityAccMag-mean(), tGravityAccMag-std(), tBodyAccJerkMag-mean(), tBodyAccJerkMag-std(), tBodyGyroMag-mean(), tBodyGyroMag-std(), tBodyGyroJerkMag-mean(), tBodyGyroJerkMag-std(), fBodyAcc-mean()-X, fBodyAcc-mean()-Y, fBodyAcc-mean()-Z, fBodyAcc-std()-X, fBodyAcc-std()-Y, fBodyAcc-std()-Z, fBodyAcc-meanFreq()-X, fBodyAcc-meanFreq()-Y, fBodyAcc-meanFreq()-Z, fBodyAccJerk-mean()-X, fBodyAccJerk-mean()-Y, fBodyAccJerk-mean()-Z, fBodyAccJerk-std()-X, fBodyAccJerk-std()-Y, fBodyAccJerk-std()-Z, fBodyAccJerk-meanFreq()-X, fBodyAccJerk-meanFreq()-Y, fBodyAccJerk-meanFreq()-Z, fBodyGyro-mean()-X, fBodyGyro-mean()-Y, fBodyGyro-mean()-Z, fBodyGyro-std()-X, fBodyGyro-std()-Y, fBodyGyro-std()-Z, fBodyGyro-meanFreq()-X, fBodyGyro-meanFreq()-Y, fBodyGyro-meanFreq()-Z, fBodyAccMag-mean(), fBodyAccMag-std(), fBodyAccMag-meanFreq(), fBodyBodyAccJerkMag-mean(), fBodyBodyAccJerkMag-std(), fBodyBodyAccJerkMag-meanFreq(), fBodyBodyGyroMag-mean(), fBodyBodyGyroMag-std(), fBodyBodyGyroMag-meanFreq(), fBodyBodyGyroJerkMag-mean(), fBodyBodyGyroJerkMag-std(), fBodyBodyGyroJerkMag-meanFreq()


# Variables
The variables are a subset of those found in the full dataset. The full list of variables is given in the features.txt file included in the data. Of the 561 variables, or features, only the data for mean and standard deviation is included. This totals 81 variables in the data set.

# File format

Tidy dataset is a txt file with header. String fields are surrounded by quotes (").

The file can be loaded in R, with the following command:

    write.table(Meandata, "Meandata.txt", sep="\t")

The dataset contains 180 observations (6 activities x 30 subjects), and 79 fields by observations.

# Fields description

| Field | Description | Origin |
| ----- | ----------- | ------ |
| subject | the subject identifier | Taken from the subject_{train,test}.txt and merged with observations |
| activity | the label of the activity (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) | Taken from a join between y_{train,test}.txt and activity_labels.txt |


Then the next values are the means by activity and subject of the mean and std variables from the X_{train,test}.txt files:

| Field | Description |
| ----- | ----------- |
| tBodyAcc / fBodyAcc (mean and std for X,Y,Z) | Body Acceleration (time and frequency) |
| tGravityAcc (mean and std for X,Y,Z) | Gravity Acceleration (time only) |
| tBodyAccJerk / fBodyAccJerk (mean and std for X,Y,Z) | Body Acceleration Jerk (time and frequency) |
| tBodyGyro / fBodyGyro (mean and std for X,Y,Z) | Body Gyroscope (time and frequency) |
| tBodyGyroJerk / fBodyGyroJerk  (mean and std for X,Y,Z) | Body Gyroscope Jerk (time and frequency) |
| tBodyAccMag / fBodyAccMag (mean and std) | Body Acceleration Magnitude (time and frequency) |
| tGravityAccMag (mean and std) | Gravity Acceleration Magnitude (time only) |
| tBodyAccJerkMag / fBodyAccJerkMag (mean and std) | Body Acceleration Jerk Magnitude (time and frequency) |
| tBodyGyroMag / fBodyGyroMag (mean and std) | Body Gyroscope Magnitude (time and frequency) |
| tBodyGyroJerkMag / fBodyGyroJerkMag (mean and std) | Body Gyroscope Jerk Magnitude (time and frequency) |

> The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
>
> Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
>
> Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
