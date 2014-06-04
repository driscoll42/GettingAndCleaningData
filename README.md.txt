Course Project for Getting and Cleaning Data
--------------------------------------------

This is the submission for the course project of [Getting and Cleaning Data](https://www.coursera.org/course/getdata).

The code is in `run_analysis.R`. See comments inside the file.

To generate the tidy test data file, set your current working directory to the
directory containing this file and then:

    source("run_analysis.R")
    write_X_means()

It will download and unzip the file and create a 
X\_means\_by\_subject\_and\_activity.csv file in the current directory.

