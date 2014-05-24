## - 'features_info.txt': Shows information about the variables used on the feature vector.
## - 'features.txt': List of all features.
## - 'activity_labels.txt': Links the class labels with their activity name.
## - 'train/X_train.txt': Training set.
## - 'train/y_train.txt': Training labels.
## - 'test/X_test.txt': Test set.
## - 'test/y_test.txt': Test labels.

##  - Features are normalized and bounded within [-1,1].

features = read.table("features.txt")
activity_labels = read.table("activity_labels.txt")

X_test = read.table("test/X_test.txt")
Y_test = read.table("test/Y_test.txt")
subject_test = read.table("test/subject_test.txt")

X_train = read.table("train/X_train.txt")
Y_train = read.table("train/Y_train.txt")
subject_train = read.table("train/subject_train.txt")

## 1 Merges the training and the test sets to create one data set.
#Add the subject test/train to the data farmeshist
Test <- data.frame(subject_test, Y_test, X_test)
Train <- data.frame(subject_train, Y_train, X_train)
#Need to fix some column names
names(Test)[names(Test)=="V1"] <- "Subject-mean"
names(Test)[names(Test)=="V1.1"] <- "V0"
names(Test)[names(Test)=="V1.2"] <- "V1"
names(Train)[names(Train)=="V1"] <- "Subject-mean"
names(Train)[names(Train)=="V1.1"] <- "V0"
names(Train)[names(Train)=="V1.2"] <- "V1"
#Merge the tests and training
data <- rbind(Test,Train)


## 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
##Merging the acitvity label with the features
## (Inefficient I know ,but it works)
V1 <- c(0)
V2 <- c("activity-mean")
df <- data.frame(V1,V2)
features <- rbind (df,features)
V1 <- c(0)
V2 <- c("subject-mean")
df <- data.frame(V1,V2)
features <- rbind (df,features)
##Replacing the column names with the feature name
colnames(data) <- features[,2]
##Keep only columns with mean or std, including meanFreq as those are mean values
data <- data[ , grepl(paste(c("mean()","std()"), collapse='|'), names( data ) ) ]
##Fixing a column name that was made that way just to make the last step easier
names(data)[names(data)=="activity-mean"] <- "activity"
names(data)[names(data)=="subject-mean"] <- "subject"


## 3 Uses descriptive activity names to name the activities in the data set
data$activity[data$activity == 1] <- "WALKING"
data$activity[data$activity == 2] <- "WALKING_UPSTAIRS"
data$activity[data$activity == 3] <- "WALKING_DOWNSTAIRS"
data$activity[data$activity == 4] <- "SITTING"
data$activity[data$activity == 5] <- "STANDING"
data$activity[data$activity == 6] <- "LAYING"


## 4 Appropriately labels the data set with descriptive activity names. 
##     I already did this in step 2 to make it easier for me to access the mean and std data.
##     I adjusged the column names to say what the value being measured was for the acitvity


## 5 Creates a second, independent tidy data set with the average of each variable for each activity 
##    and each subject. 
##    For thsi one I basically do four things six times. I first subset the data by the activity
##    e.g. WALKING. Then I aggregate the data so that I get the mean of that subset based on the subject
##    afterwards I need to do a bit of clean up as for some reason a new column and nas are introduced
##    Then I just add all the rows together and now I have a data set of 180 rows by 81 columns (remember
##    I included MeanFreq data
WALKING <- subset(data, data$activity=="WALKING")
WALKING <- aggregate(WALKING, list(subject=WALKING$subject),mean)
WALKING[is.na(WALKING)] <- "WALKING"
WALKING <- WALKING[-1]
WALKING_UPSTAIRS <- subset(data, data$activity=="WALKING_UPSTAIRS")
WALKING_UPSTAIRS <- aggregate(WALKING_UPSTAIRS, list(subject=WALKING_UPSTAIRS$subject),mean)
WALKING_UPSTAIRS[is.na(WALKING_UPSTAIRS)] <- "WALKING_UPSTAIRS"
WALKING_UPSTAIRS <- WALKING_UPSTAIRS[-1]
WALKING_DOWNSTAIRS <- subset(data, data$activity=="WALKING_DOWNSTAIRS")
WALKING_DOWNSTAIRS <- aggregate(WALKING_DOWNSTAIRS, list(subject=WALKING_DOWNSTAIRS$subject),mean)
WALKING_DOWNSTAIRS[is.na(WALKING_DOWNSTAIRS)] <- "WALKING_DOWNSTAIRS"
WALKING_DOWNSTAIRS <- WALKING_DOWNSTAIRS[-1]
SITTING <- subset(data, data$activity=="SITTING")
SITTING <- aggregate(SITTING, list(subject=SITTING$subject),mean)
SITTING[is.na(SITTING)] <- "SITTING"
SITTING <- SITTING[-1]
STANDING <- subset(data, data$activity=="STANDING")
STANDING <- aggregate(STANDING, list(subject=STANDING$subject),mean)
STANDING[is.na(STANDING)] <- "STANDING"
STANDING <- STANDING[-1]
LAYING <- subset(data, data$activity=="LAYING")
LAYING <- aggregate(LAYING, list(subject=LAYING$subject),mean)
LAYING[is.na(LAYING)] <- "LAYING"
LAYING <- LAYING[-1]
Meandata <- rbind(WALKING,WALKING_UPSTAIRS,WALKING_DOWNSTAIRS,SITTING,STANDING,LAYING)
write.table(Meandata, "Meandata.txt", sep="\t")

