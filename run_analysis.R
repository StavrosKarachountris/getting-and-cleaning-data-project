## run_analysis.R
## Getting and Cleaning Data -Project

if(!file.exists("./data")) {dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/ProjectDataset.zip")

## unzip dataset to /data directory

unzip(zipfile="./data/ProjectDataset.zip", exdir="./data")

## Reading testing tables:

x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

## Reading trainings tables:

x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")


## Reading feature vector:

features <- read.table('./data/UCI HAR Dataset/features.txt')

## Reading activity labels:

activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

## Assigning column names:

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(x_train) <- features[,2] 
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

## Step 1:Merging the training and the test sets to create one data set:

total_train <- cbind(y_train, subject_train, x_train)
total_test <- cbind(y_test, subject_test, x_test)
TotalDataset <- rbind(total_train, total_test)

## Reading column names:

colNames <- colnames(TotalDataset)

## Step 2: Extracting only the measurements on the mean and standard deviation for each measurement:

MeanAndStd <- (grepl("activityId" , colNames) | 
                         grepl("subjectId" , colNames) | 
                         grepl("mean.." , colNames) | 
                         grepl("std.." , colNames) 
)

## Subsetting from TotalDataset:

subsetofMeanAndStd <- TotalDataset[ , MeanAndStd == TRUE]

## Step 3: Using descriptive activity names to name the activities in the data set:

setDescriptiveNames <- merge(subsetofMeanAndStd, activityLabels,
                              by='activityId',
                              all.x=TRUE)

## Step 4: Appropriately labeling the data set with descriptive variable names:

names(setDescriptiveNames)<-gsub("^t", "time", names(setDescriptiveNames))
names(setDescriptiveNames)<-gsub("^f", "frequency", names(setDescriptiveNames))
names(setDescriptiveNames)<-gsub("Acc", "Accelerometer", names(setDescriptiveNames))
names(setDescriptiveNames)<-gsub("Gyro", "Gyroscope", names(setDescriptiveNames))
names(setDescriptiveNames)<-gsub("Mag", "Magnitude", names(setDescriptiveNames))
names(setDescriptiveNames)<-gsub("BodyBody", "Body", names(setDescriptiveNames))


## Step 5: Creating a second, independent tidy data set, from the data set in step 4,
## with the average of each variable for each activity and each subject: 

TidySet <- aggregate(. ~subjectId + activityId, setDescriptiveNames, mean)
TidySet <- TidySet[order(TidySet$subjectId, TidySet$activityId),]
