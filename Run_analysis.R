### Getting cleaning data project

## Packages
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)

## A)  Get data
## 1) download
if(!file.exists("./data")){dir.create("./data")}
fileURL1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL1, "project.zip")
## 2) unzip

UCI_data <- unzip("project.zip")

## 3) Read data
featureNames <- read.table("UCI HAR Dataset/features.txt")
head(featureNames)
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
head(activityLabels)

subjectTraining <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
head(subjectTraining)
activityTraining <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
head(activityTraining)
featuresTraining <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
head(featuresTraining)

subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
head(subjectTest)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
head(activityTest)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
head(featuresTest)


###  1 Merging data

subject <- rbind(subjectTraining, subjectTest)
activity <- rbind(activityTraining, activityTest)
features <- rbind(featuresTraining, featuresTest)

colnames(features) <- t(featureNames[2])

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)
View(completeData)

### 2) Getting columns with mean and STD

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
columnsWithMeanSTD
requiredColumns <- c(columnsWithMeanSTD, 562, 563) ## 562 activity 563 subject
Data_with_mean_STD <- completeData[,requiredColumns]
View (Data_with_mean_STD)

### 3) descriptive names for activities

class(Data_with_mean_STD$Activity)

Data_with_mean_STD$Activity <- as.character(Data_with_mean_STD$Activity)
for (i in 1:6){
  Data_with_mean_STD$Activity[Data_with_mean_STD$Activity == i] <- as.character(activityLabels[i,2])
}

class(Data_with_mean_STD$Activity)
Data_with_mean_STD$Activity <- as.factor(Data_with_mean_STD$Activity)

## 4) Appropriately labels the data set with descriptive variable names

names(Data_with_mean_STD)<-gsub("Acc", "Accelerometer", names(Data_with_mean_STD))
names(Data_with_mean_STD)<-gsub("Gyro", "Gyroscope", names(Data_with_mean_STD))
names(Data_with_mean_STD)<-gsub("BodyBody", "Body", names(Data_with_mean_STD))
names(Data_with_mean_STD)<-gsub("Mag", "Magnitude", names(Data_with_mean_STD))
names(Data_with_mean_STD)<-gsub("^t", "Time", names(Data_with_mean_STD))
names(Data_with_mean_STD)<-gsub("^f", "Frequency", names(Data_with_mean_STD))
names(Data_with_mean_STD)<-gsub("tBody", "TimeBody", names(Data_with_mean_STD))
names(Data_with_mean_STD)<-gsub("-mean()", "Mean", names(Data_with_mean_STD), ignore.case = TRUE)
names(Data_with_mean_STD)<-gsub("-std()", "STD", names(Data_with_mean_STD), ignore.case = TRUE)
names(Data_with_mean_STD)<-gsub("-freq()", "Frequency", names(Data_with_mean_STD), ignore.case = TRUE)
names(Data_with_mean_STD)<-gsub("angle", "Angle", names(Data_with_mean_STD))
names(Data_with_mean_STD)<-gsub("gravity", "Gravity", names(Data_with_mean_STD))
View(Data_with_mean_STD)
### Tidy data set with the average of each variable for each activity and each subject.
tidyData <- aggregate(. ~Subject, data = Data_with_mean_STD, mean)
View(tidyData)
write.table(tidyData, "TidyData.txt", row.name=FALSE)