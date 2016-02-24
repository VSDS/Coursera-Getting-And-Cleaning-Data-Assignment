# Assignment for Coursera Data Science - Getting and Cleaning Data 
# By Venkat Sarvagnam

run_analysis <- function()
{

#1.	Merges the training and the test sets to create one data set.
# 	This takes care of only the rbind part.
# 	The merged data after cbind is under 4 below, after appropriate processing

	subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
	subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
	subjects <- rbind(subject_test, subject_train)
	names(subjects) <- c("SubjectID")
	dim(subjects) 

	X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
	X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
	X <- rbind(X_test, X_train)
	dim(X)
	
	Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")
	Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
	Y <- rbind(Y_test, Y_train)
	dim(Y)


#2.	Extracts only the measurements on the mean and standard deviation for each measurement.

	features <- read.table("UCI HAR Dataset/features.txt")
	names(features) <- c("FeatureID", "FeatureName")
	dim(features)

	mean_std_cols <- grep("mean\\(\\)|std\\(\\)", features[, 2])
	selectX <- X[, mean_std_cols]
	dim(selectX)

#3.	Uses descriptive activity names to name the activities in the data set.
	
	activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
	names(activity_labels) <- c("ActivityID", "ActivityName")
	dim(activity_labels)

	names(Y) <- c("ActivityID")
	activity <- merge(Y, activity_labels, by="ActivityID", all=TRUE, sort=FALSE)$ActivityName
	dim(activity)
	head(activity)
	


#4.	Appropriately labels the data set with descriptive variable names.

	names(selectX) <- features[mean_std_cols, 2]
	names(selectX) <- gsub("\\(\\)", "", names(selectX)) 
	names(selectX) <- gsub("-", "", names(selectX)) 
	names(selectX) <- gsub("mean", "Mean", names(selectX)) 
	names(selectX) <- gsub("std", "Std", names(selectX)) 

	mergedTidySelectData <- cbind(subjects, activity, selectX)
	dim (mergedTidySelectData)
	library(dplyr)
	mergedTidySelectData <- rename(mergedTidySelectData, Activity=activity)
	write.table(mergedTidySelectData, "merged_tidy_select_data.txt")

#5.	#Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
	library(data.table)
	summarizedTidySelectData <- data.table(mergedTidySelectData)
	summarizedTidySelectData <- summarizedTidySelectData[, lapply(.SD, mean), by=c("SubjectID", "Activity")]
	write.table(summarizedTidySelectData, "summarized_tidy_select_data.txt")


}