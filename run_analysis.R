#load in needed libraries
library(dplyr)

#load in the needed data
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt")
xTest<- read.table("UCI HAR Dataset/test/X_test.txt")
yTest <- read.table("UCI HAR Dataset/test/Y_test.txt")
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
xTrain <- read.table("UCI HAR Dataset/train/X_train.txt")
yTrain <- read.table("UCI HAR Dataset/train/Y_train.txt")
features <- read.table("UCI HAR Dataset/features.txt")
activity <- read.table("UCI HAR Dataset/activity_labels.txt")

#renaming columns to prevent confusion
yTest <- rename(yTest, Activity = V1)
subjectTest <- rename(subjectTest, Subject = V1)
yTrain <- rename(yTrain, Activity = V1)
subjectTrain <- rename(subjectTrain, Subject = V1)

#merge the training and testing data sets
trainSet <- cbind(subjectTrain, yTrain, xTrain)
testSet <- cbind(subjectTest, yTest, xTest)

#finding the columns we want based on program instructions and features.txt 
mean_std_indicies <- grep("(mean|std)", features$V2)
mean_std_column_names <- paste("V", mean_std_indicies, sep = "")
desired_columns <- c("Subject", "Activity", mean_std_column_names)

#extracting only the desired columns from the training and testing data sets
trainSet <- trainSet %>% select(any_of(desired_columns))
testSet <- testSet %>% select(any_of(desired_columns))

#substituting the activity number identifier for the actual activity for training and testing data sets
trainSet$Activity <- activity$V2[trainSet$Activity]
testSet$Activity <- activity$V2[testSet$Activity]

#assigning variable names their descriptive titles
colnames(trainSet)[3:length(colnames(trainSet))] <- features$V2[as.numeric(gsub("V", "", colnames(trainSet)[3:length(colnames(trainSet))]))]
colnames(testSet)[3:length(colnames(testSet))] <- features$V2[as.numeric(gsub("V", "", colnames(testSet)[3:length(colnames(testSet))]))]

#finding the mean based for each column and putting it in a new dataframe
testSet2 <- testSet %>% group_by(Activity, Subject) %>% summarise_all(mean)
trainSet2 <- trainSet %>% group_by(Activity, Subject) %>% summarise_all(mean)

#writing the data to new tidy files
write.table(trainSet, 'training_set.txt')
write.table(trainSet2, 'training_set_averages.txt')
write.table(testSet, 'testing_set.txt')
write.table(testSet2, 'testing_set_averages.txt')