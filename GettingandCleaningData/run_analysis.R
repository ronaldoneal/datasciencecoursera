###############################################################################################


###############################################################################################
#  Read features.txt to get the header names for the data table columns
###############################################################################################

features <- read.table("features.txt", header = FALSE)
colnames(features)[1] <- "featureid"
colnames(features)[2] <- "feature"
features[,2] <- gsub("-","",features[,2],ignore.case = TRUE)
features[,2] <- gsub("\\(","",features[,2],ignore.case = TRUE)
features[,2] <- gsub("\\)","",features[,2],ignore.case = TRUE)
features[,2] <- gsub(",","",features[,2],ignore.case = TRUE)

activity_labels <- read.table("activity_labels.txt", header = FALSE)
colnames(activity_labels)[1] <- "activityid"
colnames(activity_labels)[2] <- "activity"

###############################################################################################
#  1. 1-a. Read training data into R and merge and sort on SubjectID, ActivityID
###############################################################################################

Strain <- read.table("train/subject_train.txt", header = FALSE)
Ytrain <- read.table("train/y_train.txt", header = FALSE)
Xtrain <- read.table("train/X_train.txt", header = FALSE)

###############################################################################################
#  Appropriately labels the data set with descriptive variable names (No. 4 below)
###############################################################################################
colnames(Xtrain) <- features[,2]

colnames(Strain)[1] <- "subjectid"
colnames(Ytrain)[1] <- "activityid"

###############################################################################################
#  Merge on row names (row number) and add variable
###############################################################################################
traindata <- merge(Strain, Ytrain, by="row.names", all=TRUE)
traindata$measurementtype <- "train"

###############################################################################################
#  Track rows matching between data tables
###############################################################################################
colnames(traindata)[1] <- "symatch"

###############################################################################################
#  Merge on row names (row number)
###############################################################################################
traindata <- merge(traindata, Xtrain, by="row.names", all=TRUE)

traindata[, 1]  <- as.numeric(traindata[, 1]) 
traindata[, 2]  <- as.numeric(traindata[, 2]) 

###############################################################################################
#  Track rows matching between data tables
###############################################################################################
colnames(traindata)[1] <- "syxmatch"

#################################################################################################################
#################################################################################################################
#  1. 1-b. Read test data into R and merge
#################################################################################################################
#################################################################################################################

Stest <- read.table("test/subject_test.txt", header = FALSE)
Ytest <- read.table("test/y_test.txt", header = FALSE)
Xtest <- read.table("test/X_test.txt", header = FALSE)

###############################################################################################
#  Appropriately labels the data set with descriptive variable names (No. 4 below)
###############################################################################################
colnames(Xtest) <- features[,2]

colnames(Stest)[1] <- "subjectid"
colnames(Ytest)[1] <- "activityid"

###############################################################################################
#  Merge on row names (row number) and add variable
###############################################################################################
testdata <- merge(Stest, Ytest, by="row.names", all=TRUE)
testdata$measurementtype <- "test"

###############################################################################################
#  Track rows matching between data tables
###############################################################################################
colnames(testdata)[1] <- "symatch"

###############################################################################################
#  Merge on row names (row number)
###############################################################################################
testdata <- merge(testdata, Xtest, by="row.names", all=TRUE)

testdata[, 1]  <- as.numeric(testdata[, 1]) 
testdata[, 2]  <- as.numeric(testdata[, 2]) 

###############################################################################################
#  Track rows matching between data tables
###############################################################################################
colnames(testdata)[1] <- "syxmatch"

#################################################################################################################
###############################################################################################
#  2. Extracts only the measurements on the mean and standard deviation for each measurement. 
###############################################################################################
#################################################################################################################
colset <- grep("subjectid|activityid|measurementtype|mean|std", colnames(traindata), value = TRUE, ignore.case = TRUE)

traindata <- traindata[, colset]
testdata <- testdata[, colset]

#################################################################################################################
###############################################################################################
#  3. Uses descriptive activity names to name the activities in the data set
###############################################################################################
#################################################################################################################

traindata <- merge(traindata, activity_labels, by="activityid", all=TRUE)
testdata  <- merge(testdata,  activity_labels, by="activityid", all=TRUE)

colset <- grep("mean|std", colnames(traindata), value = TRUE, ignore.case = TRUE)
colset <- c("subjectid","activity", "measurementtype", colset)

traindata <- traindata[, colset]
testdata <- testdata[, colset]

traindata <- traindata[order(traindata$subjectid, traindata$activity),]
testdata <- testdata[order(testdata$subjectid, testdata$activity),]

#################################################################################################################
###############################################################################################
#  4. Appropriately labels the data set with descriptive variable names.
###############################################################################################
#################################################################################################################

#  >>> Variable names added just after data load to minimize additional steps to track columns <<<

###############################################################################################
#  5. From the data set in step 4, creates a second, independent tidy data set with the 
#     average of each variable for each activity and each subject.
###############################################################################################

library(data.table)

fitdata <- rbind(traindata, testdata)
fitdata <- as.data.table(fitdata)
colnames(fitdata) <- tolower(colnames(fitdata))
setkey(fitdata, subjectid, activity, measurementtype)
fitdatatidy <- fitdata[,c(lapply(.SD,mean),count=.N),by=.(subjectid, activity, measurementtype)] 

write.table(fitdatatidy, file = "fitdata.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)                          



