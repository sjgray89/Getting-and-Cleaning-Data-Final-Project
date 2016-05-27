library(dplyr)
library(tidyr)
#Creating first tidy data set-----------------------------------------------------------------------------------------

#Check if file/directory exists in wd. If not, downloads it.
if(!file.exists("fp.zip")){
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","fp.zip")
}
#Unzips fp.zip if it hasn't already been unzipped        
if(!file.exists("./UCI HAR Dataset")){unzip("fp.zip")}

#read features and activity labels
features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- make.names(names=features[,2],unique=TRUE,allow_=TRUE)

#label activity descriptions
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
trainLabels <- read.table("UCI HAR Dataset/train/Y_train.txt")
testLabels <- read.table("UCI HAR Dataset/test/Y_test.txt")
mergeLabels <- rbind(trainLabels,testLabels)
colnames(mergeLabels) <- c("activity")
for (i in 1:nrow(activityLabels)) {
        mergeLabels$activity <- gsub(as.character(i),activityLabels[i,2],mergeLabels$activity)
}

#read and merge data sets
train <- read.table("UCI HAR Dataset/train/X_train.txt")
test <- read.table("UCI HAR Dataset/test/X_test.txt")
dat <- rbind(train,test)
colnames(dat) <- features[,2]

#read and combine subjectnumbers
subNumTrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
subNumTest <- read.table("UCI HAR Dataset/test/subject_test.txt")
subNum <- rbind(subNumTrain,subNumTest)
colnames(subNum) <- c("subject")

#combine the test and training data sets and activity labels
dat <- cbind(subNum, mergeLabels, dat)

#sort and select proper only mean and SD data, as well as subject/activity columns
dat <- dat %>% arrange(subject) %>% select(matches("(\\.mean[^F]|\\.std|subject|activity)"))

#Create second Tidy Data Set-----------------------------------------------------------------------------------------
dat2 <-  dat %>% group_by(subject,activity) %>%
        summarize_each(funs(mean)) %>%
        gather(variable,value,-(subject:activity)) %>%
        unite(temp,activity,variable) %>%
        spread(temp,value)

#Write Tidy Data Set, Part 5
write.table(x=dat2, file="tidyD_part5.txt",row.names = FALSE)