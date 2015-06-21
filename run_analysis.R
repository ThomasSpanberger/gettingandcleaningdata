runanalysis <- function()
{
  
## Loading data into R

variablenamestable <- read.table("./UCI HAR Dataset/features.txt")
data_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
data_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
subj_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subj_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
activ_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
activ_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
activ_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

## Naming the columns in data_train and data_test
variablenames <- as.vector(variablenamestable[,2])
colnames(data_train) <- variablenames
colnames(data_test) <- variablenames

## Naming the columns in subj_train and subj_test
colnames(subj_train)[1] <- c("subject")
colnames(subj_test)[1] <- c("subject")

## Naming the columns in activ_train and activ_test
colnames(activ_train)[1] <- c("activity")
colnames(activ_test)[1] <- c("activity")

## numbering the data_train dataframe

numberofrows <- nrow(data_train)
index <- 1:numberofrows
data_train$index <- index
rm (index)
rm(numberofrows)

## numbering the subj_train dataframe

numberofrows <- nrow(subj_train)
index <- 1:numberofrows
subj_train$index <- index
rm (index)
rm (numberofrows)

## numbering the activ_train dataframe

numberofrows <- nrow(activ_train)
index <- 1:numberofrows
activ_train$index <- index
rm (index)
rm (numberofrows)

## merging the train data frames

part_merged_train = merge(data_train, subj_train, by="index", all=TRUE)
df_train = merge(part_merged_train, activ_train, by="index", all=TRUE)
rm(part_merged_train)


## numbering the data_test dataframe

numberofrows <- nrow(data_test)
index <- 1:numberofrows
data_test$index <- index
rm (index)
rm(numberofrows)

## numbering the subj_test dataframe

numberofrows <- nrow(subj_test)
index <- 1:numberofrows
subj_test$index <- index
rm (index)
rm (numberofrows)

## numbering the activ_test dataframe

numberofrows <- nrow(activ_test)
index <- 1:numberofrows
activ_test$index <- index
rm (index)
rm (numberofrows)

## merging the test data frames

part_merged_test = merge(data_test, subj_test, by="index", all=TRUE)
df_test = merge(part_merged_test, activ_test, by="index", all=TRUE)
rm(part_merged_test)

##merging the test and train data frames

df_full <- rbind(df_train, df_test)
df_full$index <- NULL
valid_column_names <- make.names(names=names(df_full), unique=TRUE, allow_ = TRUE)
names(df_full) <- valid_column_names

df_filtered <- select(.data=df_full, subject, activity, contains("mean"), contains("std"), -contains("angle"), -contains("meanFreq"))

## reading descriptive names for activites out of activ_labels

activitynames <- as.vector(activ_labels[,2])

## giving descriptive names to activities
numberofactivities <- length(activitynames)

for (i in 1:numberofactivities)
{
df_filtered[,"activity"][df_filtered[,"activity"] == i] <- activitynames[i]
}

## calculating the mean data for each subject and activity, and creating the new table

meandata <- aggregate(df_filtered[3:68], by=list(df_filtered$subject, df_filtered$activity), FUN=mean)
names(meandata)[names(meandata) == 'Group.1'] <- 'Subject'
names(meandata)[names(meandata) == 'Group.2'] <- 'activity'

##write a file of the new table
write.table(meandata, file="resultsprojectgettingandcleaningdata.txt", row.names=FALSE)
}