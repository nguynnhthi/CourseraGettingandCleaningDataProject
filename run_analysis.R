# Set the working directory to the folder of the dataset
setwd("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")

# Load data into data frames
subject_test <- read.table("./test/subject_test.txt")
x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_train <- read.table("./train/subject_train.txt")
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
features <- read.table("./features.txt")
activities <- read.table("./activity_labels.txt")

# Rename variable names in the data frames
library(dplyr)
subject_test <- rename(subject_test, Subject = V1)
y_test <- rename(y_test, Activity = V1)
subject_train <- rename(subject_train, Subject = V1)
y_train <- rename(y_train, Activity = V1)
names(x_test) <- features$V2
names(x_train) <- features$V2

# Merges the training and the test sets to create one data set
testfull <- cbind(subject_test, y_test, x_test)
trainfull <- cbind(subject_train, y_train, x_train)
full <- rbind(testfull, trainfull)

# Extracts only the measurements on the mean and standard deviation for each 
# measurement
# Find the indices of the mean and standard deviation measurements
a <- grep("mean()|std()", features$V2)
b <- grep("meanFreq()", features$V2)
a <- a[! a %in% b]
a <- a + 2
a <- append(a, c(1, 2), after = 0)
fullmeanstd <- select(full, a)

# Uses descriptive activity names to name the activities in the data set
activities <- mutate(activities, V2 = as.character(V2))
fullmeanstd <- mutate(fullmeanstd, Activity = activities[Activity, 2])

# Appropriately labels the data set with descriptive variable names
colnames(fullmeanstd) <- gsub("-", ".", colnames(fullmeanstd))

# Creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject
subjectactivityavg <- fullmeanstd %>% group_by(Subject, Activity) %>% 
  summarize(across(.cols = everything(), mean))

# Write the tidy dataset to a text file
write.table(subjectactivityavg, file = "./tidydata.txt", row.names = FALSE)