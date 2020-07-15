#Load libraries
library(dplyr)

#Create data folder
if (!dir.exists("./data")) {
        dir.create("./data")
}


#Download and extract files from zip
zipurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(zipurl, "./data/Dataset.zip")
unzip("./data/Dataset.zip", exdir = "./data")
datadir <- "./data/UCI HAR Dataset"


#Import Features vec and remove the numbers to get our col names
features <- read.table("./data/UCI HAR Dataset/features.txt")
features <- features[ ,2]

#Find features that are either mean or standard deviation, then tidying up the names
selectionvec <- grep("(mean|std)\\(\\)", features)
selectedfeatures <- features[selectionvec]
selectedfeatures <-  gsub("-", "_", selectedfeatures)
selectedfeatures <-  gsub("\\(\\)", "", selectedfeatures)

#
#Import all our data into a table then set the column names
#

#Training data
X_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt", header=FALSE)
X_train <- X_train[ ,selectionvec]
colnames(X_train) <- selectedfeatures
activity_labels1 <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
X_train <- mutate(X_train, activity_label = case_when(activity_labels1 == 1 ~ "walking",
                                                      activity_labels1 == 2 ~ "walking_upstairs",
                                                      activity_labels1 == 3 ~ "walking_downstairs",
                                                      activity_labels1 == 4 ~ "sitting",
                                                      activity_labels1 == 5 ~ "standing",
                                                      activity_labels1 == 6 ~ "laying"))


#Testing data
X_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt", header=FALSE)
X_test <- X_test[ ,selectionvec]
colnames(X_test) <- selectedfeatures
activity_labels2 <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
X_test <- mutate(X_test, activity_label = case_when(activity_labels2 == 1 ~ "walking",
                                                    activity_labels2 == 2 ~ "walking_upstairs",
                                                    activity_labels2 == 3 ~ "walking_downstairs",
                                                    activity_labels2 == 4 ~ "sitting",
                                                    activity_labels2 == 5 ~ "standing",
                                                    activity_labels2 == 6 ~ "laying"))


trainandtest <- bind_rows(X_train, X_test)

#Get all subjects in correct order
subjects1 <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
subjects2 <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
subjects <- bind_rows(subjects1, subjects2)


#Add subjects column to trainandtest
trainandtest <- mutate(trainandtest, subject = subjects$V1)
trainandtest <- as_tibble(trainandtest)

#Group and then summarise the mean for each variable, then export our data
tidydata <- trainandtest %>% group_by(activity_label, subject)
tidysummary <- summarise_all(tidydata, mean, na.rm = TRUE)

write.csv(tidysummary, "./data/tidysummary.csv", row.names = FALSE)

