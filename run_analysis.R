
library(RCurl)

if (!file.exists("./data")) {dir.create("./data")}
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl, destfile = "./data/my_project", method='curl')
        unzip("./data/my_project")

        #1. Merges the training and the test sets to create one data set.
        x.train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
        x.test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
        x <- rbind(x.train, x.test)
        subj.train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
        subj.test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
        subj <- rbind(subj.train, subj.test)
        y.train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
        y.test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
        y <- rbind(y.train, y.test)

        #2. Extracts only the measurements on the mean and standard deviation for each measurement.        
        features <- read.table("./data/UCI HAR Dataset/features.txt")
        mean_ <- grep("-mean",features[,2])
        std_ <- grep("-std",features[,2])
        mean_std <- c(mean_,std_)
        x_mean_sd <- x[, mean_std]

        #3. Uses descriptive activity names to name the activities in the data set
        activities <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
        activities[, 2] <- tolower(as.character(activities[, 2]))
        activities[, 2] <- gsub("_", " ", activities[, 2])
        names(x_mean_sd) <- features[mean_std,2]
        colnames(y) <- "activity"
        y[,1] <- activities[y[, 1], 2]
        
        #4. Appropriately labels the data set with descriptive variable names
        colnames(subj) <- "subject"
        final.data <- cbind(subj, x_mean_sd, y)
        write.table(final.data, "./data/merged.txt", row.names = F)
        
        #5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        average.df <- aggregate(x=final.data, by=list(activities=final.data$activity, subj=final.data$subject), FUN=mean)
        average.df <- average.df[, !(colnames(average.df) %in% c("subj", "activity"))]
        #str(average.df)
        #dim(average.df)
        #dim(final.data)
        write.table(average.df, "./data/average.txt", row.names = F)

        