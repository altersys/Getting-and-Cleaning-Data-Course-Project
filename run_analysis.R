packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

# file paths
train_measurements_file                 <- "UCI HAR Dataset/train/X_train.txt"
train_measurement_activities_file       <- "UCI HAR Dataset/train/Y_train.txt"
train_measurement_subjects_file         <- "UCI HAR Dataset/train/subject_train.txt"
test_measurements_file                  <- "UCI HAR Dataset/test/X_test.txt"
test_measurement_activities_file        <- "UCI HAR Dataset/test/Y_test.txt"
activities_file                         <- "UCI HAR Dataset/activity_labels.txt"
features_file                           <- "UCI HAR Dataset/features.txt"
subjects_train_file                     <- "UCI HAR Dataset/train/subject_train.txt"
subjects_test_file                      <- "UCI HAR Dataset/test/subject_test.txt"
data_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
data_file<- "dataFiles.zip" 

# Function to download and unzip data
download.file(data_url, data_file)
unzip(zipfile = data_file)

# Function to load train or test measurenemts
# Input: filename, required features, required features names
# Output: measurement data.table
load_measurements <- function(measurements_file, 
                              features_required_names,
                              features_required ) {
        # Load only required features of measurements
        # to data.table
        measurements <- fread(measurements_file,
                             col.names   = features_required_names,
                             check.names = TRUE,
                             select      = features_required)
        measurements
}

# Function to load activities and measerements activities(training/test labels)  
# and join then to get descriptive activity names in measurements dataset
join_measurements_activities <- function(measurements_datatable, 
                                         measurement_activities_file,
                                         activities_file ) {
        activities <- fread(activities_file,col.names = c("activity", "activityName") )
        measurement_activities <- fread(measurement_activities_file, col.names = "activity")
        measurements_and_activities <- cbind(measurement_activities, measurements_datatable)
        measurements_and_activities$activity <- factor(measurements_and_activities$activity,
                                                             levels = activities$activity, 
                                                             labels = activities$activityName)
        measurements_and_activities
}

# We will extract only the measurements on the mean and standard deviation
# for each measurement. Scan features file to get vector of features to look for
features                <- fread(features_file)
features_required       <- grep("mean\\(\\)|std\\(\\)",features$V2)
features_required_names <- features[features_required,2][[1]]

# Invoking created train_measurements function to load train and test measurements
train_measurements <- load_measurements(train_measurements_file,features_required_names,features_required)
test_measurements  <- load_measurements(test_measurements_file,features_required_names,features_required)

# Using descriptive activity names to name the activities in the data set
train_measurements_and_activities <- join_measurements_activities(train_measurements,
                                                                  train_measurement_activities_file, 
                                                                  activities_file )
test_measurements_and_activities  <- join_measurements_activities(test_measurements,
                                                                  test_measurement_activities_file, 
                                                                  activities_file )
# Merging the training and the test sets to create one data set.
# Resultant dataset for tasks 1..4:
measurements_and_activities   <- rbind(train_measurements_and_activities,
                                           test_measurements_and_activities) 
# From the data set in step 4, creating a second, independent tidy data set
# with the average of each variable for each activity and each subject
train_subjects  <- fread(subjects_train_file, col.names = "subject")
test_subjects   <- fread(subjects_test_file, col.names = "subject")
subjects        <- rbind(train_subjects,test_subjects)
meas_act_subj   <- cbind(measurements_and_activities, subjects)

melted_meas     <- melt(meas_act_subj, id = c("subject", "activity"))
# Resultant dataset for tasks 5:
tidy_meas       <- dcast(melted_meas, subject + activity ~ variable, mean)
write.table(tidy_meas, "./tidy_dataset.txt", row.names = FALSE, quote = FALSE)




