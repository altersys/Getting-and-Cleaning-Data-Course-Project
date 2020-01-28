# General Description
tidy_dataset contains processed data from Human Activity Recognition Using Smartphones Dataset by www.smartlab.ws
Data downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
The origin of data is http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# Processing
* Train and test datasets were merged by binding test data after train data
* Only mean and standard deviation measurements were included(see list below). All other measeurements were dropped.
* Subject id(from train/subject_train.txt and test/subject_test.txt respectively) and 
  human-readible activity attibutes(from activity_labels.txt) were added by column binding
* Average of each variable for each activity and each subject was calcutated

# List of Measurements(features)
|---|---                              |---                            |--- |
 |[1] |"tBodyAcc-mean()-X"           |"tBodyAcc-mean()-Y"           |"tBodyAcc-mean()-Z"    |      
 |[4] |"tBodyAcc-std()-X"            |"tBodyAcc-std()-Y"            |"tBodyAcc-std()-Z"     |     
 |[7] |"tGravityAcc-mean()-X"        |"tGravityAcc-mean()-Y"        |"tGravityAcc-mean()-Z" |    
|[10] |"tGravityAcc-std()-X"         |"tGravityAcc-std()-Y"         |"tGravityAcc-std()-Z"  |     
|[13] |"tBodyAccJerk-mean()-X"       |"tBodyAccJerk-mean()-Y"       |"tBodyAccJerk-mean()-Z"|      
|[16] |"tBodyAccJerk-std()-X"        |"tBodyAccJerk-std()-Y"        |"tBodyAccJerk-std()-Z" |      
|[19] |"tBodyGyro-mean()-X"          |"tBodyGyro-mean()-Y"          |"tBodyGyro-mean()-Z"   |      
|[22] |"tBodyGyro-std()-X"           |"tBodyGyro-std()-Y"           |"tBodyGyro-std()-Z"    |      
|[25] |"tBodyGyroJerk-mean()-X"      |"tBodyGyroJerk-mean()-Y"      |"tBodyGyroJerk-mean()-Z"|     
|[28] |"tBodyGyroJerk-std()-X"       |"tBodyGyroJerk-std()-Y"       |"tBodyGyroJerk-std()-Z" |     
|[31] |"tBodyAccMag-mean()"          |"tBodyAccMag-std()"           |"tGravityAccMag-mean()" |     
|[34] |"tGravityAccMag-std()"        |"tBodyAccJerkMag-mean()"      |"tBodyAccJerkMag-std()" |     
|[37] |"tBodyGyroMag-mean()"         |"tBodyGyroMag-std()"          |"tBodyGyroJerkMag-mean()"|    
|[40] |"tBodyGyroJerkMag-std()"      |"fBodyAcc-mean()-X"           |"fBodyAcc-mean()-Y"      |    
|[43] |"fBodyAcc-mean()-Z"           |"fBodyAcc-std()-X"            |"fBodyAcc-std()-Y"       |    
|[46] |"fBodyAcc-std()-Z"            |"fBodyAccJerk-mean()-X"       |"fBodyAccJerk-mean()-Y"  |    
|[49] |"fBodyAccJerk-mean()-Z"       |"fBodyAccJerk-std()-X"        |"fBodyAccJerk-std()-Y"   |    
|[52] |"fBodyAccJerk-std()-Z"        |"fBodyGyro-mean()-X"          |"fBodyGyro-mean()-Y"     |    
|[55] |"fBodyGyro-mean()-Z"          |"fBodyGyro-std()-X"           |"fBodyGyro-std()-Y"      |    
|[58] |"fBodyGyro-std()-Z"           |"fBodyAccMag-mean()"          |"fBodyAccMag-std()"      |    
|[61] |"fBodyBodyAccJerkMag-mean()"  |"fBodyBodyAccJerkMag-std()"   |"fBodyBodyGyroMag-mean()"|    
|[64] |"fBodyBodyGyroMag-std()"      |"fBodyBodyGyroJerkMag-mean()" |"fBodyBodyGyroJerkMag-std()"| 
