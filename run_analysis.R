# run_analysis.R does the following. 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# read data from files
features     = read.table('features.txt',header=FALSE);  
activityType = read.table('activity_labels.txt',header=FALSE); 
subjectTrain = read.table('train/subject_train.txt',header=FALSE); 
xTrain       = read.table('train/x_train.txt',header=FALSE);  
yTrain       = read.table('train/y_train.txt',header=FALSE);  

# Give column names to the data
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# Merge {yTrain, subjectTrain,xTrain}: get a train set
trainingData = cbind(yTrain,subjectTrain,xTrain);

# read test data
subjectTest = read.table('test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('test/y_test.txt',header=FALSE); #imports y_test.txt

# column names to the test data 
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";


# final test: merge {xTest, yTest,subjectTest}
testData = cbind(yTest,subjectTest,xTest);


# Combine training and test data: create final dataset
finalData = rbind(trainingData,testData);

# Create a vector for column names from finalData, to get mean and stddev() columns
colNames  = colnames(finalData); 

# Get measurements of mean and std deviation for each measurement. 

# Create a logicalVector with TRUE values for ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector: keep only needed data
finalData = finalData[logicalVector==TRUE];

# Merge finalData set with acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# Update colNames vector, include new column names
colNames  = colnames(finalData); 

# Clean variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Assign new descriptive column names to  final dataset
colnames(finalData) = colNames;

# Finally, create an independent TIDY dataset with average for each activity/variable/subject. 

# Create a new table, finalDataNoActivityType WITHOUT  activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

# Include mean for each activity/variable/subject.
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging tidyData & activityType: include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set (180 rows....)
write.table(tidyData, 'tidyData.txt',row.names=TRUE,sep='\t');