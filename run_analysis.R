project1 <- function(workingDirectory){
  #Requires the following packages to work
  #data.table
  #dplyr
  setwd(workingDirectory)
  
  #read all the files from the test directory
  testSubject <- read.table(paste(workingDirectory, "/test/subject_test.txt", sep=""))
  testX <- read.table(paste(workingDirectory, "/test/X_test.txt", sep = ""))
  testY <- read.table(paste(workingDirectory, "/test/Y_test.txt", sep = ""))
  
  #read all the files from the train directory
  trainSubject <- read.table(paste(workingDirectory, "/train/subject_train.txt", sep = ""))
  trainX <- read.table(paste(workingDirectory, "/train/X_train.txt", sep = ""))
  trainY <- read.table(paste(workingDirectory, "/train/Y_train.txt", sep = ""))
  
  #Get all the header info for the X data
  featureData <- read.table(paste(workingDirectory, "/features.txt", sep = ""))
  setnames(featureData, names(featureData), c("Id", "Name"))
  
  #Merge the test data and name the columns
  allData <- rbind(mergeData(testX, testY, testSubject, featureData), mergeData(trainX, trainY, trainSubject, featureData))
  
  #Get the ActivityNum, Subject, std, and mean fields from the table
  filteredData <- extractData(allData, featureData)
  
  #Add Descriptive Naming to the data.
  filteredData <- addVerboseNaming(filteredData)
  
  #Create a tidy data set with the filtered Data
  #STEP 5
  tidyData <- ddply(filteredData, c("Subject", "ActivityNum"), numcolwise(mean))
  write.table(tidyData, "tidyData.txt", row.names = FALSE)
}

##MERGEDATA
## STEP 1 and 3
##Returns a combined data set with all the column headers
##for each subset of data (test, train)
mergeData <- function(xData, yData, subjectData, feature){
  #Add headers to each set of data
  setnames(yData, "V1", "ActivityNum")
  setnames(subjectData, "V1", "Subject")
  names(xData) <- feature$Name

  #bind the XData and the subjectdata then bind the yData to the new dataSet
  returnData <- cbind(xData, subjectData)
  returnData <- cbind(returnData, yData)
  return(returnData)
}

#FUNCTION EXTRACTDATA
#STEP 2
#Returns a filtered table on the cols containing the following
#ActivityNum, Subject, mean, std
extractData <- function(allData, features)
{
  #Create a regex to extract only the data from the columns we want
  filteredData <- grep("mean\\(\\)|std\\(\\)",features$Name,value=TRUE)
  
  filteredData <- union(c("ActivityNum", "Subject"), filteredData)
  returnData <- subset(allData, select=filteredData)
  return(returnData)
  
}

#FUNCTION addVerboseNaming
#STEP 4
#Adds descriptive names to the Current Columns of the dataset
addVerboseNaming <- function(allData)
{
  names(allData)<-gsub("BodyBody", "Body", names(allData))
  names(allData)<-gsub("-std()", "_STD", names(allData))
  names(allData)<-gsub("-mean()", "_MEAN", names(allData))
  names(allData)<-gsub("^tBody", "Time.Body.", names(allData))
  names(allData)<-gsub("^tGravity", "Time.Gravity.", names(allData))
  names(allData)<-gsub("^fBody", "Freq.Body.", names(allData))
  names(allData)<-gsub("^fGravity", "Freq.Gravity", names(allData))
  names(allData)<-gsub("Acc", "Accelerometer", names(allData))
  names(allData)<-gsub("Gyro", "Gyroscope", names(allData))
  names(allData)<-gsub("Mag", "Magnitude", names(allData))
  
  names(allData)<-gsub("\\(|\\)", "", names(allData))
  
  return(allData)
}
