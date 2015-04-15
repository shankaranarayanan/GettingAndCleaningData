cleanup <- function() {
  #Setting up the directory values
  baseDir <- "UCI HAR Dataset"
  testDir <- paste(baseDir,"test",sep = "\\")
  trainDir <- paste(baseDir,"train",sep = "\\")

  #Reading features and activites, common to both test and training data.
  features <- read.table(file = paste(baseDir,"features.txt",sep = "\\"),header = F,stringsAsFactors = F)
  activities <- read.table(file = paste(baseDir,"activity_labels.txt",sep = "\\"),header = F,stringsAsFactors = F)
  
  #Reading training data first, subject_train, xTrain and yTrain
  subjectTrain <- read.table(file = paste(trainDir,"subject_train.txt",sep = "\\"),header = F)
  xTrain <- read.table(file = paste(trainDir,"X_train.txt",sep = "\\"),header = F)
  yTrain <- read.table(file = paste(trainDir,"y_train.txt",sep = "\\"),header = F,)
  
  #Getting activity name for activity no
  yTrainActivity <- merge(yTrain, activities, by.x="V1",by.y="V1", all=F,sort = F)
  
  #naming x train based on features
  names(xTrain) <- features[,2]
  
  #Finding columns with mean and std
  xTrainMean <- xTrain[,grep(pattern = "mean",names(xTrain))]
  xTrainStd <- xTrain[,grep(pattern = "std",names(xTrain))]
  finalXTrain <- c(xTrainMean, xTrainStd)
  
  #Combining subject, activity and different mean and std measurements.
  trainData <- data.frame(subject = subjectTrain[,1], activity = yTrainActivity[,2], finalXTrain)
  
  #Similarly for test data
  subjectTest <- read.table(file = paste(testDir,"subject_test.txt",sep = "\\"),header = F)
  xTest <- read.table(file = paste(testDir,"X_test.txt",sep = "\\"),header = F)
  yTest <- read.table(file = paste(testDir,"y_test.txt",sep = "\\"),header = F,)
  
  
  yTestActivity <- merge(yTest, activities, by.x="V1",by.y="V1", all=F,sort = F)
  
  names(xTest) <- features[,2]
  xTestMean <- xTest[,grep(pattern = "mean",names(xTest))]
  xTestStd <- xTest[,grep(pattern = "std",names(xTest))]
  finalXTest <- c(xTestMean, xTestStd)
  
  testData <- data.frame(subject = subjectTest[,1], activity = yTestActivity[,2], finalXTest)
  
  #Combining both test and train data
  galaxyData <- rbind(trainData, testData)
  
  #Getting the mean of variables for combination of subject and activity
  tidyGalaxyData <- aggregate(x = galaxyData[,-c(1:2)],by = list(galaxyData$subject,galaxyData$activity),FUN = mean, data = galaxyData)
  
  #Renaming the columns to subject and activity
  names(tidyGalaxyData)[names(tidyGalaxyData)=="Group.1"] <- "subject"
  names(tidyGalaxyData)[names(tidyGalaxyData)=="Group.2"] <- "activity"
  tidyGalaxyData
}
tidyData <- cleanup()

#Writing data to file
write.table(tidyData,file = "tidyGalaxyData.txt",row.name=F)