##Load test data
test <- read.table("Data/test/X_test.txt")
testAct <- read.table("Data/test/Y_test.txt")
testSub <- read.table("Data/test/subject_test.txt")

##Load training data
train <- read.table("Data/train/X_train.txt")
trainAct <- read.table("Data/train/Y_train.txt")
trainSub <- read.table("Data/train/subject_train.txt")

#Merge activities
test$act <- testAct$V1
train$act <- trainAct$V1

#Merge subjects
test$sub <- testSub$V1
train$sub <- trainSub$V1

##Label test and training sets
test$set <- "Test"
train$set <- "Train"

##Append test set to training set
complete <- rbind(train, test)

#Label columns from features.txt
colNames <- read.table("Data/features.txt")
names(complete) <- c(as.vector(colNames$V2), "act","sub", "set")

##Extract variables for mean and sd
##These variabls end in "-mean()", "-std()", "-mean()-XYZ", "-std()-XYZ"

endings <- c("-mean()", "-std()", "-mean()-X", "-mean()-Y", "-mean()-Z", "-std()-X", "-std()-Y", "-std()-Z")
subIndex <- NA

for (i in 1:length(complete)) {
      varName <- names(complete[i])
      for (endn in nchar(endings)) {
            varEnding <- substr(varName, nchar(varName)-endn, nchar(varName))
            if (varEnding %in% endings) {
                  subIndex <- c(subIndex, i)
            }
      }
}
      
subIndex <- c(unique(subIndex[2:length(subIndex)]), 562: length(complete))

subSet <- complete[,subIndex]

#Label activities
activityCodes <- read.table("Data/activity_labels.txt")
names(activityCodes) <- c("act", "activityName")
subSetActivities <- merge(subSet, activityCodes, by.x = "act", by.y = "act")

#Create dataset of means by subject and Activity (name)
a <- aggregate(subSetActivities[,2:(length(subSetActivities) - 3)], by = list(subSetActivities[,"activityName"], subSetActivities[,"sub"]), mean, na.rm=TRUE)

names(a) <- c("activityName", "sub", names(a[3:length(a)]))

write.table(a, file="Output/activity_subject_means.txt", row.name=FALSE)
