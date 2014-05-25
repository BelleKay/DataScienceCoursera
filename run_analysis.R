# In this section of the R script we will first read in the data from the various files 
# for the purpose of later merging the files.

subject_tst <- read.table("./test/subject_test.txt")
subject_trn<- read.table("./train/subject_train.txt") 

X_tst<- read.table("./test/X_test.txt")
X_trn<- read.table("./train/X_train.txt")

Y_tst<- read.table("./test/y_test.txt")
Y_trn<- read.table("./train/y_train.txt")
 
activities<- read.table("activity_labels.txt") 
features <- read.table("features.txt") 

# This section applies descriptive names to the various activities in the dataset

Y_tst$V1[which(Y_tst$V1==1)]<- "WALKING"
Y_tst$V1[which(Y_tst$V1==2)]<- "WALKING_UPSTAIRS"
Y_tst$V1[which(Y_tst$V1==3)]<- "WALKING_DOWNSTAIRS"
Y_tst$V1[which(Y_tst$V1==4)]<- " SITTING"
Y_tst$V1[which(Y_tst$V1==5)]<- "STANDING"
Y_tst$V1[which(Y_tst$V1==6)]<- "LAYING"

Y_trn$V1[which(Y_trn$V1==1)]<- "WALKING" 
Y_trn$V1[which(Y_trn$V1==2)]<- "WALKING_UPSTAIRS"
Y_trn$V1[which(Y_trn$V1==3)]<- "WALKING_DOWNSTAIRS"
Y_trn$V1[which(Y_trn$V1==4)]<- " SITTING"
Y_trn$V1[which(Y_trn$V1==5)]<- "STANDING"
Y_trn$V1[which(Y_trn$V1==6)]<- "LAYING"
table(Y_trn)  

# This section merges the data from the 'training' and the 'test' files 
# to create a single dataset.
Subject <- rbind(subject_tst, subject_trn) 
Full_X <- rbind(X_tst,Xtrn) 
Activity <- rbind(Y_tst, Y_trn) 

# This section of the script will extract only the measurements on the mean 
# and the standard deviation for each measurement.

mean_std <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
Full_X <- Full_X[, mean_std]
names(Full_X) <- features[mean_std, 2]
names(Full_X)  <- gsub("\\(|\\)", "", names(Full_X)) 
names(Full_X)  <- tolower(names(Full_X))   

# This section labels the data set with the descriptive activity names 
activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Activity[,1] = activities[Activity[,1], 2] 
names(Activity) <- "Activity" 

names(Subject) <- "Subject"
Combo <- cbind(Full_X, Activity, Subject) 
write.table(Combo, "Cleansed Data Set.txt", sep="\t")  


# This section creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject.

uniqueSubjects = unique(Subject)[,1]
numSubjects = length(unique(Subject)[,1]) 
numActivities = length(activities[,1])
numCols = dim(Combo)[2] 
result = Combo[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- Combo[Combo$subject==s & Combo$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  } 
} 
write.table(result, "Tidy data set with averages.txt") 





