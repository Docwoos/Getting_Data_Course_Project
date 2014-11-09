#For this Script to run appropriatley your working directory must include the Samsung wearable dataset

#Function to transform the Samsung Data into a New Tidy Datset
run_analysis<-function(){

#Load of the dplyr package for later use in the code
library(dplyr)

#Import the Activity & Feature Labels 
activity_labels <- read.table("./activity_labels.txt", quote="\"")
features <- read.table("features.txt", quote="\"")

#Create two more labels for the Activity and Subject variables
add1<-data.frame(V1="562",V2="Activity")
add2<-data.frame(V1="563",V2="Subject")

#Add the new variable labels and remove the uneeded data frames
features<-rbind(features,add1,add2)
rm(add1)
rm(add2)

#Import the Test Data
X_test <- read.table("./test/X_test.txt", quote="\"")
y_test <- read.table("./test/y_test.txt", quote="\"")
subject_test <- read.table("./test/subject_test.txt", quote="\"")

#Combine the Test, Activity and Subject data frames
X_test2<-data.frame(c(X_test,y_test,subject_test))
rm(X_test)
rm(y_test)
rm(subject_test)

#Import the Training Data
X_train <- read.table("./train/X_train.txt", quote="\"")
y_train <- read.table("./train/y_train.txt", quote="\"")
subject_train <- read.table("./train/subject_train.txt", quote="\"")

#Combine the Train, Activity and Subject data frames
X_train2<-data.frame(c(X_train,y_train,subject_train))
rm(X_train)
rm(y_train)
rm(subject_train)

#Combine the Test and Training data frames
AllData<-rbind(X_test2,X_train2)
rm(X_test2)
rm(X_train2)

#Apply the variable names
names(AllData)<-features$V2
rm(features)

#Create subsets of the combined data based on varaibles that represent means or Standard Deviations; plus Activity and Subject
a<-subset(AllData, select=c("Subject","Activity"))
b<-subset(AllData, select=names(AllData)[grep("*mean\\()", names(AllData))])
c<-subset(AllData, select=names(AllData)[grep("*std", names(AllData))])

#Combine subsetted data
AllData<-data.frame(cbind(a,b,c))
rm(a)
rm(b)
rm(c)

#Add descriptive Activty Labels
AllData2<-merge(AllData, activity_labels, by.x="Activity", by.y="V1")

#Remove the old Activity variable
AllData<-subset(AllData2, select=-Activity)

#Update the name of the new activity variable
names(AllData)[names(AllData)=="V2"]<-"Activity"

#Create a New Tidy Data set by summarizing by Activity & Subject, returning the mean of all other variables
newTidyData<-AllData %>% 
  group_by(Activity, Subject) %>% 
  summarise_each(funs(mean))
rm(AllData)
rm(AllData2)
rm(activity_labels)  

#Return the New Tidy Data set.
return(newTidyData)
}

