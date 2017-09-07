if(!require(dplyr)){
    install.packages("dplyr")
}

if(!require(tidyr)){
    install.packages("tidyr")
}

library(dplyr)

library(tidyr)

URL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(URL, "UCI.zip", method = "libcurl")

unzip("UCI.zip")

test.raw <- read.table("UCI HAR Dataset/test/X_test.txt")

test.subject<- read.table("UCI HAR Dataset/test/subject_test.txt")

test.activity<- read.table("UCI HAR Dataset/test/Y_test.txt")

train.raw <- read.table("UCI HAR Dataset/train/X_train.txt") 

train.subject<- read.table("UCI HAR Dataset/train/subject_train.txt")

train.activity<- read.table("UCI HAR Dataset/train/Y_train.txt")

activity.labels<- read.table("UCI HAR Dataset/activity_labels.txt")

var.names<- read.table("UCI HAR Dataset/features.txt")

data.raw<-rbind(test.raw, train.raw)

names(data.raw)<-tolower(var.names[,2])  

names(data.raw)<-gsub("-", " ", names(data.raw)) 

list1<-grepl("mean", names(data.raw))

list2<-grepl("std", names(data.raw))

list3<-grepl("freq", names(data.raw))

list4<-grepl("angle", names(data.raw))

filter.list<- (list1 | list2) & (!list3 & !list4) 

data.raw1<-data.raw[,filter.list] 

subject.activity<- rbind(cbind(test.subject, test.activity), cbind(train.subject, train.activity)) 

subject.activity[,2]<- as.factor(subject.activity[,2])

levels(subject.activity[,2])<- c("walking", "walking upstairs", "walking downstairs", "sitting", "standing", "laying") 

names(subject.activity)<-c("subject", "activity")

data.raw2<- cbind(subject.activity, data.raw1) 

data.raw2 %>%
    
    gather(key = variable, value = result, -subject, -activity, factor_key = TRUE) %>%
 
    separate(variable, c("variable", "statistic", "vector"), sep= " ")->tidy   

tidy$statistic<-gsub('.{2}$', '',tidy$statistic) 

tidy<-group_by(tidy, subject, activity, variable, statistic, vector) 

final<- summarise(tidy, meanvalue=mean(result)) 

write.table(final, file="Tidy Data.txt", row.names = FALSE) 
