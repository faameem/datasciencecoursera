library(dplyr)

###################################################
## step 0: read all data files
###################################################

# root present working directory
rpwd<-getwd()
# rpwd

# read features data file
setwd(rpwd)
features<-read.table("features.txt")
#str(features)

# read training data files
setwd(paste0(rpwd,"/train"))
x.train<-read.table("X_train.txt")
#str(x.train)
subject.train<-read.table("subject_train.txt")
names(subject.train)<-"subject"
#str(subject.train)
y.train<-read.table("y_train.txt")
names(y.train)<-"activity"
#str(y.train)

# read test data files
setwd(paste0(rpwd,"/test"))
x.test<-read.table("X_test.txt")
#str(x.test)
subject.test<-read.table("subject_test.txt")
names(subject.test)<-"subject"
#str(subject.test)
y.test<-read.table("y_test.txt")
names(y.test)<-"activity"
#str(y.test)

###################################################
## step 1: Merge the training and the test sets to create one data set.
###################################################

# merge training data sets
subject.y.train<-bind_cols(subject.train,y.train)
#str(subject.y.train)
subject.y.x.train<-bind_cols(subject.y.train,x.train)
#str(subject.y.x.train)

# merge test data sets
subject.y.test<-bind_cols(subject.test,y.test)
#str(subject.y.test)
subject.y.x.test<-bind_cols(subject.y.test,x.test)
#str(subject.y.x.test)

# merge training and test data sets
data.merged<-bind_rows(subject.y.x.train,subject.y.x.test)
#str(data.merged)

###################################################
## step 2: Extract only the measurements on the mean and standard deviation for each measurement.
## step 3: Use descriptive activity names to name the activities in the data set
## step 4: Appropriately labels the data set with descriptive variable names.
###################################################

# step 2 & step 3
names(data.merged)<-c("subject","activity",as.character(features$V2))
data.merged.nonduplicate<-data.merged[ , !duplicated(colnames(data.merged))]
data<-data.merged.nonduplicate %>%
    select(subject,activity,matches("(mean|std)\\(.*\\)")) %>%
    mutate(activity = ifelse(activity==1,"WALKING"
                      ,ifelse(activity==2,"WALKING_UPSTAIRS"
                      ,ifelse(activity==3,"WALKING_DOWNSTAIRS"
                      ,ifelse(activity==4,"SITTING"
                      ,ifelse(activity==5,"STANDING"
                      ,ifelse(activity==6,"LAYING","")))))))

# step 4
names(data)<-gsub("\\(.*\\)","",names(data))
names(data)<-gsub("-","",names(data))
names(data)<-tolower(names(data))
#names(data)

#str(data)
#slice<-data[(data$activity=="LAYING" & data$subject==1),1:3]
#mean(slice$tbodyaccmeanx)

###################################################
## step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
###################################################

data.tidy<-data %>% 
            group_by(activity,subject) %>% 
            summarise_each(funs(mean(.,na.rm=TRUE)))
#str(data.tidy)
setwd(rpwd)
write.table(data.tidy, file="data-tidy.txt",row.names = FALSE,quote = FALSE, col.names = TRUE)

