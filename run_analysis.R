library(dplyr) #used in step 5
#	1.	Merges the training and the test sets to create one data set.

setwd("./UCI HAR Dataset")
features_txt<-read.table("features.txt")
dim(features_txt)
names(features_txt)
head(features_txt)
activity_labels_txt<-read.table("activity_labels.txt")
activity_labels_txt


fileListTest <- list.files(path="./test/Inertial Signals", pattern=".txt")



test_Inertial_Signals=lapply(paste("./test/Inertial Signals", fileListTest, sep="/"), read.table)

length(test_Inertial_Signals)

sapply(test_Inertial_Signals, dim)

names(test_Inertial_Signals)<-sub(".txt", "",fileListTest)



subject_test=read.table("./test/subject_test.txt")

dim(subject_test); unique(subject_test); dim(unique(subject_test)) 
# 2947*1 with 9 unique numbers
names(subject_test)<-"subject_id"

X_test=read.table("./test/X_test.txt")
dim(X_test); head(X_test);
names(X_test)<-features_txt$V2
# 2947*561 

Y_test=read.table("./test/Y_test.txt")
dim(Y_test); dim(unique(Y_test)); head(Y_test)
# 2947*1 with 6 unique numbers

Y_test = merge(Y_test, activity_labels_txt, by.X="V1", by.Y="V1")
# 2947*1 with 6 unique numbers with 6 unique activity names
names(Y_test)<-c("activity_num", "activity_name")
subject_act_test<-cbind(subject_test, Y_test, "Test")
names(subject_act_test)[4]<-"Set"

test_test<-lapply(test_Inertial_Signals, cbind, subject_act)
wholeset<-cbind(test_test[[1]], sub(".txt", "",fileListTest[1]))
names(wholeset)<-c(names(wholeset)[1:132], "measure")

for (i in 2:length(fileListTest)){
	temp<-cbind(test_test[[i]], sub(".txt", "",fileListTest[i]))
	names(temp)<-c(names(temp)[1:132], "measure")
	wholeset<-rbind(wholeset, temp)}
	
	fileListTrain <- list.files(path="./train/Inertial Signals", pattern=".txt")



train_Inertial_Signals=lapply(paste("./train/Inertial Signals", fileListTrain, sep="/"), read.table)

length(train_Inertial_Signals)

sapply(train_Inertial_Signals, dim)
#9 * 7352*128
names(train_Inertial_Signals)<-sub(".txt", "",fileListTrain)



subject_train=read.table("./train/subject_train.txt")

dim(subject_train); unique(subject_train); dim(unique(subject_train)) 
# 7352*1 with 9 unique numbers
names(subject_train)<-"subject_id"

X_train=read.table("./train/X_train.txt")
dim(X_train); head(X_train);
is.numeric(X_train)
# 7352*561 
names(X_train)<-gsub("()", "", gsub("-", "_", features_txt$V2))

Y_train=read.table("./train/Y_train.txt")
dim(Y_train); dim(unique(Y_train)); head(Y_train)
# 7352*1 with 6 unique numbers

Y_train = merge(Y_train, activity_labels_txt, by.X="V1", by.Y="V1")
# 7352*1 with 6 unique numbers with 6 unique activity names
names(Y_train)<-c("activity_num", "activity_name")
subject_act_train<-cbind(subject_train, Y_train, "train")
names(subject_act_train)[4]<-"Set"

train_train<-lapply(train_Inertial_Signals, cbind, subject_act)

for (i in 1:length(fileListTrain)){
	temp<-cbind(train_train[[i]], sub(".txt", "",fileListTrain[i]))
	names(temp)<-c(names(temp)[1:132], "measure")
	wholeset<-rbind(wholeset, temp)}

dim(wholeset)
head(wholeset)

X_Whole <- rbind(cbind(X_test, subject_act_test), cbind(X_train, subject_act_train))

temp_names<-gsub("-", "_", names(X_Whole))
names(X_Whole)<-gsub("[()]", "",temp_names)


#2.	Extracts only the measurements on the mean and standard deviation for each measurement. 

matches <- unique (grep("mean|std", names(X_Whole), value=TRUE))
extracts_set<-X_Whole[,names(X_Whole) %in% matches | names(X_Whole) %in%names(X_Whole)[c(562:565)]]
dim(extracts_set)

#4. Appropriately labels the data set with descriptive variable names. 
#done in the first step

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
names(extracts_set)
tempNames<-names(by_id_activity)[c(1:79)]

by_id_activity<-group_by(extracts_set, subject_id,activity_name) 

summary_id_act<-summarise_each(by_id_activity, funs(mean, sd), dim_names=1:79 )

names(summary_id_act)[3:160] <- c(paste("mean_", tempNames, sep=""),paste("std_", tempNames, sep=""))

getwd()
write.table(summary_id_act, "cleanData.txt", row.names=FALSE)
#list(quote(-c(subject_id, activity_num,activity_name,Set))))



##organizing for the code table

matches <- unique (grep("mean|std", features_txt$V2, value=TRUE))


myfeatures<-features_txt[features_txt$V2%in%matches,]
myfeatures
myfeatures$V2<-gsub("[()]", "",gsub("-", "_", myfeatures$V2))
myfeatures$seq<-seq(myfeatures$V2)
myfeatures<-myfeatures[,c(3,2)]
myfeatures$V3<-paste("mean",myfeatures$V2, sep="_")
temp<-data.frame(cbind(myfeatures$seq+79,myfeatures$V2, 
	paste("std",myfeatures$V2, sep="_")))
names(temp)<-c("seq", "V2", "V3")
myfeatures<-rbind(myfeatures, temp)

myfeatures<-myfeatures[,-2]
write.table(myfeatures, "code_list", row.names=FALSE, quote=FALSE)




	





