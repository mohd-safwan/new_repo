# Script for Course #3 Project
#
# You should create one R script called run_analysis.R that does the following.
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each
#    variable for each activity and each subject.

# load the packages
library(dplyr)

# Create data folder
if(!file.exists("data")) {
  dir.create("data")
}
codebook <- function(...){
  cat(..., "\n",file=targetCodebookFilePath,append=TRUE, sep="")
}

#codebook
targetCodebookFilePath <- "./CodeBook.md"
file.remove(targetCodebookFilePath)
codebook("# Code Book")
codebook("generated ",as.character(Sys.time())," during sourcing of `run_analysis.R`")
codebook("")  

if(!exists("keyColumns")){
  keyColumns <<- c()
}


if(!exists("featureColumns")){
  featureColumns <<- c()
}

codebook("## Actions performed on data:")

# create path
codebook("* create data dir `",dataPath,"`")
if(!file.exists(dataPath)){
  log("create path: `",dataPath,"`")
  dir.create(dataPath)
}


# download .zip file if not exists
if(debug){file.remove(filePath)}

codebook("* downloading zip file: [",fileUrl,"](",fileUrl,") to `",dataPath,"`")
if(!file.exists(targetZipFilePath)){
  log("downloading zip file: `",fileUrl,"`")
  binaryData <- getBinaryURL(fileUrl, ssl.verifypeer=FALSE, followlocation=TRUE) 
  log("writing zip file: `",targetZipFilePath,"`")
  fileHandle <- file(targetZipFilePath, open="wb")
  writeBin(binaryData, fileHandle)  
  close(fileHandle)  
  rm(binaryData)
}else{
  log("zip file already exists: `",targetZipFilePath,"`")
}

# unzip if not already exists
extractedZipPath <- file.path(dataPath, zipDir)
codebook("* extracting zip file: `",targetZipFilePath,"` to `",extractedZipPath,"`")
if(!file.exists(extractedZipPath) || debug){
  log("unzip file: `",targetZipFilePath, "` to `",dataPath,"`")
  unzip(targetZipFilePath, exdir=dataPath, overwrite=TRUE)
}else{
  log("zip file already extracted to: `",extractedZipPath,"`")
}

dirList <- list.files(extractedZipPath, recursive=TRUE)


# load all train & test .txt files into memory: ignoring the 'Inertial Signals' folders
sanitizedDirList <- dirList[!grepl("Inertial", dirList) & grepl("test|train", dirList)]

codebook("* merging all *_test.txt and *_train.txt files into one dataset: `mergedData`")
if(!exists("mergedData") || debug){
  log("load .txt files:")
  for(dataFile in sanitizedDirList){
    # generate parameters based on filesc
    paramName <- paste0("data_", tolower(sub("^.*/([^\\.]+).*$","\\1",dataFile, perl=TRUE)))
    txtFile <- file.path(extractedZipPath, dataFile)
    log("\t- `",txtFile, "` into var `", paramName,"`")
    tableData <- read.table(txtFile)  
    assign(paramName, tableData)
    rm(tableData)
  }
  


# get the data, load it into data.frames
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl,destfile = "./data/rundata.zip")
unzip("./data/rundata.zip")
fileurl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.names"
download.file(fileurl,destfile = "./data/rundata.names")

col_names <- read.table("./UCI HAR Dataset/features.txt") # the variable names

X_training <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = col_names[,2]) # the values from the activity
y_training <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = as.factor(c("activity"))) # the activity
subject_training <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = as.factor(c("subject"))) # the subject

#Activites: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
X_testing <- read.table("./UCI HAR Dataset/test/X_test.txt",  col.names = col_names[,2])  # the values from the activity
y_testing <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = c("activity")) 
subject_testing <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = c("subject")) # the subject

# ---------------------------------------------
# 1. Merges the training and the test sets to create one data set.
TESTing <- cbind(X_testing,y_testing,subject_testing)
TRAINing <- cbind(X_training,y_training,subject_training)
all <- rbind(TESTing,TRAINing)

# ---------------------------------------------
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
Mean_Std <- select(all, contains("mean"),contains("std"),activity,subject)

# ---------------------------------------------
# 3. Uses descriptive activity names to name the activities in the data set
xlate_act <- function(x)
{
  activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
  activities[x,2]
}
mean_std <- mutate(mean_std, activity = xlate_act(mean_std$activity))

# ---------------------------------------------
# 4. Appropriately labels the data set with descriptive variable names.

names(mean_std) <- gsub("\\.","",tolower(names(mean_std)))


# ---------------------------------------------
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each
#    variable for each activity and each subject.
mean_std_group <- group_by(mean_std,activity,subject)
tidy_data <- summarise_all(mean_std_group,mean)
View(tidy_data)
write.table(tidy_data,"./data/tidy_data.txt",quote = FALSE, row.names = FALSE)


# To view the tidy_data set provided you can run the follow commands
# data <- read.table("./data/tidy_data.txt", header = TRUE) 
# View(data)
