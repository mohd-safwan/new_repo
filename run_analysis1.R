
log <- function(...) {
  cat("[run_analysis.R] ", ..., "\n", sep="")
}

codebook <- function(...){
  cat(..., "\n",file=targetCodebookFilePath,append=TRUE, sep="")
}

vars <- ls()
vars <- vars[which(vars!="mergedData")]
#rm(list = vars)
debug <- FALSE

log("DEBUGGING: ",debug)
log("workingDir: `",getwd(),"`")


if(debug && exists("mergedData")){
  rm(mergedData)
}

# libs
library(RCurl)
library(reshape2)


# data
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipDir <- "UCI HAR Dataset"
TargetZipFile <- "data.zip"
TargetResultFile <- "tidy_data.txt"
dataPath <- "./data"
TargetZipFilePath <- file.path(dataPath,TargetZipFile)
TargetResultFilePath <- file.path(dataPath,TargetResultFile)

#codebook
TargetCODEBOOKFilePath <- "./CodeBook.md"
file.remove(TargetCODEBOOKFilePath)
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
if(!file.exists(TargetZipFilePath)){
  log("downloading zip file: `",fileUrl,"`")
  BinaryData <- getBinaryURL(fileUrl, ssl.verifypeer=FALSE, followlocation=TRUE) 
  log("writing zip file: `",TargetZipFilePath,"`")
  FileHandle <- file(TargetZipFilePath, open="wb")
  writeBin(BinaryData, FileHandle)  
  close(FileHandle)  
  rm(BinaryData)
}else{
  log("zip file already exists: `",TargetZipFilePath,"`")
}

# unzip if not already exists
ExtractedZipPath <- file.path(dataPath, zipDir)
codebook("* extracting zip file: `",TargetZipFilePath,"` to `",ExtractedZipPath,"`")
if(!file.exists(ExtractedZipPath) || debug){
  log("unzip file: `",TargetZipFilePath, "` to `",dataPath,"`")
  unzip(TargetZipFilePath, exdir=dataPath, overwrite=TRUE)
}else{
  log("zip file already extracted to: `",ExtractedZipPath,"`")
}

dirList <- list.files(ExtractedZipPath, recursive=TRUE)


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
  TABLEData <- read.table(txtFile)  
    assign(paramName, TABLEData)
    rm(TABLEData)
  }
  
  
  
  # should have these variables (check via ls()):
  # data_subject_test
  # data_subject_train
  # data_x_test
  # data_x_train
  # data_y_test
  # data_y_train
  
  # Assignment/Project:
  # 1. Merges the training and the test sets to create one data set.
  log("[#1] Merges the training and the test sets to create one data set.")
  
  # combine test & training data as rows into 3 data sets
  log("\t- combine subject test & train")
  DATA_subject <- rbind(data_subject_test, data_subject_train)
  names(DATA_subject) <- c("subject")
  keyColumns <- union(keyColumns, names(DATA_subject))
  rm(data_subject_test)
  rm(data_subject_train)
  
  log("\t- combine activity test & train")
 DATA_y <- rbind(data_y_test, data_y_train)
  names(DATA_y) <- c("activity_num")
  keyColumns <- union(keyColumns, names(DATA_y))
  rm(data_y_test)
  rm(data_y_train)
  
  log("\t- combine feature data test & train")
  DATA_x <- rbind(data_x_test, data_x_train)
  featuresFile <- file.path(extractedZipPath,"features.txt")
  featureData <- read.table(featuresFile)
  featureColumns <- featureData$V2
  names(DATA_x) <- featureColumns
  rm(data_x_test)
  rm(data_x_train)
  rm(featureData)
  
  
  
  # combine the 3 data sets as colums into mergedData
  log("\t- combine subject, activity & feature data")
  MERGEDData <- cbind(data_subject, data_y)
  MERGEDData <- cbind(mergedData, data_x)
  rm(DATA_subject)
  rm(DATA_x)
  rm(DATA_y)
  
  log("\t - `mergedData` loaded in memory: ", nrow(MERGEDData)," x ",ncol(MERGEDData))
}else{
  log("[#1] Merges the training and the test sets to create one data set.")
  log("\t - `mergedData` already loaded in memory: ", nrow(MERGEDData)," x ",ncol(MERGEDData))
}
codebook("* `mergedData` loaded in memory, dimensions: ", nrow(MERGEDData)," x ",ncol(MERGEDData))



# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

log("[#2] Extracts only the measurements on the mean and standard deviation for each measurement. ")
MeanStdFeatureColumns <- featureColumns[grepl("(mean|std)\\(\\)",featureColumns)]
subSetColumns <- union(keyColumns, MeanStdFeatureColumns)
subSetMergedData <- mergedData[,subSetColumns]
codebook("* subsetted `mergedData` into `subSetMergedData` keeping only the key columns and features containing `std` or `mean`, dimensions : ", nrow(subSetMergedData)," x ",ncol(subSetMergedData))


# 3. Uses descriptive activity names to name the activities in the data set
log("[#3] Uses descriptive activity names to name the activities in the data set")
activitiesFile <- file.path(extractedZipPath,"activity_labels.txt")
activitiesData <- read.table(activitiesFile)
names(activitiesData) <- c("activity_num", "activity_name")

subSetMergedData <- merge(subSetMergedData, activitiesData, by="activity_num", all.x=TRUE)
subSetKeyColumns <- union(keyColumns, c("activity_name"))
codebook("* merged `",activitiesFile,"` contents with correct `activity_num` column, effectivly appending `activity_name` to `subSetMergedData`, dimensions : ", nrow(subSetMergedData)," x ",ncol(subSetMergedData))

# 4. Appropriately labels the data set with descriptive variable names. 
log("[#4] Appropriately labels the data set with descriptive variable names. ")
ReshapedData <- melt(subSetMergedData, subSetKeyColumns)
codebook("* melt `subSetMergedData` into `reshapedData`, based on key columns, dimensions : ", nrow(ReshapedData)," x ",ncol(ReshapedData))

# split the variable into parts (list of char vectors) and reshape it into a data frame and add it to reshapedData
variableList <- strsplit(gsub("^((f|t)(Body|BodyBody|Gravity)(Gyro|Acc|Body)[\\-]*(Jerk)?(Mag)?[\\-]*(mean|std)[\\(\\)\\-]*(X|Y|Z)?)", "\\2|\\3|\\4|\\5|\\6|\\7|\\8|\\1", reshapedData$variable), "\\|")
nrows <- length(variableList)
ncols <- length(unlist(variableList[1]))
variableUnlist <- unlist(variableList)
variableMatrix <- matrix(variableUnlist, nrow=nrows, ncol=ncols, byrow=TRUE)
variableData <- as.data.frame(variableMatrix)
variableData$V8 <- NULL
names(variableData) <- c("dimension", "source","type","jerk", "magnitude","method","axis")
reshapedData <- cbind(ReshapedData, variableData)
rm(variableList)
rm(variableUnlist)
rm(variableMatrix)
rm(variableData)
codebook("* split feature column `variable` into 7 seperate colums (for each sub feature), and added it to `reshapedData`, dimensions : ", nrow(reshapedData)," x ",ncol(reshapedData))


resultData <- ReshapedData
rm(rReshapedData)
codebook("* renamed `reshapedData` to **`resultData`**")
log("variable `resultData` available for use : ", nrow(resultData)," x ",ncol(resultData))


# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
log("[#5] Appropriately labels the data set with descriptive variable names. ")
TidyData <- dcast(resultData, activity_name + subject ~ variable, mean)
log("variable `tidyData` available for use ", nrow(TidyData)," x ",ncol(TidyData))
codebook("* cast `resultData` into **`tidyData`** with the average of each variable for each activity and each subject dimensions :", nrow(TidyData)," x ",ncol(TidyData))

log("Writing `tidyData` to `",targetResultFilePath,"`")
codebook("* write `tidyData` to file  `",targetResultFilePath,"`")
write.table(TidyData, "targetResultFilePath.txt", row.names = FALSE, quote = FALSE,col.names = TRUE)



# writing variable properties
codebook("") 
codebook("## `resultData` variable\n")
codebook("### key columns\n")
codebook("Variable name       | Description")
codebook("--------------------|------------")
codebook("`subject`           | ID of subject, int (1-30)")
codebook("`activity_num`      | ID of activity, int (1-6)")
codebook("`activity_name`     | Label of activity, Factor w/ 6 levels")

codebook("### non-key columns\n")
codebook("Variable name       | Description")
codebook("--------------------|------------")
codebook("`variable`          | comlete name of the feature, Factor w/ 66 levels (eg. tBodyAcc-mean()-X) ")
codebook("`value`             | the actual value, num (range: -1:1)")
codebook("`dimension`         | dimension of measurement, Factor w/ 2 levels: `t` (Time) or `f` (Frequency)")
codebook("`source`            | source of measurement, Factor w/ 3 levels: `Body`,`BodyBody` or `Gravity`")
codebook("`type`              | type of measurement, Factor w/ 2 levels: `Acc` (accelerometer) or `Gyro` (gyroscope)")
codebook("`jerk`              | is 'Jerk' signal , Factor w/ 2 levels:  `Jerk` or `` (non Jerk)")
codebook("`magnitude`         | is 'Magnitude' value , Factor w/ 2 levels:  `Mag` or `` (non Mag)")
codebook("`method`            | result from method , Factor w/ 2 levels:  `mean` (average) or `std` (standard deviation)")
codebook("`axis`              | FFT exrapolated to axis , Factor w/ 2 levels:  `` (no FFT-axis) or `X`, `Y` or `Z`")

codebook("") 
codebook("## `tidyData` variable\n")
codebook("### key columns\n")
codebook("Variable name       | Description")
codebook("--------------------|------------")
codebook("`activity_name`     | Label of activity, Factor w/ 6 levels")
codebook("`subject`           | ID of subject, int (1-30)")


codebook("### non-key columns\n")
codebook("Variable name       | Description")
codebook("--------------------|------------")
tidyDataCols <- names(tidyData)[3:68]
for(tdc in tidyDataCols){
  codebook("`",tdc,"`   | the average value for this feature, num (range: -1:1)")
}
