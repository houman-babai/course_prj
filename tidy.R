library (dplyr)
library(RCurl)

activityLabelPath  <- 'activity_labels.txt';
featuresPath <- 'features.txt';
xTestPath <- 'X_test.txt';
yTestPath <- 'Y_test.txt';
subjectTestPath <- 'subject_test.txt';
xTrainPath <-'X_train.txt'; 
yTrainPath <-'Y_train.txt'; 
subjectTrainPath <- 'subject_train.txt';
ZIP_DATA_SET_ROOT <- 'UCI HAR Dataset';
TEST_FOLDER <- 'test'
TRAIN_FOLDER <- 'train'


prepareInputFolder <- function () {
	tempFolder <- tempdir();
	zipFileName <- tempfile(tmpdir = tempFolder, fileext='.zip');	
	z <- getBinaryURL('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip')	;
	writeBin(z, zipFileName);
	unzip(zipFileName,exdir=tempFolder, overwrite=TRUE);
	file.path(tempFolder,ZIP_DATA_SET_ROOT);
}

extractMeanStdCols  <- function (features) {
	mapping <- rbind(
		features[grep('*-mean()',features$V2),], #mean measurements
		features[grep('*-std()',features$V2),]); #std measurements
	
	#sort by column number
	mapping <- mapping [order(mapping[,1]),];
}

selectMeanStdCols <- function(df, codebook) {
	selectVars <- df[, codebook$V1];
	colnames(selectVars) <- codebook$V2;
	selectVars;
}

activityLabel <- function(ids) {
	sapply(ids, function(x) {activityCodeBook[activityCodeBook$activityId==x,2]});	
}

appendSubjectActivity <- function (df, activity, subject, activityCodeBook) {
	answer <- cbind(activity, df);
	answer <- cbind(subject, answer);
	colnames(answer)[1] <- 'subject';
	colnames(answer)[2] <- 'activity';
	answer <- answer %>%
		mutate(activity = activityLabel(activity)) 
	answer;					
}

# read testDataSet
loadDataSetByPath <- function (varPath, activityPath, subjectPath, activityCodeBook, meanAndStdCols) {
	vars <- read.table(varPath);
	activity <- read.table(activityPath);
	subject <- read.table(subjectPath);

	dataSet <- selectMeanStdCols(vars, meanAndStdCols);
	dataSet <- appendSubjectActivity(dataSet, 
					activity, 
					subject, 
					activityCodeBook);
	dataSet;
}

testPath <- function(fileName) {
	file.path(root,file.path(TEST_FOLDER, fileName));
}

trainPath <- function(fileName) {
	file.path(root,file.path(TRAIN_FOLDER,fileName));
}

filePath <- function(fileName) {
	file.path(root, fileName);
}

loadDataSet <- function() {
	# read activity_labels.txt
	activityCodeBook <- read.table(filePath(activityLabelPath), 
								   col.names=c('activityId', 'activity'));

	# read feature.txt
	featureCodeBook <- read.table(filePath(featuresPath));

	#extract only the mean & std features from the codebook
	meanAndStdCols <- extractMeanStdCols(featureCodeBook);

	dataSet <- rbind(loadDataSetByPath(testPath(xTestPath) , 
									   testPath(yTestPath), 
									   testPath(subjectTestPath), 
									   activityCodeBook, 
									   meanAndStdCols),
					 loadDataSetByPath(trainPath(xTrainPath), 
					 				   trainPath(yTrainPath), 
					 				   trainPath(subjectTrainPath), 
					 				   activityCodeBook, 
					 				   meanAndStdCols));
	dataSet;					 
}			

main <- function() {
dataSet <- loadDataSet();

avg_dataSet <- dataSet %>% 
				group_by(subject, activity) %>% 	 
				summarise_each(funs(mean), contains('mean'), contains('std'));
}

#root is a global variable
root <- prepareInputFolder();
main();