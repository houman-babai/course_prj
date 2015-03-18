library (dplyr)
library(RCurl)

# a few constants to help make the code readable
ACTIVITY_LABEL_PATH  <- 'activity_labels.txt';
FEATURE_PATH <- 'features.txt';
X_TEST_PATH <- 'X_test.txt';
Y_TEST_PATH <- 'Y_test.txt';
SUBJECT_TEST_PATH <- 'subject_test.txt';
X_TRAIN_PATH <-'X_train.txt'; 
Y_TRAIN_PATH <-'Y_train.txt'; 
SUBJECT_TRAIN_PATH <- 'subject_train.txt';
ZIP_DATA_SET_ROOT <- 'UCI HAR Dataset';
TEST_FOLDER <- 'test'
TRAIN_FOLDER <- 'train'

# this is a function that downloads the zip file into a temp location
# extracts the content and reports the location of the extracted file
# deprecated...for the assignment we just rely on the 'useZipFileInWorkingDirectory'
downloadAndPrepareInputFolder <- function () {
	tempFolder <- tempdir();
	zipFileName <- tempfile(tmpdir = tempFolder, fileext='.zip');	
	z <- getBinaryURL('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip')	;
	writeBin(z, zipFileName);
	unzip(zipFileName,exdir=tempFolder, overwrite=TRUE);
	file.path(tempFolder,ZIP_DATA_SET_ROOT);
}

# unzips the file in the working directory
# returns the location of the unziped file
useZipFileInWorkingDirectory <- function () {
	zipFileName <- file.path(getwd(),'getdata-projectfiles-UCI HAR Dataset.zip');
	print(c('trying to unzip', zipFileName));
	unzip(zipFileName, overwrite=TRUE);
	print('file unziped successfully.');
	ZIP_DATA_SET_ROOT;
}

# given a list of fatures it collects the ones that end with -mean() or -std()
# then it sorts them based on the column number
extractMeanStdCols  <- function (features) {
	mapping <- rbind(
		features[grep('*-mean\\(\\)',features$V2),], #mean measurements
		features[grep('*-std\\(\\)',features$V2),]); #std measurements
	
	#sort by column number
	mapping <- mapping [order(mapping[,1]),];
}

# give the feature data-frame and a list of columns 'codebook' it returns 
# a new dataframe composed of columns in the codebook
# it also names all the columns so that the new df has the same column-name as the original df
selectMeanStdCols <- function(df, codebook) {
	selectVars <- df[, codebook$V1];
	colnames(selectVars) <- codebook$V2;
	selectVars;
}

# given a list of activity-ids and an activity-id to lable mapping 'activityCodeBook'
# it replaces the id with the lable
activityLabel <- function(ids, activityCodeBook) {
	sapply(ids, function(x) {activityCodeBook[activityCodeBook$activityId==x,2]});	
}

# prepends the df frame with 2 new columns the first one is the subject-id
# the second one is the activity-id. Finally, it translates the activity-id 
# to activity-lable and returns the resulting df.
appendSubjectActivity <- function (df, activity, subject, activityCodeBook) {
	answer <- cbind(activity, df);
	answer <- cbind(subject, answer);
	colnames(answer)[1] <- 'subject';
	colnames(answer)[2] <- 'activity';
	answer <- answer %>%
		mutate(activity = activityLabel(activity, activityCodeBook)) 
	answer;					
}

# loads the feature data-frame from the varPath
# loads the activity data-frame from the activityPath
# loads the subject data-frame from the subjectPath
# it then extracts from the feature data-frame a new data-frame with columns whose column names
# end with mean() or std()
# Finally it stiches the 3 dataframes togethers by calling appendSubjectAndActivity
# The resulting dataframe has the subject, activity-lable and all columns that end with mean() and std()
loadDataSetByPath <- function (varPath, activityPath, subjectPath, activityCodeBook, meanAndStdCols) {
	print(c ('loading and preparing data set ', varPath));
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

#helper function translates a file-name to test folder path
testPath <- function(fileName) {
	file.path(root,file.path(TEST_FOLDER, fileName));
}

#helper function translates a file-name to train folder path
trainPath <- function(fileName) {
	file.path(root,file.path(TRAIN_FOLDER,fileName));
}

#helper function translates a file-name to the root dir path
filePath <- function(fileName) {
	file.path(root, fileName);
}

#loads the activity code book
#loads the feature code book
#extracts the mean/std cols from the feature code-book
#loads and configures the test-dataset by invoking the loadDataSetByPath
#loads and configures the train-dataset by invoking the loadDataSetByPath
#finally uses rbind to stich the two dfs together and returns the resulting df
loadDataSet <- function() {
	print('load the activity book');
	# read activity_labels.txt
	activityCodeBook <- read.table(filePath(ACTIVITY_LABEL_PATH), 
								   col.names=c('activityId', 'activity'));

	print('load the feature code book');
	# read feature.txt
	featureCodeBook <- read.table(filePath(FEATURE_PATH));

	print('extracting mean & std columns from the code-book');
	#extract only the mean & std features from the codebook
	meanAndStdCols <- extractMeanStdCols(featureCodeBook);

	dataSet <- rbind(loadDataSetByPath(testPath(X_TEST_PATH) , 
									   testPath(Y_TEST_PATH), 
									   testPath(SUBJECT_TEST_PATH), 
									   activityCodeBook, 
									   meanAndStdCols),
					 loadDataSetByPath(trainPath(X_TRAIN_PATH), 
					 				   trainPath(Y_TRAIN_PATH), 
					 				   trainPath(SUBJECT_TRAIN_PATH), 
					 				   activityCodeBook, 
					 				   meanAndStdCols));
	dataSet;					 
}			

#root is a global variable
root <- useZipFileInWorkingDirectory();

#load, configure & combine the test & train datasets
#THEN group them by subject and activity
#THEN take the average for every column that has the word 'mean' or 'std'
avg_dataSet <- loadDataSet() %>% 
				group_by(subject, activity) %>% 	 
				summarise_each(funs(mean), contains('mean'), contains('std'));

#write the output to output.txt in the working directory
print('writing result to output.txt');
write.table(avg_dataSet, file='output.txt',row.name=FALSE);				