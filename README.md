# course_prj

## required packages
The following packages are required:
    * dplyr
    * RCurl

## executing the code
To execute this project, you need to:

    1. download the run_analysis.R file to your working folder.  
    2. download the following zip file: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) to your working folder.  
    3. Then you can execute source('run_analysis.R').
    4. verify the output.txt in your working folder.
    
You may look at the code book to see the schema for the output file    
    
## how it works
    * It unzips the zip file to the working directory
    * It loads the feature-code book.
    * It loads the activity-code book.
    * extracts only the elements that have the std() or mean() in thier name
    * loads the actual feature file from the test folder
    * adds the subject and activity columns to it and relables the activity id with the actual lable
    * it repeats the same thing for the training data
    * appends the two datasets togethers
    * then groups the data by subject & activity and calculates the mean for each column.
    * the code is modular via functions and fully documented.