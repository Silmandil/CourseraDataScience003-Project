library("plyr")

#This script will:
#1. Merge the training and the test sets to create one data set.
#2. Extract only the measurements on the mean and standard deviation for each measurement. 
#3. Use descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive activity names. 
#5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 


#Don't mind this function, purely for debugging with a smaller set.
load_test = function(directory = .)
{
    features <- read.table(paste(directory, "features.txt", sep='/'))
    named_features <- as.character(features[,2])
    xtest <- read.table("test/X_test.txt", col.names=named_features)
    xtestactivity <- read.table("test/y_test.txt")
    xtestprime <- cbind(xtestactivity[[1]], xtest)
    names(xtestprime)[[1]] <- "activity_num"
    
    xtestsubject <- read.table("test/subject_test.txt")
    xtestprime <- cbind(xtestsubject[[1]], xtestprime)
    names(xtestprime)[[1]] <- "subject"
    xtestprime
}

# This function creates a data table.
#1. Merge the training and the test sets to create one data set.
merge_test_train_sets = function(directory = .) 
{
    features <- read.table(paste(directory, "features.txt", sep='/'))
    named_features <- as.character(features[,2])
    xtest <- read.table(paste(directory, "test/X_test.txt", sep='/'), col.names=named_features)
    xtestactivity <- read.table(paste(directory, "test/y_test.txt", sep='/'))
    xtestprime <- cbind(xtestactivity[[1]], xtest)
    names(xtestprime)[[1]] <- "activity_num"
    
    xtestsubject <- read.table(paste(directory, "test/subject_test.txt", sep='/'))
    xtestprime <- cbind(xtestsubject[[1]], xtestprime)
    names(xtestprime)[[1]] <- "subject"
    
    xtrain <- read.table(paste(directory, "train/X_train.txt", sep='/'), col.names=named_features)
    xtrainactivity <- read.table(paste(directory, "train/y_train.txt", sep='/'))
    xtrainprime <- cbind(xtrainactivity[[1]], xtrain)
    names(xtrainprime)[[1]] <- "activity_num"

    xtrainsubject <- read.table(paste(directory, "train/subject_train.txt", sep='/'))
    xtrainprime <- cbind(xtrainsubject[[1]], xtrainprime)
    names(xtrainprime)[[1]] <- "subject"
    
    xtotal <- rbind(xtestprime,xtrainprime)


    xtotal
}

# This function modifies an existing data table.
#2. Extract only the measurements on the mean and standard deviation for each measurement. 
extract_mean_sd = function(dt)
{
    #grep(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE,
    #     fixed = FALSE, useBytes = FALSE, invert = FALSE)
    
    #We only want the means and the standard deviations. Mean Frequency is 
    #not terribly interesting since it is a specific value, not the average of a 
    #discrete item. The fact that there is no standard deviation associated
    #confirms this as the right choice. Excluding meanFreq is as simple as
    #discarding a match if the next character after mean is an 'F'.
    #Case sensitive has the advantage of excluding the angles at
    #the end of the data set.
    #And of course it'd be foolish to throw away our information on the subject.
    dt[grep("subject|activity_num|std|mean[^F]", names(dt))] 
                                
    
}

# This function modifies an existing data table.
#3. Use descriptive activity names to name the activities in the data set
name_activities = function(dt, directory = .)
{
    activity_labels <- read.table(paste(directory, "activity_labels.txt", sep='/'))
    names(activity_labels) <- c("activity_num", "activity")
    nt <- merge(activity_labels, dt)    #join them based on the common column
    nt$activity_num <- NULL         #drop the id column since we have names now.
    nt
        
}

# This function modifies an existing data table.
#4. Appropriately labels the data set with descriptive activity names. 
# OR make the features of the data pretty and better named.
make_features_pretty = function(dt)
{
    names(dt) = tolower(names(dt))    # Make everything nice and lowercase.
    names(dt) = gsub("\\.", "", names(dt))  # Get rid of stray periods as well.
    dt
}

# This function returns a new data table.
#5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
create_tidy_summary = function(dt)
{
#    split_pt <- split(dt, dt$activity, drop = TRUE)
#    dsplit <- lapply(split_pt, function(x) split(x, x["subject"]))
    
#    overall_means <- lapply(split_pt, function(x) sapply(x, mean))
#    overall_means <- lapply(dsplit, function(x) lapply(x, function(x) sapply(x, mean)))

    pry1 <- ddply(dt, .(subject, activity), function(x) numcolwise(mean)(x)) # oh right, that was easy.

    final_table <- pry1 #tbd
    write.table(final_table, "./Summary.txt", sep="\t")
}

mydir <- if(!any(grepl("test", dir()))) "./UCI HAR dataset" else "."

xt <- merge_test_train_sets(mydir)
mt <- extract_mean_sd(xt)
nt <- name_activities(mt, mydir)
pt <- make_features_pretty(nt)
create_tidy_summary(pt)

