firstdataset <- function() {
    
    ###########################################################################
    # assignment step 1-4
    # call this function to return the first data.frame
    # 
    # use this sequence to call both functions at once (step 1-5)
    # x <- firstdataset(); y <- seconddataset(x)
    ###########################################################################
    
    # reading test data
    testdata <- read.table("X_test.txt")
    # reading train data
    traindata <- read.table("X_train.txt")
    
    # reading test subjects
    testsubject <- read.table("subject_test.txt")
    # reading train subjects
    trainsubject <- read.table("subject_train.txt")
    
    # reading descriptive activity names
    descrlabelnames <- read.table("activity_labels.txt")   
    # convert activity names to data.table
    dt_descrlabelnames <- data.table(descrlabelnames)
    setkey(dt_descrlabelnames,V1)
    
    # reading test label
    testlabel <- read.table("y_test.txt")
    # convert test label to data.table
    dt_testlabel <- data.table(testlabel)
    setkey(dt_testlabel,V1)       
    # join descriptive activity names
    dt_joinedtestlabel <- merge(dt_testlabel, dt_descrlabelnames)
    # extract descriptive activity names
    testlabel <- data.frame(dt_joinedtestlabel)
    testlabel <- data.frame(testlabel[,"V2"])    
    
    # reading train label 
    trainlabel <- read.table("y_train.txt")
    # convert train label to data.table
    dt_trainlabel <- data.table(trainlabel)
    setkey(dt_trainlabel,V1)       
    # join descriptive activity names
    dt_joinedtrainlabel <- merge(dt_trainlabel, dt_descrlabelnames)
    # extract descriptive activity names
    trainlabel <- data.frame(dt_joinedtrainlabel)
    trainlabel <- data.frame(trainlabel[,"V2"])  
    
    # add subject(person) and label(activity) to testdata 
    testdata2 <- cbind(testdata,testsubject,testlabel)
    names(testdata2) <- c("activity")
    
    # add subject(person) and label(activity) to traindata 
    traindata2 <- cbind(traindata,trainsubject,trainlabel)
    names(traindata2) <- c("activity")    
    
    # merging to total
    totaldata <- rbind(testdata2, traindata2)
    
    # add column names
    featuresdata <- read.table("features.txt")
    datacolumns <- as.vector(featuresdata[,2])
    datacolumns_sl <-  c(datacolumns,c("subject","activity"))
    names(totaldata) <- datacolumns_sl
    
    # extract values on the mean and standard deviation
    stdcols <-datacolumns[grep("*-std\\(\\)*", datacolumns)]
    meancols <-datacolumns[grep("*-mean\\(\\)-*", datacolumns)]
    stdmeancols <- c(stdcols, meancols,"subject","activity")
    stdmeandata <- totaldata[,stdmeancols]
    
    # replace columns names by descriptive names
    toreplacecols <- names(stdmeandata)
    
    toreplacecols <- gsub("\\QtBody\\E","time Body",toreplacecols)
    toreplacecols <- gsub("\\QfBody\\E","frequency Body",toreplacecols)
    toreplacecols <- gsub("\\Qmean\\E\\(\\)","mean value",toreplacecols)
    toreplacecols <- gsub("\\Qstd\\E\\(\\)","standard deviation",toreplacecols)
    toreplacecols <- gsub("\\QAcc\\E"," accelerometer ",toreplacecols)
    toreplacecols <- gsub("\\QGyro\\E"," gyroscope ",toreplacecols)    
       
    # put colnames on data.frame
    names(stdmeandata) <- toreplacecols
    
    # return data.frame
    result <- stdmeandata
}

seconddataset <- function(stdmeandata) {  
    
    ###########################################################################
    # assignment step 5
    # call this function with the result data.set of function firstdataset()
    # argument choice let users choose between grouping on subject or activity
    ###########################################################################
    
    # save the column names for reuse
    toreplacecols <- names(stdmeandata)
    
    # make a data frame for subject or activity 
    stdmeandata_subject <- stdmeandata[, toreplacecols[! toreplacecols %in% c("activity")]]
    
    stdmeandata_activity <- stdmeandata[, toreplacecols[! toreplacecols %in% c("subject")]] 
    
    ####### for subject
    # just use temporarily short column names for grouping on subject
    names(stdmeandata_subject) <- gsub(" ","",paste("k",1:67))
    
    aggr_subject <- aggregate(cbind(k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,
                                    k11,k12,k13,k14,k15,k16,k17,k18,k19,k20,
                                    k21,k22,k23,k24,k25,k26,k27,k28,k29,k30,
                                    k31,k32,k33,k34,k35,k36,k37,k38,k39,k40,
                                    k41,k42,k43,k44,k45,k46,k47,k48,k49,k50,
                                    k51,k52,k53,k54,k55,k56,k57,k58,k59,k60,
                                    k61,k62,k63,k64,k65,k66)~k67,stdmeandata_subject,mean)
    
    # put back descriptive long names
    names(aggr_subject) <- c(c("subject"),toreplacecols[! toreplacecols %in% c("subject","activity")])
    
    ####### for activity
    # just use temporarily short column names for grouping on subject
    names(stdmeandata_activity) <- gsub(" ","",paste("k",1:67))
    
    aggr_activity <- aggregate(cbind(k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,
                                     k11,k12,k13,k14,k15,k16,k17,k18,k19,k20,
                                     k21,k22,k23,k24,k25,k26,k27,k28,k29,k30,
                                     k31,k32,k33,k34,k35,k36,k37,k38,k39,k40,
                                     k41,k42,k43,k44,k45,k46,k47,k48,k49,k50,
                                     k51,k52,k53,k54,k55,k56,k57,k58,k59,k60,
                                     k61,k62,k63,k64,k65,k66)~k67,stdmeandata_activity,mean)
    
    # put back descriptive long names
    names(aggr_activity) <- c(c("activity"),toreplacecols[! toreplacecols %in% c("subject","activity")])
    
    write.table(aggr_subject, file="output.txt", row.name=FALSE)
    write.table(aggr_activity, file="output.txt", row.name=FALSE, append=TRUE)    
    
    # return the aggregated result
    result <- aggr_subject
}