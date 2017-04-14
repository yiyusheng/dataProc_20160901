# load failure record from yemao. It contains all failure in 2014
# Date: 2016-09-12
# Author: yorkyi
# for 硬盘故障单140701_150701.txt
# we first use regexp of notepad++ to remove \t ((\t){2,100})
rm(list = ls())
source('head.R')
require(data.table)

####################################
#S1.recovery and load table
failRecord <- fread(file.path(dir_data,'files','硬盘故障单140701_150630.txt'),
                    sep = '\t',encoding = 'UTF-8',data.table = F)
failRecord$f_time <- as.POSIXct(failRecord$createtime,tz = 'UTC')
names(failRecord) <- c('svr_id','hday','faultInfo','faultType','createtime','f_time')
failRecord$svr_id <- factor(failRecord$svr_id)
failRecord <- subset(failRecord,1 == 1,c('svr_id','faultInfo','faultType','f_time'))
attr(failRecord,'.internal.selfref') <- NULL

#S3. deduplicate for failures take place in 3 days
source(file.path(dir_c,'dataLoadforDiskAnalysis','loadFunc.R'))
failRecord <- dedupTime(failRecord,3,'svr_id')
failRecord$svr_id <- factor(failRecord$svr_id)

#S3.save
attr(failRecord,'.internal.selfref') <- NULL
names(failRecord)[names(failRecord) == 'svr_id'] <- 'svrid'
save(failRecord,file = file.path(dir_data,'failRecord_1407-1506.Rda'))
