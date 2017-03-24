# Parse failure record from yemao. 
# compute the faction of failed servers who have information of disk drive
# Date: 2016-08-25
# Author: yorkyi

rm(list = ls())
source('head.R')
load(file.path(dir_data,'failRecord_1407-1506.Rda'))

####################################
# S1. parse event description
lineIdx <- grep('sd[a-z]{1}',failRecord$faultInfo)
startIdx <- regexpr('sd[a-z]{1}',failRecord$faultInfo[lineIdx])
driveName <- regmatches(failRecord$faultInfo[lineIdx],startIdx)

# failRecord with disk info
failRecordWithDev <- factorX(failRecord[lineIdx,])
failRecordWithDev$faultDev <- driveName
lt <- data.frame(ch = letters,num = 1:26)
failRecordWithDev$faultDiskId <- lt$num[match(gsub('sd','',failRecordWithDev$faultDev),lt$ch)]
# failRecord without disk info.
failRecordWithoutDev <- factorX(failRecord[-lineIdx,])
# NOTE: a and b have part of intersect

# number of failure for each svr_id
numFailure <- melt(table(failRecord$svr_id))
names(numFailure) <- c('svr_id','numFailure')
numFailure <- numFailure[order(numFailure$svr_id),]

# S2. Save
failRecordWithoutDev <- factorX(failRecordWithoutDev)
failRecordWithDev <- factorX(failRecordWithDev)
save(numFailure,failRecordWithDev,failRecordWithoutDev,file = file.path(dir_data,'failRecord_diskParse.Rda'))

# S3. show first 10 words of event description
# first5word <- substring(failRecord$faultInfo,1,10)
# table.f5w <- tableX(first5word)

# histgram of time and number of failure
# createtimeCut <- melt(table(cut.POSIXt(failRecord$f_time,breaks = 'month')))
# names(createtimeCut) <- c('month','number')
# ggplot(createtimeCut,aes(x = month,y = number)) + geom_bar(stat = 'identity')
