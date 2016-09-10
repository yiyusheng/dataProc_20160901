# Parse failure record from yemao. compute the faction of failed servers who have information of disk drive
# Date: 2016-08-25
# Author: yorkyi

rm(list = ls())
source('head.R')
load(file.path(dir_data,'failRecord.Rda'))

####################################
# S1. parse event description
lineIdx <- grep('sd[a-z]{1}',failRecordA$eventdescription)
startIdx <- regexpr('sd[a-z]{1}',failRecordA$eventdescription[lineIdx])
driveName <- regmatches(failRecordA$eventdescription[lineIdx],startIdx)

a <- factorX(failRecordA[lineIdx,])
b <- factorX(failRecordA[-lineIdx,])
c <- factorX(subset(b,serverassetid %in% a$serverassetid))

# show first 10 words of event description
first5word <- substring(failRecordA$eventdescription,1,10)
table.f5w <- tableX(first5word)

# histgram of time and number of failure
createtimeCut <- melt(table(cut.POSIXt(failRecordA$f_time,breaks = 'month')))
names(createtimeCut) <- c('month','number')
ggplot(createtimeCut,aes(x = month,y = number)) + geom_bar(stat = 'identity')
