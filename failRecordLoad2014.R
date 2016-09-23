e# load failure record from yemao. It contains all failure in 2014
# Date: 2016-08-25
# Author: yorkyi
# for 硬盘故障单140101_141231A.txt, we first use regexp of notepad++ to remove \t ((\t){2,100})
rm(list = ls())
source('head.R')
require(data.table)

####################################
#S1. recovery and load table A
failRecordA <- fread(file.path(dir_data,'硬盘故障单140101_141231A.txt'),sep = '~',header = F,encoding = 'UTF-8')
tenc_fail_record_parse <- function(str){
  s <- strsplit(str,split = '\t')[[1]]
  len <- length(s)
  if (len != 10){
    s1 <- character(10)
    s1[1:5] <- s[1:5]
    s1[6] <- paste(s[6:(len-4)],sep='',collapse = '')
    s1[7:10] <- s[(len-3):len]
    s <- s1
  }
  s
}
a <- sapply(1:nrow(failRecordA),function(i)tenc_fail_record_parse(failRecordA[i]$V1))
failRecordA <- data.frame(t(a[,2:nrow(failRecordA)]))
names(failRecordA) <- a[,1]
failRecordA$f_time <- as.POSIXct(fct2ori(failRecordA$createtime),tz = 'UTC')

#S2.recovery and load table B
failRecordB <- fread(file.path(dir_data,'硬盘故障单140101_141231B.txt'),sep = '\t',encoding = 'UTF-8')
failRecordB$f_time <- as.POSIXct(failRecordB$createtime,tz = 'UTC')

#S3. deduplicate for A
source(file.path(dir_c,'dataLoadforDiskAnalysis','loadFunc.R'))
failRecordA$ip <- failRecordA$serverip
failRecordA$svr_id <- failRecordA$serverassetid
failRecordA <- dedupTime(failRecordA,3)
failRecordA$serverip <- NULL
failRecordA$createtime <- NULL
failRecordA$serversn <- NULL
failRecordA <- factorX(failRecordA)

#S3.save
save(failRecordA,failRecordB,file = file.path(dir_data,'failRecord.Rda'))
