#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: divideFault.R
#
# Description: divide data of fault server into normal part and fault part
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-09-13 15:18:21
#
# Last   modified: 2016-09-13 15:19:09

rm(list = ls())
source('head.R')
####################################
# Read dir and file name
source(file.path(dir_c,'dataLoadforDiskAnalysis','loadFunc.R'))
sDir <- file.path(dir_data,'reduceSvrid')
tDir <- file.path(dir_data,'divideSvrid')
fileName <- list.files(sDir)
fileName <- fileName[!grepl('g',fileName)]

# Load failure record
load(file.path(dir_data,'failRecord_diskParse.Rda'))
frecord <- rbind(failRecordWithDev[,c('svr_id','f_time')],
                 failRecordWithoutDev[,c('svr_id','f_time')])

#determine range 
range_det <- function(dfx,frx){
  days <- 60
  fr <- frx[order(frx$f_time),]
  df <- dfx
  
  minT <- min(df$time);maxT <- max(df$time)
  fr <- rbind(data.frame(svr_id = '0',f_time = minT),
              fr,
              data.frame(svr_id = '0',f_time = maxT))
  
  fr$t2 <- fr$f_time - 60*86400
  fr$t1 <- fr$f_time - 90*86400
  fr$t3 <- fr$f_time + 30*86400
  lenFr <- nrow(fr)
  fr <- fr[,c('svr_id','t1','t2','f_time','t3')]
  
  # fault range
  fr$effective <- as.numeric(difftime(fr$t2,minT,units = 'days')) > -30
  rangeFau <- subset(fr,svr_id != '0' & effective == T,c('t2','f_time'))
  names(rangeFau) <- c('start','end')
  if(nrow(rangeFau) == 0)rangeFau <- data.frame()
  
  # normal range
  fr$numReserve <- 0
  fr$t1[lenFr] <- min(maxT,fr$t3[lenFr])
  tmp <- as.numeric(difftime(fr$t1[2:lenFr],fr$t3[1:(lenFr-1)],units = 'days'))/days
  fr$numReserve[1:(lenFr - 1)] <- floor(tmp)
  # fr$numReserve[lenFr - 1] <- floor(as.numeric(difftime(fr$t3[lenFr],fr$t3[lenFr - 1]),units = 'days')/days)
  
  tmp <- subset(fr,numReserve > 0)
  rangeNor <- list()
  if(nrow(tmp) > 0){
    for (i in 1:nrow(tmp)){
      tmp1 <- data.frame(start = tmp$t3[i] + 0:(tmp$numReserve[i]-1) * days * 86400)
      tmp1$end <- min(tmp1$start + days*86400,maxT)
      rangeNor[[i]] <- tmp1
    }
    rangeNor <- do.call(rbind,rangeNor)
  }else{
    rangeNor <- data.frame()
  }
  list(rangeFau,rangeNor)
}


# process for each svrid
divide_svrid <- function(df,fn){
  cat(sprintf('[divideFault::divide_svrid]\tfileName: %s\tsvrid:%s\tcount:%d\tlength:%d\n',
              fn,df$svr_id[1],nrow(df),ncol(df)))
  curSvr <- as.character(df$svr_id[1])
  fr <- subset(frecord,svr_id == curSvr)
  
  if(nrow(fr) > 1)fr <- dedupTime(fr,30,'svr_id')
  lenFr <- nrow(fr)
  
  # Divide data by time. 
  # data generate in two month before failure are used as fault data.
  # data generate three months ahead of failure or one months after failure are used as normal data.
  # duration of data longer than one month indicates an effective data for fault and normal data
  tmp <- range_det(df,fr)
  rangeFau <- tmp[[1]]
  rangeNor <- tmp[[2]]
  # list[rangeFau,rangeNor] <- range_det(df,fr)
  
  # Generate data
  dataF <- list()
  if(nrow(rangeFau) >= 1){
    for (i in 1:nrow(rangeFau)){
      tmp <- subset(df,time > rangeFau$start[i] & time <= rangeFau$end[i])
      if(nrow(tmp) == 0) {
        dataF[[i]] <- 0
        next
      }
      tmp$svr_id <- paste(tmp$svr_id,'F',i,sep='')
      dataF[[i]] <- tmp
    }
  }
  
  dataN <- list()
  if(nrow(rangeNor) >= 1){
    for (i in 1:nrow(rangeNor)){
      tmp <- subset(df,time > rangeNor$start[i] & time <= rangeNor$end[i])
      if(nrow(tmp) == 0) {
        dataN[[i]] <- 0
        next
      }
      tmp$svr_id <- paste(tmp$svr_id,'N',i,sep='')
      dataN[[i]] <- tmp
    }
  }
  
  list(dataF,dataN)
}

# process for each file
divide_data <- function(fn){
  load(file.path(sDir,fn))
  cat(sprintf('[divideFault::divide_data]\tfileName: %s\tcount:%d\n',fn,length(data)))
  tmp <- lapply(data,divide_svrid,fn = fn)
  # tmp <- divide_svrid(data[[which(names(data) == '18524')]],fn)
  data <- tmp
  save(data,file = file.path(tDir,fn))
}

# a <- divide_data('d48.Rda')
#doParallel
require(doParallel)
numCore <- floor(detectCores()*0.9)
ck <- makeCluster(min(numCore,length(fileName)),outfile = '')
registerDoParallel(ck)
tmp <- foreach(i = fileName,.verbose = T) %dopar% divide_data(i)
stopCluster(ck)

