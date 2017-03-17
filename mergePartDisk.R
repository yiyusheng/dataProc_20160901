#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: mergePartDisk.R
#
# Description: generate disk data based on server data.
# 1.dcast server data on sid and time for all attr
# 2.generate number of bytes read/written column for each disk on the percentage of iops
# 3.divide large table by column to generate disk table.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-10 17:51:19
#
# Last   modified: 2017-01-10 17:51:21
#
#
#
rm(list = ls())
source('head.R')
load(file.path(dir_data,'failRecord_1407-1506.Rda'))
sDir <- file.path(dir_data,'mergePartSvrid')
tDir <- file.path(dir_data,'mergePartSvridDcast')
fname <- list.files(sDir)
fname_t <- list.files(tDir)
fname <- setdiff(fname,fname_t)
# fname <- fname[!grepl('a[0-9]',fname)]

extract_io_disk <- function(fn){
  load(file.path(sDir,fn))
  dt <- data
  dt$time <- as.POSIXct(as.character(dt$date),tz = 'UTC',format = '%Y%m%d') + dt$timepoint*300
  
  # step1:dcast
  cat(sprintf('DCAST: %s\n',fn))
  dt_dcast <- dcast(dt,svr_id + time ~ attrid,value.var = 'value',fun.aggregate = mean,na.rm = T)
  dt_dcast[is.na(dt_dcast)] <- 0
  dim2 <- dim(dt_dcast)[2]
  len_iops <- dim2 - 5
  if(len_iops > 0){
    col_iopsr <- paste('iopsr',1:(len_iops/2),sep='_')
    col_iopsw <- paste('iopsw',1:(len_iops/2),sep='_')
    names(dt_dcast) <- c('svrid','time','rps','wps','util',col_iopsr,col_iopsw)
    dt_dcast <- subset(dt_dcast,1==1,c('svrid','time','rps','wps','util',col_iopsr,col_iopsw))
  }else{
    names(dt_dcast) <- c('svrid','time','rps','wps','util')
  }
  dt_dcast$svrid <- factor(dt_dcast$svrid)
  dt_dcast <- factorX(dt_dcast)

  # step2:save
  cat(sprintf('SAVING: %s\n',fn))
  save(dt_dcast,file = file.path(tDir,fn))
  return(0)
}

# doParallel
require(doParallel)
idx <- seq_len(length(fname))
numCore <- floor(detectCores()*0.9)
ck <- makeCluster(min(numCore,length(fname)),outfile = 'mpd',type = 'FORK')
registerDoParallel(ck)
foreach(i = idx,.verbose = F) %dopar% extract_io_disk(fname[i])
stopCluster(ck)






# dt_dcast[dt_dcast < 0] <- 0
# dt_dcast$util[dt_dcast$util > 100] <- 100


# step2:generate rps and wps for each disk based on percentage of iops
# m_rps <- t(apply(dt_dcast[,col_iopsr],1,function(x)x/sum(x))) * dt_dcast$rps
# m_wps <- t(apply(dt_dcast[,col_iopsw],1,function(x)x/sum(x))) * dt_dcast$wps
# m_rps[is.na(m_rps)] <- 0
# m_wps[is.na(m_wps)] <- 0

# step3:divide
# cat(sprintf('DIVIDE: %s\n',fn))
# dt_disk <- lapply(1:(len_iops/2),function(i){
#   an <- paste(c('iopsr','iopsw'),i,sep='_')
#   tmp <- cbind(dt_dcast[,c('svrid','time','util',an)],m_rps[,an[1]],m_wps[,an[2]])
#   tmp$diskID <- i
#   names(tmp) <- c('svrid','time','util','iops_r','iops_w','nbps_r','nbps_w','diskID')
#   # tmp[is.na(tmp)] <- 0
#   return(tmp)
# })
# dt_disk <- do.call(rbind,dt_disk)
# dt_disk[is.na(dt_disk)] <- 0
# dt_disk$svrid <- factor(dt_disk$svrid)
# dt_disk$diskID <- factor(dt_disk$diskID)

# sapply(seq_len(length(fname)),function(i){
#   system.time(extract_io_disk(fname[i]))
# })

# step4:
# check
# a <- apply(dt_dcast[,c(col_iopsr,col_iopsw)],1,function(x)any(x > 1e7))
# b <- dt_dcast[a,]
# f <- subset(failRecord,svr_id %in% b$svrid)