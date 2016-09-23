#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: restoreIO.R
#
# Description: restore data from mergePartSvrid.Rda
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-09-12 23:16:30
#
# Last   modified: 2016-09-12 23:16:34
#
#
#
rm(list = ls())
source('head.R')
####################################
sDir <- file.path(dir_data,'mergePartSvrid')
tDir <- file.path(dir_data,'restoreSvrid')
fileName <- list.files(file.path(dir_data,'mergePartSvrid'))

restore_data <- function(fn){
  load(file.path(sDir,fn))
  cat(sprintf('PROCESSING[restoreIO::restore_data]: %s\tcount:%d\n',fn,nrow(data)))
  data <- data[,c('svr_id','date','timepoint','attrid','value')]
  data$svr_id <- factor(data$svr_id)
  data$attrid <- factor(data$attrid)
  
  data$date <- as.POSIXct(as.character(data$date),tz = 'UTC',format = '%Y%m%d')
  data$date <- data$date + data$timepoint*5*60
  data$timepoint <- NULL
  names(data)[names(data) == 'date'] <- 'time'
  
  data <- factorX(data)
  svridSplit <- split(data,data$svr_id)
  tmp <- lapply(svridSplit,mergeAttr,fn = fn)
  data <- tmp
  save(data,file = file.path(tDir,fn))
  
}

mergeAttr <- function(df,fn){
  attrList <- split(df[,c('svr_id','time','value')],df$attrid)
  cat(sprintf('PROCESSING[restoreIO::mergeAttr]: file%s\tsvr_id: %s\tcount:%d\n',fn,df$svr_id[1],nrow(df)))
  mergeDf <- Reduce(function(...)merge(...,by = c('svr_id','time'),all = T),attrList)
  tryCatch(names(mergeDf)[3:ncol(mergeDf)] <- paste('a',names(attrList),sep=''),error = function(e) NULL)
  mergeDf[is.na(mergeDf)] <- 0
  dataSvrid <- mergeDf
  dataSvrid
}

# doParallel
require(doParallel)
numCore <- floor(detectCores()*0.9)
ck <- makeCluster(min(numCore,length(fileName)),outfile = '')
registerDoParallel(ck)
tmp <- foreach(i = fileName,.verbose = T) %dopar% restore_data(i)
stopCluster(ck)
