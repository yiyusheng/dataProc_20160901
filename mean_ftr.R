#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: mean_ftr.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-09-13 20:15:59
#
# Last   modified: 2016-09-13 20:16:05
rm(list = ls())
source('head.R')
####################################
# Read dir and file name
sDir <- file.path(dir_data,'divideSvrid')
fileName <- list.files(sDir)

# process for each two months
mean_month <- function(listM){
  if(listM == 0)return(NULL)
  curSvr <- listM$svr_id[1]
  curFtime <- max(listM$time)
  count <- nrow(listM)
  cMean <- colMeans(listM[,3:ncol(listM)])
  tmp <- data.frame(svr_id = curSvr,maxTime = curFtime,count = count,t(cMean))
}

# process for each svrid
mean_svrid <- function(listS){
  dfF <- lapply(listS[[1]],mean_month)
  dfF <- do.call(rbind,dfF)
  dfN <- lapply(listS[[2]],mean_month)
  dfN <- do.call(rbind,dfN)
  list(dfF,dfN)
}

# process for each file
mean_data <- function(fn){
  load(file.path(sDir,fn))
  cat(sprintf('[mean_ftr::mean_data]\tfileName: %s\n',fn))
  tmp <- lapply(data,mean_svrid)
  tmp <- do.call(rbind,c(tmp))
}

#doParallel
# fileName <- fileName[1:2]
require(doParallel)
numCore <- floor(detectCores()*0.9)
ck <- makeCluster(min(numCore,length(fileName)),outfile = '')
registerDoParallel(ck)
mean_ftr <- foreach(i = fileName,.combine = rbind,.verbose = T) %dopar% mean_data(i)
mean_ftr <- do.call(rbind,mean_ftr)
save(mean_ftr,file = file.path(dir_data,'mean_ftr.Rda'))
stopCluster(ck)
