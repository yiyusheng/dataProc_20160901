#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: reduceIOPS.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-09-13 11:23:35
#
# Last   modified: 2016-09-13 11:23:37
#
rm(list = ls())
source('head.R')
####################################
sDir <- file.path(dir_data,'restoreSvrid')
tDir <- file.path(dir_data,'reduceSvrid')
fileName <- list.files(sDir)
fileName <- fileName[!grepl('a\\d',fileName)]

reduce_data <- function(fn){
  load(file.path(sDir,fn))
  cat(sprintf('[reduceIOPS::reduce_data]\tfileName: %s\tcount:%d\n',fn,length(data)))
  tmp <- lapply(data,reduce_svrid,fn = fn)
  data <- tmp
  save(data,file = file.path(tDir,fn))
}

reduce_svrid <- function(df,fn){
  cat(sprintf('[reduceIOPS::reduce_svrid]\tfileName: %s\tsvrid:%s\tcount:%d\tlength:%d\n',
              fn,df$svr_id[1],nrow(df),ncol(df)))
  
  # filter wrong value
  lenDf <- ncol(df)
  tmp <- df[,3:lenDf]
  tmp[tmp > 1e5|tmp < 0] <- 0
  df[,3:lenDf] <-tmp
  
  lenIOPS <- (lenDf - 5)/2
  
  idxIopsRead <- 5 + seq_len(lenIOPS)
  maxIopsRead <- colSums(df[,idxIopsRead])
  idxMainRead <- which(maxIopsRead == max(maxIopsRead)) + 5
  
  idxIopsWrite <- max(idxIopsRead) + seq_len(lenIOPS)
  maxIopsWrite <- colSums(df[,idxIopsWrite])
  idxMainWrite <- which(maxIopsWrite == max(maxIopsWrite)) + max(idxIopsRead)
  
  df$a3681 <- rowSums(df[,idxIopsRead])
  df$a3682 <- df[,idxMainRead[1]]
  df$a3686 <- rowSums(df[,idxIopsWrite])
  df$a3687 <- df[,idxMainWrite[1]]
  
  df <- df[,c('svr_id','time','a902','a903','a999','a3681','a3682','a3686','a3687')]
  df
}


# doParallel
require(doParallel)
numCore <- floor(detectCores()*0.9)
ck <- makeCluster(min(numCore,length(fileName)),outfile = '')
registerDoParallel(ck)
tmp <- foreach(i = fileName,.verbose = T) %dopar% reduce_data(i)
stopCluster(ck)


