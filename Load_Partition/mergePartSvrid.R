#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: mergePartSvrid.R
#
# Description: Merge svrid partition. Laura Ye send me on 09/02/2016
# Merge svrid partition based on file name.
# In directory partSvrid, data of a svrid is distributed in many files which is partitioned by mergePartDisk.R.
# Here, we check each file in partSvrid and merge data from the same svrid in one file.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-10 17:51:19
#
# Last   modified: 2017-03-21 17:06:19
#
#
#

#source('partSvrid.R')
rm(list = ls());source('~/rhead')

load(file.path(dir_data,'partIO.Rda'))
sDir <- file.path(dir_data,'partSvrid')
tDir <- file.path(dir_data,'mergePartSvrid')
# sDir <- file.path('/mnt/window/partSvrid')
# tDir <- file.path('/mnt/window/mergePartSvrid')

fileName <- list.files(sDir)
fileNamePre <- gsub('-.*','',fileName)
fn <- unique(fileNamePre)
fn <- fn[!grepl('d',fn)]

merge_file <- function(fname){
  fset <- sort(fileName[fname == fileNamePre])
  # create file
  dataset <- do.call(rbind,lapply(fset,function(f){
    cat(sprintf('PROCESS[mergePartSvrid]: %s\n',f))
    read.table(file.path(sDir,f),header = F)
  }))
  names(dataset) <- c('date','svr_id','attrid','timepoint','value')
  data <- subset(dataset,!is.na(value))
  cat(sprintf('SAVING: %s\n',fname))
  save(data,file = file.path(tDir,paste(fname,'.Rda',sep='')))
  cat(sprintf('SAVED: %s\n',fname))
}

# doParallel
require(doParallel)
numCore <- floor(detectCores()*0.9)
ck <- makeCluster(min(numCore,length(fn)),outfile = '')
registerDoParallel(ck)
foreach(i = fn,.verbose = T) %dopar% merge_file(i)
stopCluster(ck)
