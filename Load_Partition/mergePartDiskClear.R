#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: mergePartDiskClear.R
#
# Description: clear data in mergepartDiskdcast, merge iops of each disk to iopsw/r and generate xps = rps + wps and iops = iopsr + iopsw
# Rules of removing item:
# a. any lines with rps/wps larger than 1e6 or iopsr/iopsw(sum of iopsr_i) larger than 1e6 or util > 110
# 1. any lines with only NA and zero indicating no valid data in this line.
# 2. any svrid with 0 wps indicating a error recording(use sta_dcastClear_result.Rda)
# 3. set iopsr/iopsw/iops of a svrid to zero when any iopsw of this svrid is zero(use sta_dcastClear_result.Rda)
# 4. set util of lines to NA when xps of there lines are larger than 100.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-03-15 17:10:01
#
# Last   modified: 2017-03-15 17:10:03
#
#
#

rm(list = ls());source('~/rhead')

iops_dcast_clear <- function(dt_dcast,fn){
  num_id <- data.frame(id = c('a','b','c','d','e','f','g'),
                       num = c(0,2,3,12,13,24,24))
  fnum <- num_id$num[num_id == gsub('\\d.*','',fn)]
  dt_dcast <- dt_dcast[,c('svrid','time','rps','wps','util',
                          paste('iopsr',seq_len(fnum),sep='_'),
                          paste('iopsw',seq_len(fnum),sep='_'))]
  
  # R1. Radical Filter: method to remove all anomaly line
  idx_iopsr <- rowSums(apply(dt_dcast[grepl('iopsr',names(dt_dcast))],2,function(x)x > 1e6))
  idx_iopsw <- rowSums(apply(dt_dcast[grepl('iopsw',names(dt_dcast))],2,function(x)x > 1e6))
  
  idx_remove <- dt_dcast$rps > 1e6 | dt_dcast$wps > 1e6 | dt_dcast$util > 110 | idx_iopsr > 0 | idx_iopsw > 0 #a
  cat(sprintf('REMOVE %d[%.8f] lines in function iops_dcast_clear\tfile:%s\n',sum(idx_remove > 0),sum(idx_remove > 0)/length(idx_remove > 0),fn))
  dt_dcast <- dt_dcast[!idx_remove,]
  dt_dcast$util[dt_dcast$util > 100] <- 100
  
  # R2. set all inefficient value(-1) to NA
  dt_dcast[grepl('ps|util|iops',names(dt_dcast))][dt_dcast[grepl('ps|util|iops',names(dt_dcast))] < 0] <- NA
  
  dt_dcast
}

iops_aggragate <- function(df){
  x <- ifelse(sum(grepl('iopsr',names(df))) != 1,df$iopsr <- rowSums(df[,grepl('iopsr',names(df))]),names(df)[grepl('iopsr',names(df))] <- 'iopsr')
  x <- ifelse(sum(grepl('iopsw',names(df))) != 1,df$iopsw <- rowSums(df[,grepl('iopsw',names(df))]),names(df)[grepl('iopsw',names(df))] <- 'iopsw')
  
  df$iops <- rowSums(df[,c('iopsr','iopsw')])
  df$xps <- rowSums(df[,c('rps','wps')])
  
  df[,grepl('iops.*_.*',names(df))] <- NULL
  df
}

attr_clear <- function(dd){
  idx_r1 <- apply(dd[,-c(1,2)],1,function(x)all(is.na(x) | x == 0)) #1
  idx_r2 <- dd$svrid %in% invalid_wps$svrid #2
  dd <- dd[!(idx_r1|idx_r2),]
  
  idx_r3 <- dd$svrid %in% invalid_iopsw$svrid #3
  dd[idx_r3,grepl('iops',names(dd))] <- 0
  
  dd$util[dd$xps > 100 & dd$util == 0] <- NA #4
  
  factorX(subset(dd,!is.na(xps)))
}

clear_dcast <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t%s SATRT!!!\n',date(),fn))
  load(file.path(sDir,fn))
  if(grepl('a\\d.*',fn)){dt_dcast$iopsr_ <- 0;dt_dcast$iopsw_ <- 0}
  
  dt_dcast <- iops_dcast_clear(dt_dcast,fn)
  dt_dcast <- iops_aggragate(dt_dcast)
  dt_dcast <- attr_clear(dt_dcast)
  
  dt_dcast <- factorX(dt_dcast[,c('svrid','time','util','xps','iops','rps','iopsr','wps','iopsw')])
  save(dt_dcast,file = file.path(tDir,fn))
  cat(sprintf('[%s]\t%s END!!!\n',date(),fn))
  return(0)
}

###### MAIN ######
load('~/Data/attrid/sta_dcastClear_result.Rda')
sDir <- file.path(dir_data,'mergePartSvridDcast')
tDir <- file.path(dir_data,'mergePartSvridDcastClear')
if(!dir.exists(tDir))dir.create(tDir)
fname <- list.files(sDir)

idx <- seq_len(length(fname))
r <- foreachX(idx,'clear_dcast')

