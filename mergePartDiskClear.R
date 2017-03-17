#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: mergePartDiskClear.R
#
# Description: clear data in mergepartDiskdcast and generate new data for 7 attributes util,xps,iops,rps,wps,iopsr,iopsw
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

rm(list = ls());setwd('~/Code/R/tencProcess/');source('head.R');source('../attrid/base.R')

clear_dcast <- function(i){
  fn <- fname[i]
  load(file.path(sDir,fn))
  if(grepl('a\\d.*',fn)){dt_dcast$iopsr_ <- 0;dt_dcast$iopsw_ <- 0}
  
  dt_dcast <- iops_dcast_clear(dt_dcast,fn)
  dt_dcast <- iops_aggragate(dt_dcast)
  
  dt_dcast <- factorX(dt_dcast[,c('svrid','time','util','xps','iops','rps','iopsr','wps','iopsw')])
  save(dt_dcast,file = file.path(tDir,fn))
  return(0)
}

###### MAIN ######
sDir <- file.path(dir_data,'mergePartSvridDcast')
tDir <- file.path(dir_data,'mergePartSvridDcastClear')
if(!dir.exists(tDir))dir.create(tDir)
fname <- list.files(sDir)
# fname_t <- list.files(tDir)
# fname <- setdiff(fname,fname_t)

# doParallel
require(doParallel)
idx <- seq_len(length(fname))
numCore <- floor(detectCores()*0.9)
if(file.exists('mpdc'))file.remove('mpdc')
ck <- makeCluster(min(numCore,length(fname)),outfile = 'mpdc',type = 'FORK')
registerDoParallel(ck)
foreach(i = idx,.verbose = F) %dopar% tryCatch(clear_dcast(i),error = function(e){cat(sprintf('ERROR:%s\n%s',fname[i],e))})
stopCluster(ck)
