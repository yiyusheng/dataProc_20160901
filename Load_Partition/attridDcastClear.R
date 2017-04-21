#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: attridDcastClear.R
#
# Description: We use IOclearFunc to clear NA, overflow value and bad relationship between attrid. we generate items for each svrid to record zeros for each iopsw
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-15 11:36:49
#
# Last   modified: 2017-04-15 11:36:50
#
#
#

rm(list = ls());setwd('/home/yiyusheng/Code/R/Load_Data_2015/');source('~/rhead')

clear_dcast15ADC <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t%s SATRT!!!\n',date(),fn))
  load(file.path(sDir,fn))
  DT <- dt_dcast
  # DT <- factorX(subset(DT,svrid %in% levels(DT$svrid)[1:10]))

  DT <- invalid_item_clear(DT,fn)
  DT <- invalid_relation_clear(DT,fn)

  DT <- factorX(DT)
  list[sta_zero,sta_sum] <- sta_zero_sum(DT,fn)
  
  save(DT,sta_zero,sta_sum,file = file.path(tDir,fn))
  cat(sprintf('[%s]\t%s END!!!\n',date(),fn))
  return(list(sta_zero,sta_sum))
}

###### MAIN ######
source('Load_Partition/IOclearFunc.R')
attrName <- attrNameExtend
sDir <- dir_data15AD
tDir <- dir_data15ADC
if(!dir.exists(tDir))dir.create(tDir)
fname <- list.files(sDir)
idx <- seq_len(length(fname))
r <- foreachX(idx,'clear_dcast15ADC')
sta_zero <- lapply(r,'[[',1);sta_sum <- lapply(r,'[[',2)
save(sta_zero,sta_sum,file = file.path(dir_data,'attridDcastClear15.Rda'))
