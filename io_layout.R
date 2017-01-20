#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: io_layout.R
#
# Description: sampling the normal data by bootstraping normal period of failed servers during this year. 
# 1.set a time window to capture the failed server in it
# 2.extract svrid faied in the time window
# 3.extract svrid failed far away from the time window to be the normal server.
# 4.return failed_svrid and normal_svrid
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-10 09:37:32
#
# Last   modified: 2017-01-10 09:37:34
#
#
#
rm(list = ls())
source('head.R')

load(file.path(dir_data,'diskInfo0902.Rda'))
load(file.path(dir_data,'failRecord_1407-1506.Rda'))
tw_start <- as.p('2014-07-01')
tw_end <- as.p('2014-08-01')
dist_tw <- 180

io_layout <- function(tw_start,tw_end,dist_tw,num_bs){
  # 1.extract svrid failed in time window
  failed_svrid <- factor(unique(failRecord$svr_id[failRecord$f_time > tw_start & failRecord$f_time <= tw_end]))
  
  # 2.extract svrid failed far away from the time window to be the normal server
  norm_svrid <- factor(unique(failRecord$svr_id[difftime(failRecord$f_time,tw_end,tz = 'UTC',units = 'days') > dist_tw]))
  
  # 3.expand the norm_svrid based on num_bs
  norm_svrid_expand <- factor(sample(norm_svrid,num_bs,replace = T))
  norm_svrid_unique <- unique(norm_svrid_unique)
  
  # return
  return(list(failed_svrid,norm_svrid_unique,norm_svrid_expand))
}
