#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: loadData.R
#
# Description: load data from disk_fault_list_140101_141231_clear and link it with the other table to merge svrid and id from ym together
# failRecordA/B is the fail record identifying server with svrid. disk_fault_list_140101-141231_clear is the one with svrnum
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-19 17:49:43
#
# Last   modified: 2016-12-19 20:17:43
#
#
rm(list = ls())
source('head.R')
load(file.path(dir_data,'failRecord2014.Rda'))


# 1. load data from disk_fault_list_140101_141231_clear
# disk_fault <- read.csv(file.path(dir_data,'files','disk_fault_list_140101_141231_clear.csv'),sep = ',')
# save(disk_fault,file = file.path(dir_data,'disk_fault_list_140101-141231_clear.Rda'))
load(file.path(dir_data,'disk_fault_list_140101-141231_clear.Rda'))

# 2. merge two table together
disk_fault_svrid <- subset(failRecordA,1==1,c('svr_id','ip','f_time','tag','faulttypename','eventdescription'))
names(disk_fault_svrid)[names(disk_fault_svrid) == 'svr_id'] <- 'svrid'
disk_fault_svrid$svrid <- factor(disk_fault_svrid$svrid)

disk_fault$hday <- NULL
names(disk_fault) <- c('sid','eventdescription','faulttypename','f_time')
disk_fault$f_time <- as.POSIXct(disk_fault$f_time,tz = 'UTC')
disk_fault$eventdescription <- gsub(',','',disk_fault$eventdescription)


disk_fault_dup <- disk_fault[!duplicated(disk_fault[,c('f_time','faulttypename','eventdescription')]),]
disk_fault_svrid_dup <- disk_fault_svrid[!duplicated(disk_fault_svrid[,c('f_time','faulttypename','eventdescription')]),]
merge_id_svrid <- merge(disk_fault_svrid_dup,disk_fault_dup,by = c('f_time','faulttypename','eventdescription'))
save(merge_id_svrid,file = file.path(dir_data,'merge_id_svrid.Rda'))
