#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-15 11:54:36
#
# Last   modified: 2017-01-18 10:21:08
#
#
#

rm(list = ls())
source('head.R')

dir_data5k <- '/home/yiyusheng/Data/tencProcess/mergePartSvrid'
fname <- list.files(dir_data5k)

require(doParallel)
idx <- seq_len(length(fname))
ck <- makeCluster(min(40,length(idx)),type = 'FORK',outfile = 'out_sta')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = T) %dopar% {
  load(file.path(dir_data5k,fname[i]))
  return(list(fn = fname[i],numItem = nrow(data),numUnit = unique(data$svr_id)))
}
stopCluster(ck)
save(r,file = file.path(dir_data,'sta.Rda'))
ni <- sum(as.numeric(sapply(r,'[[','numItem')))
nu <- sum(as.numeric(sapply(r,'[[','numUnit')))
cat(sprintf('numItems: %.0f\tnumUnits: %.0f',ni,nu))
